# app.R
# NYPD Hate Crimes — Game UI (Kahoot style) + Hidden Scoring + Auto-advance
#
# Improvements:
# 1) Ensure chart is fully visible (bigger plot area + responsive)
# 2) Remove "Next question" button
# 3) Remove "Submit" button
# 4) Clicking an answer auto-logs + auto-advances after 0.5s
# 5) Colored answer buttons

library(shiny)
library(readxl)
library(dplyr)
library(stringr)
library(plotly)
library(tibble)
library(tidyr)
library(writexl)

xlsx_path <- "NYPD_Hate_Crimes_20260128.xlsx"

# ---------- Load data ----------
raw <- read_excel(xlsx_path, col_types = "text")
names(raw) <- str_trim(names(raw))

COL_YEAR   <- "Complaint Year Number"
COL_MONTH  <- "Month Number"
COL_OFFCAT <- "Offense Category"
COL_BIAS   <- "Bias Motive Description"

needed <- c(COL_YEAR, COL_MONTH, COL_OFFCAT, COL_BIAS)
missing <- setdiff(needed, names(raw))
if (length(missing) > 0) stop("Missing columns: ", paste(missing, collapse = ", "))

df <- raw %>%
  transmute(
    year  = suppressWarnings(as.integer(.data[[COL_YEAR]])),
    month = suppressWarnings(as.integer(.data[[COL_MONTH]])),
    offense_category = ifelse(is.na(.data[[COL_OFFCAT]]) | .data[[COL_OFFCAT]] == "", "Unknown", .data[[COL_OFFCAT]]),
    bias_motive      = ifelse(is.na(.data[[COL_BIAS]])   | .data[[COL_BIAS]]   == "", "Unknown", .data[[COL_BIAS]])
  ) %>%
  filter(!is.na(year), !is.na(month))

years <- sort(unique(df$year))

# ---------- Helpers ----------
cat_counts_for_year <- function(data, yr) {
  data %>%
    filter(year == yr) %>%
    count(offense_category, name = "n") %>%
    arrange(desc(n))
}

pick_year_for_pid <- function(pid, years_vec) {
  years_vec <- sort(unique(years_vec))
  if (length(years_vec) == 0) return(NA_integer_)
  pid <- toupper(str_trim(as.character(pid)))
  if (is.na(pid) || pid == "") pid <- "P00"
  s <- sum(utf8ToInt(pid))
  idx <- (s %% length(years_vec)) + 1
  years_vec[idx]
}

make_task_pool <- function(one_year, n_tasks = 12, seed = 490) {
  set.seed(seed)
  tibble(
    task_id = seq_len(n_tasks),
    year = rep(as.integer(one_year), n_tasks),
    task_type = sample(
      c("MAX", "MIN", "SECOND"),
      size = n_tasks, replace = TRUE,
      prob = c(0.4, 0.4, 0.2)
    )
  )
}

make_choices <- function(counts, correct_label, k = 4, seed = 490) {
  labels <- unique(as.character(counts$offense_category))
  labels <- labels[!is.na(labels) & labels != ""]
  if (length(labels) == 0) return(character(0))

  if (!(correct_label %in% labels)) correct_label <- labels[1]
  set.seed(seed + nchar(correct_label))

  others <- setdiff(labels, correct_label)
  if (length(others) == 0) return(correct_label)

  n_distractors <- min(k - 1, length(others))
  distractors <- sample(others, n_distractors, replace = FALSE)
  choices <- unique(c(correct_label, distractors))

  while (length(choices) < min(k, length(labels))) {
    remaining <- setdiff(labels, choices)
    if (length(remaining) == 0) break
    choices <- c(choices, sample(remaining, 1))
  }

  if (length(choices) > 1) choices <- sample(choices, length(choices))
  choices
}

get_correct_answer <- function(counts, task_type) {
  if (nrow(counts) == 0) return(NA_character_)
  if (task_type == "MAX") return(counts$offense_category[which.max(counts$n)][1])
  if (task_type == "MIN") return(counts$offense_category[which.min(counts$n)][1])
  if (nrow(counts) < 2) return(counts$offense_category[1])
  counts$offense_category[2]
}

task_text <- function(task_type) {
  if (task_type == "MAX") return("highest")
  if (task_type == "MIN") return("lowest")
  "second-highest"
}

pretty_chart_name <- function(x) {
  switch(
    x,
    "treemap" = "Tree Map",
    "bar"     = "Bar Chart",
    "stack"   = "Stack Chart",
    "line"    = "Line Graph",
    "Chart"
  )
}

# ---------- Plot builder ----------
plot_for_year <- function(yr, chart_type) {
  x <- df %>% filter(year == yr)
  if (nrow(x) == 0) return(plot_ly() %>% layout(title = "No data for this year"))

  counts <- x %>% count(offense_category, name = "n") %>% arrange(desc(n))
  ord <- counts$offense_category
  counts <- counts %>% mutate(offense_category = factor(offense_category, levels = ord))

  # tighter margins so plot fits better
  base_layout <- list(
    margin = list(t = 10, l = 55, r = 20, b = 110),
    xaxis = list(title = "Offense Category", tickangle = -35, automargin = TRUE),
    yaxis = list(title = "Incidents", automargin = TRUE)
  )

  if (chart_type == "bar") {
    return(
      plot_ly(
        counts, x = ~offense_category, y = ~n,
        type = "bar",
        hovertemplate = "<b>%{x}</b><br>Incidents: %{y}<extra></extra>"
      ) %>% layout(base_layout)
    )
  }

  if (chart_type == "treemap") {
    nodes <- tibble(
      ids = as.character(counts$offense_category),
      labels = as.character(counts$offense_category),
      parents = "",
      values = counts$n
    )
    return(
      plot_ly(
        nodes, type = "treemap",
        ids = ~ids, labels = ~labels, parents = ~parents, values = ~values,
        textinfo = "label+value",
        hovertemplate = "<b>%{label}</b><br>Incidents: %{value}<extra></extra>"
      ) %>%
        layout(margin = list(t = 10, l = 0, r = 0, b = 0))
    )
  }

  if (chart_type == "stack") {
    by_month <- x %>%
      count(offense_category, month, name = "n") %>%
      complete(offense_category = unique(x$offense_category), month = 1:12, fill = list(n = 0)) %>%
      mutate(
        offense_category = factor(offense_category, levels = ord),
        month = factor(month, levels = 1:12, labels = month.abb)
      ) %>%
      arrange(offense_category, month)

    return(
      plot_ly(
        by_month,
        x = ~offense_category, y = ~n,
        color = ~month,
        type = "bar",
        hovertemplate = "<b>%{x}</b><br>Month: %{fullData.name}<br>Incidents: %{y}<extra></extra>"
      ) %>%
        layout(
          barmode = "stack",
          margin = list(t = 10, l = 55, r = 20, b = 110),
          xaxis = list(title = "Offense Category", tickangle = -35, automargin = TRUE),
          yaxis = list(title = "Incidents", automargin = TRUE),
          legend = list(title = list(text = "<b>Month</b>"))
        )
    )
  }

  plot_ly(
    counts,
    x = ~offense_category, y = ~n,
    type = "scatter",
    mode = "lines+markers",
    hovertemplate = "<b>%{x}</b><br>Incidents: %{y}<extra></extra>"
  ) %>%
    layout(base_layout)
}

# ---------- UI ----------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background: #ffffff; }
      .app-wrap { max-width: 1150px; margin: 0 auto; padding: 10px; }
      .big-title { font-size: 30px; font-weight: 800; margin: 10px 0 0 0; }
      .subtle { color: #666; }
      .card { border: 1px solid #e5e5e5; border-radius: 14px; padding: 16px; background: #fafafa; }
      .btn-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 12px; }
      .bigbtn button { width: 100%; padding: 18px 14px; font-size: 18px; border-radius: 14px; }
      .quiz-layout { display: flex; flex-direction: column; gap: 12px; }
      .plot-box { border: 1px solid #eee; border-radius: 14px; padding: 8px; background: #fff; }
      .plot-height { height: 68vh; }
      .plot-height .plotly { height: 100% !important; }
      .question-box { border: 1px solid #e5e5e5; border-radius: 14px; padding: 14px; background: #fafafa; }
      .question-text { font-size: 18px; font-weight: 750; margin-bottom: 10px; }
      .countdown { font-size: 56px; font-weight: 800; text-align: center; padding: 20px; }
      .topbar { display:flex; gap: 10px; align-items:center; justify-content: space-between; margin-bottom: 10px;}
      .smallbtn button { border-radius: 12px; }

      /* Answer buttons (Kahoot-ish) */
      .ans-grid { display:grid; grid-template-columns: 1fr 1fr; gap: 12px; }
      .ansbtn button { width:100%; padding:18px 14px; font-size:16px; font-weight:700; border-radius:14px; border: 0px; }
      .ans1 button { background:#2D7FF9; color:white; }
      .ans2 button { background:#F04A4A; color:white; }
      .ans3 button { background:#2FBF71; color:white; }
      .ans4 button { background:#F5A623; color:white; }

      /* Make long category names wrap */
      .ansbtn button { white-space: normal; line-height: 1.2; }
    "))
  ),

  div(class = "app-wrap",
      div(class = "topbar",
          div(
            div(class = "big-title", "NYPD Hate Crimes — Visualization Speed Test"),
            div(class = "subtle", "Pick a chart, read the instructions, then answer quick questions.")
          ),
          div(class = "card",
              textInput("pid", "Participant ID", value = "P01"),
              downloadButton("download_log_xlsx", "Researcher: Download Results (Excel)")
          )
      ),
      uiOutput("screen_ui")
  ),

  # JS: countdown + auto-next trigger
  tags$script(HTML("
    Shiny.addCustomMessageHandler('start_countdown', function(message) {
      var el = document.getElementById('countdown_text');
      if (!el) return;

      var seq = ['3','2','1','START'];
      var i = 0;

      function tick() {
        el.textContent = seq[i];
        i++;
        if (i < seq.length) {
          setTimeout(tick, 900);
        } else {
          setTimeout(function() {
            Shiny.setInputValue('countdown_done', Date.now(), {priority: 'event'});
          }, 600);
        }
      }
      tick();
    });

    // After an answer is clicked, server asks browser to wait 500ms then trigger next question.
    Shiny.addCustomMessageHandler('auto_next', function(message) {
      setTimeout(function(){
        Shiny.setInputValue('auto_next_trigger', Date.now(), {priority: 'event'});
      }, 500);
    });
  "))
)

# ---------- Server ----------
server <- function(input, output, session) {

  rv <- reactiveValues(
    screen = "landing",         # landing -> instructions -> countdown -> quiz
    chart = NA_character_,
    tasks = NULL,
    i = 0,
    story_year = NA_integer_,
    story_type = NA_character_,
    correct = NA_character_,
    choices = NULL,
    prompt = "",
    start_time = NULL,
    participant_year = NA_integer_,
    log = tibble(
      participant = character(),
      chart = character(),
      task_id = integer(),
      year = integer(),
      task_type = character(),
      correct_answer = character(),
      submitted = character(),
      correct = logical(),
      seconds = numeric()
    )
  )

  # researcher download
  output$download_log_xlsx <- downloadHandler(
    filename = function() paste0("nypd_storymode_results_", Sys.Date(), ".xlsx"),
    content = function(file) {
      writexl::write_xlsx(list(results = rv$log), path = file)
    }
  )

  # ----- Screen UI -----
  output$screen_ui <- renderUI({
    if (rv$screen == "landing") {
      return(
        div(class = "card",
            h4("Choose a chart to start"),
            div(class = "btn-grid",
                div(class = "bigbtn", actionButton("pick_treemap", "Tree Map")),
                div(class = "bigbtn", actionButton("pick_bar", "Bar Chart")),
                div(class = "bigbtn", actionButton("pick_stack", "Stack Chart")),
                div(class = "bigbtn", actionButton("pick_line", "Line Graph"))
            ),
            hr(),
            div(class = "subtle",
                "You will get one year for your session. Every chart + question uses the same year for you."
            )
        )
      )
    }

    if (rv$screen == "instructions") {
      return(
        div(class = "card",
            h4(paste0("Instructions — ", pretty_chart_name(rv$chart))),
            tags$ul(
              tags$li("You will answer quick questions about the graph/chart."),
              tags$li("Use graph/chart to see exact incident counts."),
              tags$li("No filters. No changing the year."),
              tags$li("Click an answer — it will automatically go to the next question.")
            ),
            hr(),
            div(class = "smallbtn",
                actionButton("go_countdown", "I understand — Start")
            )
        )
      )
    }

    if (rv$screen == "countdown") {
      return(
        div(class = "card",
            div(id = "countdown_text", class = "countdown", "")
        )
      )
    }

    # quiz screen: graph top, question bottom
    div(class = "quiz-layout",
        div(class = "plot-box plot-height",
            plotlyOutput("quiz_plot", height = "100%")
        ),
        div(class = "question-box",
            div(class = "question-text", textOutput("task_prompt")),
            uiOutput("answer_ui"),
            div(style = "display:flex; gap:10px; margin-top:10px;",
                actionButton("back_home", "Quit / Home")
            )
        )
    )
  })

  # ----- Chart picks -----
  observeEvent(input$pick_treemap, { rv$chart <- "treemap"; rv$screen <- "instructions" })
  observeEvent(input$pick_bar,     { rv$chart <- "bar";     rv$screen <- "instructions" })
  observeEvent(input$pick_stack,   { rv$chart <- "stack";   rv$screen <- "instructions" })
  observeEvent(input$pick_line,    { rv$chart <- "line";    rv$screen <- "instructions" })

  # ----- Start / restart for participant -----
  reset_for_participant <- function() {
    py <- pick_year_for_pid(input$pid, years)
    rv$participant_year <- py

    rv$tasks <- make_task_pool(py, n_tasks = 12, seed = 490)
    rv$i <- 0
    rv$story_year <- NA_integer_
    rv$story_type <- NA_character_
    rv$correct <- NA_character_
    rv$choices <- NULL
    rv$prompt <- ""
    rv$start_time <- NULL
  }

  observeEvent(input$go_countdown, {
    reset_for_participant()
    rv$screen <- "countdown"

    session$onFlushed(function() {
      session$sendCustomMessage("start_countdown", list())
    }, once = TRUE)
  })

  observeEvent(input$countdown_done, {
    rv$screen <- "quiz"
    isolate({ rv$i <- 0 })

    session$onFlushed(function() {
      # load first question immediately after quiz UI exists
      session$sendCustomMessage('auto_next', list(message = "first"))
    }, once = TRUE)
  })

  # ----- Load next question (server-side) -----
  load_next_question <- function() {
    req(!is.null(rv$tasks))
    req(!is.na(rv$participant_year))

    rv$i <- rv$i + 1
    if (rv$i > nrow(rv$tasks)) rv$i <- 1

    task <- rv$tasks[rv$i, ]
    yr <- as.integer(task$year)     # fixed per participant
    ttype <- as.character(task$task_type)

    counts <- cat_counts_for_year(df, yr) %>%
      filter(!is.na(offense_category) & offense_category != "")

    if (nrow(counts) < 2) {
      rv$prompt <- paste("Not enough categories in year", yr, "— restarting.")
      rv$choices <- NULL
      rv$start_time <- NULL
      return()
    }

    correct_label <- get_correct_answer(counts, ttype)
    choices <- make_choices(counts, correct_label, k = 4, seed = 490 + rv$i)

    if (length(choices) < 2) {
      rv$prompt <- "Could not build choices — restarting."
      rv$choices <- NULL
      rv$start_time <- NULL
      return()
    }

    rv$story_year <- yr
    rv$story_type <- ttype
    rv$correct <- correct_label
    rv$choices <- choices
    rv$start_time <- Sys.time()

    rv$prompt <- paste0(
      "In year ", yr, ", which offense category has the ", task_text(ttype), " number of incidents?"
    )
  }

  # Triggered by JS after 0.5s (or first load)
  observeEvent(input$auto_next_trigger, {
    load_next_question()
  }, ignoreInit = TRUE)

  output$task_prompt <- renderText(rv$prompt)

  # ----- Answer UI: 4 colored buttons -----
  output$answer_ui <- renderUI({
    if (is.null(rv$choices) || length(rv$choices) < 2) {
      return(div(class = "subtle", "Loading question..."))
    }

    # ensure exactly 4 buttons if possible (if fewer, show what exists)
    ch <- rv$choices
    btns <- lapply(seq_along(ch), function(i) {
      cls <- paste("ansbtn", paste0("ans", i))
      actionButton(
        inputId = paste0("ans_", i),
        label = ch[i],
        class = cls
      )
    })

    div(class = "ans-grid", btns)
  })

  # ----- Handle answer clicks: log + auto-advance -----
  handle_answer <- function(i) {
    if (is.null(rv$choices) || length(rv$choices) < i) return()
    req(rv$i > 0, !is.null(rv$start_time), !is.na(rv$story_year), !is.na(rv$story_type))

    submitted <- rv$choices[i]
    if (is.null(submitted) || submitted == "") return()

    secs <- as.numeric(difftime(Sys.time(), rv$start_time, units = "secs"))
    is_correct <- identical(submitted, rv$correct)

    rv$log <- bind_rows(rv$log, tibble(
      participant = str_trim(input$pid),
      chart = rv$chart,
      task_id = rv$i,
      year = rv$story_year,
      task_type = rv$story_type,
      correct_answer = rv$correct,
      submitted = submitted,
      correct = is_correct,
      seconds = secs
    ))

    # prevent multiple answers for same question
    rv$start_time <- NULL

    # ask browser to wait 0.5s then trigger next question
    session$sendCustomMessage('auto_next', list())
  }

  observeEvent(input$ans_1, { handle_answer(1) }, ignoreInit = TRUE)
  observeEvent(input$ans_2, { handle_answer(2) }, ignoreInit = TRUE)
  observeEvent(input$ans_3, { handle_answer(3) }, ignoreInit = TRUE)
  observeEvent(input$ans_4, { handle_answer(4) }, ignoreInit = TRUE)

  # Quit/Home
  observeEvent(input$back_home, {
    rv$screen <- "landing"
    rv$choices <- NULL
    rv$prompt <- ""
    rv$start_time <- NULL
  })

  # ----- Quiz plot -----
  output$quiz_plot <- renderPlotly({
    req(!is.na(rv$participant_year))
    yr <- if (!is.na(rv$story_year)) rv$story_year else rv$participant_year
    req(!is.na(rv$chart))

    p <- plot_for_year(yr, rv$chart)
    p %>% layout(
      title = list(text = paste0(pretty_chart_name(rv$chart), " (Year ", yr, ")"))
    ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })
}

shinyApp(ui, server)