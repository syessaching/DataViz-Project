# app.R
# NYPD Hate Crimes — Full revision (full 16-trial design)
# - 2 timed performance (MAX, MIN) per chart (scored)
# - 2 perception (clarity, confidence) per chart (untimed)
# - 4 charts => 16 trials, then preference (single choice)
# - Same preset year per participant (deterministic from PID)
# - Per-question timeout (later::later), auto-advance after answer (0.5s)
# - Single-click answer protection, hidden scoring for participants
# - Researcher download includes time + correctness + timeout flag

library(shiny)
library(readxl)
library(dplyr)
library(stringr)
library(plotly)
library(tibble)
library(tidyr)
library(writexl)
library(later)

# ---------------------------
# Config
# ---------------------------
xlsx_path <- "NYPD_Hate_Crimes_20260128.xlsx"
PERFORMANCE_TIMEOUT <- 15     # seconds for timed (performance) trials
AUTO_ADVANCE_MS <- 500        # ms to wait after answer before next trial (client-side)

# ---------------------------
# Load data
# ---------------------------
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

# ---------------------------
# Helpers
# ---------------------------
cat_counts_for_year <- function(data, yr) {
  data %>% filter(year == yr) %>% count(offense_category, name = "n") %>% arrange(desc(n))
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
  # fallback: unknown task type -> return NA (fail loudly)
  return(NA_character_)
}

task_text <- function(task_type) {
  if (task_type == "MAX") return("highest")
  if (task_type == "MIN") return("lowest")
  "second-highest"
}

pretty_chart_name <- function(x) {
  switch(x,
         "treemap" = "Tree Map",
         "bar"     = "Bar Chart",
         "stack"   = "Stack Chart",
         "line"    = "Line Graph",
         "Chart")
}

# Full plan: per chart (random order per participant) => performance MAX, MIN then perception Clarity, Confidence
make_full_plan <- function(one_year, seed = 490) {
  charts <- c("treemap", "bar", "stack", "line")
  set.seed(seed)
  charts_order <- sample(charts, length(charts))
  rows <- list(); idx <- 1
  for (ch in charts_order) {
    rows[[idx]] <- tibble(trial_idx = idx, phase = "performance", chart = ch, year = as.integer(one_year), task_type = "MAX", perc_q = NA_integer_); idx <- idx + 1
    rows[[idx]] <- tibble(trial_idx = idx, phase = "performance", chart = ch, year = as.integer(one_year), task_type = "MIN", perc_q = NA_integer_); idx <- idx + 1
    rows[[idx]] <- tibble(trial_idx = idx, phase = "perception", chart = ch, year = as.integer(one_year), task_type = NA_character_, perc_q = 1L); idx <- idx + 1
    rows[[idx]] <- tibble(trial_idx = idx, phase = "perception", chart = ch, year = as.integer(one_year), task_type = NA_character_, perc_q = 2L); idx <- idx + 1
  }
  bind_rows(rows)
}

# Plot builder
plot_for_year <- function(yr, chart_type, data_df) {
  x <- data_df %>% filter(year == yr)
  if (nrow(x) == 0) return(plot_ly() %>% layout(title = "No data for this year"))
  counts <- x %>% count(offense_category, name = "n") %>% arrange(desc(n))
  ord <- counts$offense_category
  counts <- counts %>% mutate(offense_category = factor(offense_category, levels = ord))
  base_layout <- list(
    margin = list(t = 10, l = 55, r = 20, b = 110),
    xaxis = list(title = "Offense Category", tickangle = -35, automargin = TRUE),
    yaxis = list(title = "Incidents", automargin = TRUE)
  )
  if (chart_type == "bar") {
    plot_ly(counts, x = ~offense_category, y = ~n, type = "bar",
            hovertemplate = "<b>%{x}</b><br>Incidents: %{y}<extra></extra>") %>% layout(base_layout)
  } else if (chart_type == "treemap") {
    nodes <- tibble(ids = as.character(counts$offense_category),
                    labels = as.character(counts$offense_category),
                    parents = "", values = counts$n)
    plot_ly(nodes, type = "treemap", ids = ~ids, labels = ~labels, parents = ~parents, values = ~values,
            textinfo = "label+value", hovertemplate = "<b>%{label}</b><br>Incidents: %{value}<extra></extra>") %>%
      layout(margin = list(t = 10, l = 0, r = 0, b = 0))
  } else if (chart_type == "stack") {
    by_month <- x %>% count(offense_category, month, name = "n") %>%
      complete(offense_category = unique(x$offense_category), month = 1:12, fill = list(n = 0)) %>%
      mutate(offense_category = factor(offense_category, levels = ord),
             month = factor(month, levels = 1:12, labels = month.abb)) %>%
      arrange(offense_category, month)
    plot_ly(by_month, x = ~offense_category, y = ~n, color = ~month, type = "bar",
            hovertemplate = "<b>%{x}</b><br>Month: %{fullData.name}<br>Incidents: %{y}<extra></extra>") %>%
      layout(barmode = "stack",
             margin = list(t = 10, l = 55, r = 20, b = 110),
             xaxis = list(title = "Offense Category", tickangle = -35, automargin = TRUE),
             yaxis = list(title = "Incidents", automargin = TRUE),
             legend = list(title = list(text = "<b>Month</b>")))
  } else {
    plot_ly(counts, x = ~offense_category, y = ~n, type = "scatter", mode = "lines+markers",
            hovertemplate = "<b>%{x}</b><br>Incidents: %{y}<extra></extra>") %>% layout(base_layout)
  }
}

# ---------------------------
# UI
# ---------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background: #ffffff; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial; }
      .app-wrap { max-width: 1150px; margin: 0 auto; padding: 10px; }
      .big-title { font-size: 28px; font-weight: 700; margin: 6px 0; }
      .subtle { color: #666; }
      .card { border: 1px solid #e5e5e5; border-radius: 10px; padding: 14px; background: #fafafa; }
      .plot-box { border: 1px solid #eee; border-radius: 10px; padding: 6px; background: #fff; }
      .plot-height { height: 66vh; }
      .plot-height .plotly { height: 100% !important; }
      .question-box { border: 1px solid #e5e5e5; border-radius: 10px; padding: 12px; background: #fafafa; }
      .question-text { font-size: 17px; font-weight: 700; margin-bottom: 8px; }
      .countdown { font-size: 56px; font-weight: 800; text-align: center; padding: 20px; }
      .btn-grid { display:grid; grid-template-columns: repeat(2, 1fr); gap:10px; }
      .bigbtn button { width:100%; padding:14px; font-size:16px; border-radius:10px; color:white; font-weight:800; border: 0; }
      .pick1 button { background:#6C5CE7; } .pick2 button { background:#0984E3; } .pick3 button { background:#00B894; } .pick4 button { background:#D63031; }
      .ans-grid { display:grid; grid-template-columns: 1fr 1fr; gap: 10px; margin-top:8px;}
      .ansbtn button { width:100%; padding:14px; font-size:15px; font-weight:700; border-radius:10px; border:0; white-space: normal; line-height:1.2;}
      .ans1 button { background:#2D7FF9; color:white; } .ans2 button { background:#F04A4A; color:white; } .ans3 button { background:#2FBF71; color:white; } .ans4 button { background:#F5A623; color:white; }
      .ansbtn button:disabled { opacity: 0.6; filter: grayscale(0.1); }
      .likert-grid { display:grid; grid-template-columns: repeat(5,1fr); gap:8px; margin-top:8px; }
      .likert-grid button { padding:12px; font-weight:800; border-radius:8px; border:0; background:#edf2f7; }
      .pref-grid { display:grid; grid-template-columns: repeat(2,1fr); gap:10px; margin-top:8px; }
      .pref-grid button { padding:12px; font-weight:800; border-radius:8px; border:0; color:white; }
      .p1 { background:#6C5CE7; } .p2 { background:#0984E3; } .p3 { background:#00B894; } .p4 { background:#D63031; }
      .topbar { display:flex; gap: 10px; align-items:center; justify-content: space-between; margin-bottom: 12px;}
    "))
  ),

  div(class = "app-wrap",
      div(class = "topbar",
          div(
            div(class = "big-title", "NYPD Hate Crimes — Visualization Speed Test"),
            div(class = "subtle", "Per chart: 2 timed performance questions then 2 perception questions.")
          ),
          div(class = "card",
              textInput("pid", "Participant ID", value = "P01"),
              downloadButton("download_log_xlsx", "Researcher: Download Results (Excel)")
          )
      ),

      uiOutput("screen_ui")
  ),

  # JS handlers: countdown + small auto-next handler
  tags$script(HTML(sprintf("
    Shiny.addCustomMessageHandler('start_countdown', function(message) {
      var el = document.getElementById('countdown_text'); if (!el) return;
      var seq = ['3','2','1','START']; var i = 0;
      function tick(){ el.textContent = seq[i]; i++; if (i < seq.length) { setTimeout(tick, 900); } else { setTimeout(function(){ Shiny.setInputValue('countdown_done', Date.now(), {priority:'event'}); }, 600); } }
      tick();
    });

    Shiny.addCustomMessageHandler('auto_next', function(message) {
      setTimeout(function(){ Shiny.setInputValue('auto_next_trigger', Date.now(), {priority:'event'}); }, %d);
    });
  ", AUTO_ADVANCE_MS)))
)

# ---------------------------
# Server
# ---------------------------
server <- function(input, output, session) {

  rv <- reactiveValues(
    screen = "instructions",    # instructions -> countdown -> quiz -> preference -> done
    participant_year = NA_integer_,
    plan = NULL,
    idx = 0,
    chart = NA_character_,
    story_year = NA_integer_,
    task_type = NA_character_,
    choices = NULL,
    prompt = "",
    start_time = NULL,         # actual Sys.time() when performance trial presented
    answered = FALSE,          # to prevent double-logging
    log = tibble(
      participant = character(),
      trial_kind = character(),     # performance / perception / preference
      chart = character(),
      year = integer(),
      trial_idx = integer(),
      task_type = character(),
      correct_answer = character(),
      submitted = character(),
      correct = logical(),
      seconds = numeric(),
      timeout = logical(),          # TRUE if auto-timed out
      perc_q = integer(),
      survey_question = character()
    )
  )

  # Researcher download
  output$download_log_xlsx <- downloadHandler(
    filename = function() paste0("nypd_results_", Sys.Date(), ".xlsx"),
    content = function(file) {
      writexl::write_xlsx(list(results = rv$log), path = file)
    }
  )

  # Screen UI
  output$screen_ui <- renderUI({
    if (rv$screen == "instructions") {
      div(class = "card",
          h4("Instructions"),
          tags$ul(
            tags$li("For each visualization you'll answer two timed performance questions (highest, lowest)."),
            tags$li("After those, you'll answer two perception questions: clarity (1–5) and confidence (1–5)."),
            tags$li("Clicking an answer for performance questions logs time & correctness and auto-advances."),
            tags$li("Perception questions are untimed; click a rating to continue."),
            tags$li(sprintf("Timed trials have a %d second limit.", PERFORMANCE_TIMEOUT))
          ),
          hr(),
          div(class="btn-grid",
              div(class="bigbtn pick1", actionButton("begin", "Begin")),
              div(class="subtle", "Press Begin to start. Make sure Participant ID is set.")
          )
      )
    } else if (rv$screen == "countdown") {
      div(class = "card", div(id = "countdown_text", class = "countdown", ""))
    } else if (rv$screen == "quiz") {
      div(class = "quiz-layout",
          div(class = "plot-box plot-height", plotlyOutput("quiz_plot", height = "100%")),
          div(class = "question-box",
              div(class = "question-text", textOutput("task_prompt")),
              uiOutput("answer_ui"),
              div(style = "margin-top:8px;", actionButton("back_home", "Quit / Home"))
          )
      )
    } else if (rv$screen == "preference") {
      div(class = "card",
          h3("Preference (final)"),
          p("Which visualization did you prefer overall?"),
          div(class = "pref-grid",
              actionButton("pref_treemap", "Tree Map", class = "p1"),
              actionButton("pref_bar", "Bar Chart", class = "p2"),
              actionButton("pref_stack", "Stack Chart", class = "p3"),
              actionButton("pref_line", "Line Graph", class = "p4")
          )
      )
    } else {
      div(class = "card", h3("Done — thank you!"), p("You can close this tab."))
    }
  })

  #### Start flow: build plan and countdown ####
  observeEvent(input$begin, {
    # set participant year and plan deterministically from PID
    py <- pick_year_for_pid(input$pid, years)
    rv$participant_year <- py
    seed_val <- 490 + sum(utf8ToInt(str_trim(input$pid)))
    rv$plan <- make_full_plan(py, seed = seed_val)
    rv$idx <- 0
    rv$chart <- NA_character_; rv$story_year <- NA_integer_; rv$task_type <- NA_character_
    rv$choices <- NULL; rv$prompt <- ""; rv$start_time <- NULL; rv$answered <- FALSE
    rv$screen <- "countdown"
    session$onFlushed(function() session$sendCustomMessage("start_countdown", list()), once = TRUE)
  })

  observeEvent(input$countdown_done, {
    rv$screen <- "quiz"
    rv$idx <- 0
    # trigger first trial after UI is ready
    session$onFlushed(function() session$sendCustomMessage("auto_next", list()), once = TRUE)
  })

  #### Load next trial (called when client triggers auto_next) ####
  load_next_trial <- function() {
    req(!is.null(rv$plan), !is.na(rv$participant_year))
    rv$idx <- rv$idx + 1

    # completed all trials -> preference screen
    if (rv$idx > nrow(rv$plan)) {
      rv$screen <- "preference"
      return()
    }

    row <- rv$plan[rv$idx, ]
    phase <- row$phase
    rv$chart <- as.character(row$chart)
    rv$story_year <- as.integer(row$year)
    rv$choices <- NULL
    rv$start_time <- NULL
    rv$answered <- FALSE

    if (phase == "performance") {
      # build choices and schedule a timeout (later)
      ttype <- as.character(row$task_type)
      counts <- cat_counts_for_year(df, rv$story_year) %>% filter(!is.na(offense_category) & offense_category != "")
      if (nrow(counts) < 2) {
        # log skip and go next
        rv$log <- bind_rows(rv$log, tibble(
          participant = str_trim(input$pid),
          trial_kind = "performance",
          chart = rv$chart, year = rv$story_year, trial_idx = rv$idx,
          task_type = ttype, correct_answer = NA_character_, submitted = NA_character_,
          correct = NA, seconds = NA_real_, timeout = NA, perc_q = NA_integer_, survey_question = "skip_no_data"
        ))
        session$sendCustomMessage("auto_next", list())
        return()
      }
      correct_label <- get_correct_answer(counts, ttype)
      choices <- make_choices(counts, correct_label, k = 4, seed = 1000 + rv$idx)
      rv$task_type <- ttype
      rv$choices <- choices
      rv$start_time <- Sys.time()
      # snapshot local values for later callback
      local_start <- rv$start_time
      local_idx <- rv$idx
      local_chart <- rv$chart
      local_year <- rv$story_year
      local_task <- rv$task_type

      # schedule timeout check (PERFORMANCE_TIMEOUT seconds) using later()
      later::later(function() {
        # use isolate() to read reactive values safely inside later callback
        if (!is.null(isolate(rv$start_time)) && identical(isolate(rv$start_time), local_start) && !isolate(rv$answered)) {
          # log as timeout (seconds = PERFORMANCE_TIMEOUT, correct = FALSE)
          rv$log <- bind_rows(isolate(rv$log),
                              tibble(
                                participant = str_trim(input$pid),
                                trial_kind = "performance",
                                chart = local_chart,
                                year = local_year,
                                trial_idx = local_idx,
                                task_type = local_task,
                                correct_answer = get_correct_answer(cat_counts_for_year(df, local_year), local_task),
                                submitted = NA_character_,
                                correct = FALSE,
                                seconds = PERFORMANCE_TIMEOUT,
                                timeout = TRUE,
                                perc_q = NA_integer_,
                                survey_question = NA_character_
                              ))
          # mark as no longer active
          rv$start_time <- NULL
          rv$answered <- TRUE
          # auto advance
          session$sendCustomMessage("auto_next", list())
        }
      }, delay = PERFORMANCE_TIMEOUT)

      rv$prompt <- paste0("[", pretty_chart_name(rv$chart), " — Performance: Q", rv$idx, " of ", nrow(rv$plan), "] ",
                          "In year ", rv$story_year, ", which offense category has the ", task_text(ttype), " number of incidents?")
    } else {
      # perception trial (untimed)
      perc_q <- as.integer(row$perc_q)
      rv$task_type <- NA_character_
      rv$choices <- NULL
      rv$start_time <- NULL
      if (perc_q == 1) {
        rv$prompt <- paste0("[", pretty_chart_name(rv$chart), " — Perception: Q", rv$idx, " of ", nrow(rv$plan), "] ",
                            "How clear was this visualization for answering the questions? (1 = not clear, 5 = very clear)")
      } else {
        rv$prompt <- paste0("[", pretty_chart_name(rv$chart), " — Perception: Q", rv$idx, " of ", nrow(rv$plan), "] ",
                            "How confident are you in your answers? (1 = not confident, 5 = very confident)")
      }
    }
  }

  # client tells server to load next trial (auto_advance handler)
  observeEvent(input$auto_next_trigger, { load_next_trial() }, ignoreInit = TRUE)

  output$task_prompt <- renderText({ rv$prompt })

  # Answer UI
  output$answer_ui <- renderUI({
    if (is.null(rv$plan) || rv$idx == 0 || rv$idx > nrow(rv$plan)) {
      return(div(class = "subtle", "Preparing..."))
    }
    row <- rv$plan[rv$idx, ]
    if (row$phase == "performance") {
      if (is.null(rv$choices) || length(rv$choices) < 1) return(div(class = "subtle", "Loading choices..."))
      ch <- rv$choices
      btns <- lapply(seq_along(ch), function(i) {
        actionButton(inputId = paste0("ans_", i), label = ch[i], class = paste0("ansbtn ans", i))
      })
      div(class = "ans-grid", btns)
    } else {
      div(
        div(class = "likert-grid",
            lapply(1:5, function(i) actionButton(inputId = paste0("lik_", i), label = i, class = "likert-btn"))
        ),
        tags$div(style="font-size:12px;color:#666;margin-top:8px;", "Tap a number to continue.")
      )
    }
  })

  # ----- Performance handlers (single-click protected) -----
  handle_perf_click <- function(i) {
    # ensure trial active and not already answered
    if (is.null(rv$start_time) || rv$answered) return()
    ch <- isolate(rv$choices)
    if (is.null(ch) || length(ch) < i) return()
    submitted <- ch[i]
    secs <- as.numeric(difftime(Sys.time(), isolate(rv$start_time), units = "secs"))
    counts <- cat_counts_for_year(df, isolate(rv$story_year)) %>% filter(!is.na(offense_category) & offense_category != "")
    correct_label <- get_correct_answer(counts, isolate(rv$task_type))
    is_correct <- identical(submitted, correct_label)

    # record log row (timeout = FALSE)
    rv$log <- bind_rows(isolate(rv$log), tibble(
      participant = str_trim(input$pid),
      trial_kind = "performance",
      chart = isolate(rv$chart),
      year = isolate(rv$story_year),
      trial_idx = isolate(rv$idx),
      task_type = isolate(rv$task_type),
      correct_answer = correct_label,
      submitted = submitted,
      correct = is_correct,
      seconds = secs,
      timeout = FALSE,
      perc_q = NA_integer_,
      survey_question = NA_character_
    ))

    # prevent double-logging
    rv$answered <- TRUE
    # clear start_time
    rv$start_time <- NULL
    # auto-advance
    session$sendCustomMessage("auto_next", list())
    invisible(TRUE)
  }

  observeEvent(input$ans_1, { handle_perf_click(1) }, ignoreInit = TRUE)
  observeEvent(input$ans_2, { handle_perf_click(2) }, ignoreInit = TRUE)
  observeEvent(input$ans_3, { handle_perf_click(3) }, ignoreInit = TRUE)
  observeEvent(input$ans_4, { handle_perf_click(4) }, ignoreInit = TRUE)

  # ----- Perception handlers (immediate advance) -----
  handle_perc <- function(val) {
    if (is.null(rv$plan) || rv$idx == 0) return()
    row <- rv$plan[rv$idx, ]
    if (row$phase != "perception") return()
    qtxt <- if (row$perc_q == 1) "Clarity (1-5)" else "Confidence (1-5)"

    rv$log <- bind_rows(isolate(rv$log), tibble(
      participant = str_trim(input$pid),
      trial_kind = "perception",
      chart = as.character(row$chart),
      year = as.integer(row$year),
      trial_idx = rv$idx,
      task_type = NA_character_,
      correct_answer = NA_character_,
      submitted = as.character(val),
      correct = NA,
      seconds = NA_real_,
      timeout = NA,
      perc_q = as.integer(row$perc_q),
      survey_question = qtxt
    ))
    # advance to next trial
    session$sendCustomMessage("auto_next", list())
  }

  observeEvent(input$lik_1, { handle_perc(1) }, ignoreInit = TRUE)
  observeEvent(input$lik_2, { handle_perc(2) }, ignoreInit = TRUE)
  observeEvent(input$lik_3, { handle_perc(3) }, ignoreInit = TRUE)
  observeEvent(input$lik_4, { handle_perc(4) }, ignoreInit = TRUE)
  observeEvent(input$lik_5, { handle_perc(5) }, ignoreInit = TRUE)

  # ----- Preference handlers (single choice, then finish) -----
  observeEvent(input$pref_treemap, {
    rv$log <- bind_rows(isolate(rv$log), tibble(
      participant = str_trim(input$pid), trial_kind = "preference", chart = "Tree Map",
      year = rv$participant_year, trial_idx = NA_integer_, task_type = NA_character_, correct_answer = NA_character_,
      submitted = "Tree Map", correct = NA, seconds = NA_real_, timeout = NA, perc_q = NA_integer_, survey_question = "Preference"
    ))
    rv$screen <- "done"
  }, ignoreInit = TRUE)

  observeEvent(input$pref_bar, {
    rv$log <- bind_rows(isolate(rv$log), tibble(
      participant = str_trim(input$pid), trial_kind = "preference", chart = "Bar Chart",
      year = rv$participant_year, trial_idx = NA_integer_, task_type = NA_character_, correct_answer = NA_character_,
      submitted = "Bar Chart", correct = NA, seconds = NA_real_, timeout = NA, perc_q = NA_integer_, survey_question = "Preference"
    ))
    rv$screen <- "done"
  }, ignoreInit = TRUE)

  observeEvent(input$pref_stack, {
    rv$log <- bind_rows(isolate(rv$log), tibble(
      participant = str_trim(input$pid), trial_kind = "preference", chart = "Stack Chart",
      year = rv$participant_year, trial_idx = NA_integer_, task_type = NA_character_, correct_answer = NA_character_,
      submitted = "Stack Chart", correct = NA, seconds = NA_real_, timeout = NA, perc_q = NA_integer_, survey_question = "Preference"
    ))
    rv$screen <- "done"
  }, ignoreInit = TRUE)

  observeEvent(input$pref_line, {
    rv$log <- bind_rows(isolate(rv$log), tibble(
      participant = str_trim(input$pid), trial_kind = "preference", chart = "Line Graph",
      year = rv$participant_year, trial_idx = NA_integer_, task_type = NA_character_, correct_answer = NA_character_,
      submitted = "Line Graph", correct = NA, seconds = NA_real_, timeout = NA, perc_q = NA_integer_, survey_question = "Preference"
    ))
    rv$screen <- "done"
  }, ignoreInit = TRUE)

  # Quit/Home
  observeEvent(input$back_home, {
    rv$screen <- "instructions"
    rv$plan <- NULL
    rv$idx <- 0
    rv$participant_year <- NA_integer_
    rv$choices <- NULL
    rv$prompt <- ""
    rv$start_time <- NULL
    rv$answered <- FALSE
    rv$log <- tibble(
      participant = character(), trial_kind = character(), chart = character(),
      year = integer(), trial_idx = integer(), task_type = character(),
      correct_answer = character(), submitted = character(), correct = logical(),
      seconds = numeric(), timeout = logical(), perc_q = integer(), survey_question = character()
    )
  }, ignoreInit = TRUE)

  # Plot output
  output$quiz_plot <- renderPlotly({
    if (is.na(rv$participant_year) && rv$screen != "instructions") {
      return(plot_ly() %>% layout(title = "Please enter Participant ID and start."))
    }
    yr <- if (!is.na(rv$story_year)) rv$story_year else rv$participant_year
    chart_to_show <- if (!is.na(rv$chart)) rv$chart else if (!is.null(rv$plan)) as.character(rv$plan$chart[1]) else "bar"
    req(!is.null(chart_to_show))
    p <- plot_for_year(yr, chart_to_show, df)
    p %>% layout(title = list(text = paste0(pretty_chart_name(chart_to_show), " (Year ", yr, ")"))) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  # tiny reactive dependency so UI updates when answered toggles (prevents accidental double clicks)
  observe({
    rv$answered
    invisible(NULL)
  })
}

shinyApp(ui, server)