# app.R
# NYPD Hate Crimes — Story Mode (hidden scoring) + Charts
#
# ✅ Participants do NOT see time/accuracy (logged silently).
# ✅ Researcher downloads results as Excel (.xlsx).
# ✅ Randomized tasks (MAX / MIN / SECOND) with 4 choices (safe UI).
# ✅ Participants choose chart type (Bar/Treemap/Stack/Line).
# ✅ NO year toggle for participants.
# ✅ IMPORTANT UPDATE: ONE year per participant (same year across ALL charts/tasks for that participant).
#    - New participant (different Participant ID) -> different year (deterministic).
#
# Requirements:
#   put NYPD_Hate_Crimes_20260128.xlsx in same folder
# install.packages(c("shiny","readxl","dplyr","stringr","plotly","tibble","tidyr","writexl"))

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

# ✅ One year per participant (deterministic from Participant ID)
pick_year_for_pid <- function(pid, years_vec) {
  years_vec <- sort(unique(years_vec))
  if (length(years_vec) == 0) return(NA_integer_)

  pid <- toupper(str_trim(as.character(pid)))
  if (is.na(pid) || pid == "") pid <- "P00"

  s <- sum(utf8ToInt(pid))
  idx <- (s %% length(years_vec)) + 1
  years_vec[idx]
}

# ✅ Task pool: ALL tasks use the SAME year (participant_year)
make_task_pool <- function(one_year, n_tasks = 12, seed = 490) {
  set.seed(seed)
  tibble(
    task_id = seq_len(n_tasks),
    year = rep(as.integer(one_year), n_tasks),
    task_type = sample(c("MAX", "MIN", "SECOND"),
                       size = n_tasks, replace = TRUE,
                       prob = c(0.4, 0.4, 0.2))
  )
}

# Safe multiple-choice builder
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
  # counts is desc by n already
  if (task_type == "MAX") {
    return(counts$offense_category[which.max(counts$n)][1])
  }
  if (task_type == "MIN") {
    return(counts$offense_category[which.min(counts$n)][1])
  }
  # SECOND: 2nd highest (ties handled by row order)
  if (nrow(counts) < 2) return(counts$offense_category[1])
  return(counts$offense_category[2])
}

task_text <- function(task_type) {
  if (task_type == "MAX") return("highest")
  if (task_type == "MIN") return("lowest")
  "second-highest"
}

# ---------- UI ----------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .plot-container { width:100%; height: calc(100vh - 160px); }
      .plot-container .plotly { height:100% !important; }
      .taskbox { background:#f7f7f7; padding:12px; border-radius:10px; border:1px solid #ddd; margin-bottom:10px;}
      .small { color:#555; font-size: 12px; }
    "))
  ),

  titlePanel("NYPD Hate Crimes — Story Mode (Tasks) + Interactive Charts"),
  sidebarLayout(
    sidebarPanel(
      h4("Story Mode"),
      textInput("pid", "Participant ID:", value = "P01"),
      selectInput(
        "story_chart",
        "Chart used for tasks:",
        choices = c("Bar" = "bar", "Treemap" = "treemap", "Stack" = "stack", "Line" = "line"),
        selected = "bar"
      ),
      actionButton("start_story", "Start / Restart"),
      actionButton("next_task", "Next task"),
      hr(),
      # ✅ researcher-only output
      downloadButton("download_log_xlsx", "Download Results (Excel)"),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Story Mode",
          div(class = "taskbox",
              verbatimTextOutput("task_prompt"),
              div(class = "small", "Tip: Hover for counts. Zoom/pan if needed.")
          ),
          uiOutput("answer_ui"),
          actionButton("submit", "Submit answer"),
          plotlyOutput("story_plot", height = "720px")
        ),
        tabPanel("Bar", div(class = "plot-container", plotlyOutput("bar_plot", height = "100%"))),
        tabPanel("Treemap", div(class = "plot-container", plotlyOutput("treemap", height = "100%"))),
        tabPanel("Stack", div(class = "plot-container", plotlyOutput("stack_plot", height = "100%"))),
        tabPanel("Line", div(class = "plot-container", plotlyOutput("line_plot", height = "100%")))
      )
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {

  rv <- reactiveValues(
    tasks = NULL,
    i = 0,
    start_time = NULL,
    story_year = NA_integer_,
    story_type = NA_character_,
    correct = NA_character_,
    prompt = "Click Start / Restart to begin. Then click Next task.",
    choices = NULL,
    participant_year = NA_integer_,   # ✅ one year per participant
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

  # Dynamic answers UI (prevents empty radio crash)
  output$answer_ui <- renderUI({
    if (is.null(rv$choices) || length(rv$choices) < 2) {
      return(tags$div(class = "small", "No answer choices yet. Click “Next task”."))
    }
    radioButtons("answer", "Choose your answer:", choices = rv$choices, selected = character(0))
  })

  observeEvent(input$start_story, {
    # ✅ choose one fixed year per participant
    py <- pick_year_for_pid(input$pid, years)
    rv$participant_year <- py

    rv$tasks <- make_task_pool(py, n_tasks = 12, seed = 490)
    rv$i <- 0
    rv$choices <- NULL
    rv$correct <- NA_character_
    rv$story_year <- NA_integer_
    rv$story_type <- NA_character_
    rv$start_time <- NULL

    rv$prompt <- paste0("Tasks loaded. (Your session year is set.) Click Next task.")
  })

  observeEvent(input$next_task, {
    req(!is.null(rv$tasks))
    req(!is.na(rv$participant_year))

    rv$i <- rv$i + 1
    if (rv$i > nrow(rv$tasks)) rv$i <- 1

    task <- rv$tasks[rv$i, ]
    yr <- as.integer(task$year)             # will always equal participant_year
    ttype <- as.character(task$task_type)

    counts <- cat_counts_for_year(df, yr) %>%
      filter(!is.na(offense_category) & offense_category != "")

    if (nrow(counts) < 2) {
      rv$prompt <- paste("Not enough categories in year", yr, "— click Next task.")
      rv$choices <- NULL
      rv$start_time <- NULL
      return()
    }

    correct_label <- get_correct_answer(counts, ttype)
    choices <- make_choices(counts, correct_label, k = 4, seed = 490 + rv$i)

    if (length(choices) < 2) {
      rv$prompt <- paste("Could not build choices for year", yr, "— click Next task.")
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
      "Task ", rv$i, " (", ttype, "): In year ", yr,
      ", which offense category has the ", task_text(ttype), " number of incidents?"
    )
  })

  output$task_prompt <- renderText(rv$prompt)

  # Submit answer (✅ no correctness/time shown to participant)
  observeEvent(input$submit, {
    if (is.null(rv$choices) || length(rv$choices) < 2) return()
    req(rv$i > 0, !is.null(rv$start_time), !is.na(rv$story_year), !is.na(rv$story_type))

    submitted <- input$answer
    if (is.null(submitted) || submitted == "") return()

    secs <- as.numeric(difftime(Sys.time(), rv$start_time, units = "secs"))
    is_correct <- identical(submitted, rv$correct)

    rv$log <- bind_rows(rv$log, tibble(
      participant = str_trim(input$pid),
      chart = input$story_chart,
      task_id = rv$i,
      year = rv$story_year,
      task_type = rv$story_type,
      correct_answer = rv$correct,
      submitted = submitted,
      correct = is_correct,
      seconds = secs
    ))

    rv$start_time <- NULL
  })

  # Download Excel (researcher-only)
  output$download_log_xlsx <- downloadHandler(
    filename = function() paste0("nypd_storymode_results_", Sys.Date(), ".xlsx"),
    content = function(file) {
      writexl::write_xlsx(list(results = rv$log), path = file)
    }
  )

  # ---- Plot builders (shared) ----
  plot_for_year <- function(yr, chart_type) {
    x <- df %>% filter(year == yr)
    if (nrow(x) == 0) return(plot_ly() %>% layout(title = "No data for this year"))

    counts <- x %>% count(offense_category, name = "n") %>% arrange(desc(n))
    ord <- counts$offense_category
    counts <- counts %>% mutate(offense_category = factor(offense_category, levels = ord))

    if (chart_type == "bar") {
      return(
        plot_ly(counts, x = ~offense_category, y = ~n, type = "bar",
                hovertemplate = "<b>%{x}</b><br>Incidents: %{y}<extra></extra>") %>%
          layout(title = paste0("Bar (", yr, ")"),
                 xaxis = list(title = "Offense Category", tickangle = -45),
                 yaxis = list(title = "Incidents"),
                 margin = list(b = 180))
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
        plot_ly(nodes, type = "treemap",
                ids = ~ids, labels = ~labels, parents = ~parents, values = ~values,
                textinfo = "label+value",
                hovertemplate = "<b>%{label}</b><br>Incidents: %{value}<extra></extra>") %>%
          layout(title = paste0("Treemap (", yr, ")"),
                 margin = list(t = 40, l = 0, r = 0, b = 0))
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
        plot_ly(by_month, x = ~offense_category, y = ~n, color = ~month, type = "bar",
                hovertemplate = "<b>%{x}</b><br>Month: %{fullData.name}<br>Incidents: %{y}<extra></extra>") %>%
          layout(barmode = "stack",
                 title = paste0("Stacked Bar by Month (", yr, ")"),
                 xaxis = list(title = "Offense Category", tickangle = -45),
                 yaxis = list(title = "Incidents"),
                 margin = list(b = 180),
                 legend = list(title = list(text = "<b>Month</b>")))
      )
    }

    # line
    plot_ly(counts, x = ~offense_category, y = ~n, type = "scatter",
            mode = "lines+markers",
            hovertemplate = "<b>%{x}</b><br>Incidents: %{y}<extra></extra>") %>%
      layout(title = paste0("Line (", yr, ")"),
             xaxis = list(title = "Offense Category", tickangle = -45),
             yaxis = list(title = "Incidents"),
             margin = list(b = 180))
  }

  # Story plot uses current task year + selected chart
  output$story_plot <- renderPlotly({
    req(!is.na(rv$story_year))
    plot_for_year(rv$story_year, input$story_chart)
  })

  # Explore plots use participant_year once Start is pressed; otherwise default = most recent year
  explore_year <- reactive({
    if (!is.na(rv$participant_year)) rv$participant_year else max(years)
  })

  output$bar_plot   <- renderPlotly(plot_for_year(explore_year(), "bar"))
  output$treemap    <- renderPlotly(plot_for_year(explore_year(), "treemap"))
  output$stack_plot <- renderPlotly(plot_for_year(explore_year(), "stack"))
  output$line_plot  <- renderPlotly(plot_for_year(explore_year(), "line"))
}

shinyApp(ui, server)