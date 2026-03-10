# app.R
# NYPD Hate Crimes — Side-by-side with taller graph column + visible digital timer
# - 2 timed performance per chart (custom per-chart questions)
# - 1 perception (confidence 1-5) per chart (untimed)
# - 4 charts => 12 trials, then preference (single choice)
# - Same preset year per participant (deterministic from PID)
# - Per-question timeout (later::later), auto-advance after answer (0.5s)
# - Single-click answer protection, hidden scoring for participants
# - Researcher download includes time + correctness + timeout flag + per-participant confidence & preference
# - Left (graph) column taller than right (question) column; both equal width
# - Visible digital timer (numeric + progress bar) appears below question area on performance trials
# - Custom performance questions per chart as requested
# - Data restricted to year 2025 only

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
# Load data (restrict to 2025 only)
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
  filter(!is.na(year), !is.na(month)) %>%
  filter(year == 2025)   # <-- ONLY 2025 data

years <- sort(unique(df$year))

# ---------------------------
# Helpers
# ---------------------------
cat_counts_for_year <- function(data, yr) {
  data %>% filter(year == yr) %>% count(offense_category, name = "n") %>% arrange(desc(n))
}

pick_year_for_pid <- function(pid, years_vec) {
  # since we only use 2025, fallback to 2025 if pid mapping fails
  years_vec <- sort(unique(years_vec))
  if (length(years_vec) == 0) return(2025L)
  pid <- toupper(str_trim(as.character(pid)))
  if (is.na(pid) || pid == "") pid <- "P00"
  s <- sum(utf8ToInt(pid))
  idx <- (s %% length(years_vec)) + 1
  years_vec[idx]
}

pretty_chart_name <- function(x) {
  switch(x,
         "treemap" = "Tree Map",
         "bar"     = "Bar Chart",
         "stack"   = "Stack Chart",
         "line"    = "Line Graph",
         "Chart")
}

# Generate candidate choices & the correct label(s) for custom task types
generate_choices_for_task <- function(counts, task_code, k = 4, seed = 490) {
  labels <- as.character(counts$offense_category)
  nums <- counts$n

  set.seed(seed + nchar(task_code))

  draw_distractors <- function(correct_label, k) {
    pool <- setdiff(labels, correct_label)
    n_d <- min(k - 1, length(pool))
    if (n_d <= 0) return(character(0))
    sample(pool, n_d, replace = FALSE)
  }

  if (task_code == "BAR_SECOND_HIGHEST") {
    if (nrow(counts) >= 2) {
      ord_desc <- counts %>% arrange(desc(n))
      correct <- ord_desc$offense_category[2]
    } else {
      correct <- counts$offense_category[1]
    }
    choices <- unique(c(correct, draw_distractors(correct, k)))
    choices <- sample(choices, length(choices))
    return(list(choices = choices, correct = correct))
  }

  if (task_code == "BAR_NUM25") {
    if (length(nums) == 0) {
      correct <- labels[1]
    } else {
      diffs <- abs(nums - 25)
      idx <- which.min(diffs)
      correct <- labels[idx]
    }
    choices <- unique(c(correct, draw_distractors(correct, k)))
    choices <- sample(choices, length(choices))
    return(list(choices = choices, correct = correct))
  }

  if (task_code == "STACK_NUM58") {
    # fixed numeric options for the Stack Chart Q2
    choices <- c("27", "39", "46", "58")
    correct <- "58"
    return(list(choices = choices, correct = correct))
  }

  # other existing tasks preserved...
  if (task_code == "BAR_MEDIAN") {
    ord <- counts %>% arrange(n)
    idx <- ceiling(nrow(ord) / 2)
    correct <- ord$offense_category[idx]
    choices <- unique(c(correct, draw_distractors(correct, k)))
    choices <- sample(choices, length(choices))
    return(list(choices = choices, correct = correct))
  }

  if (task_code == "TREEMAP_SECOND_LOWEST") {
    ord <- counts %>% arrange(n)
    if (nrow(ord) >= 2) correct <- ord$offense_category[2] else correct <- ord$offense_category[1]
    choices <- unique(c(correct, draw_distractors(correct, k)))
    choices <- sample(choices, length(choices))
    return(list(choices = choices, correct = correct))
  }

  if (task_code == "TREEMAP_CLOSE_PAIR") {
    if (nrow(counts) < 2) {
      correct_pair <- paste(labels[1], "&", labels[1])
      choices <- c(correct_pair)
      return(list(choices = choices, correct = correct_pair))
    }
    m <- expand.grid(i = seq_len(nrow(counts)), j = seq_len(nrow(counts)))
    m <- m[m$i < m$j, , drop = FALSE]
    m$diff <- abs(nums[m$i] - nums[m$j])
    m <- m[order(m$diff, decreasing = FALSE), , drop = FALSE]
    best <- m[1, ]
    a <- labels[best$i]; b <- labels[best$j]
    correct_pair <- paste(a, "&", b)
    all_pairs <- apply(m, 1, function(r) paste(labels[as.integer(r["i"])], "&", labels[as.integer(r["j"])]))
    pool <- setdiff(unique(all_pairs), correct_pair)
    n_d <- min(k - 1, length(pool))
    distractors <- if (n_d > 0) sample(pool, n_d) else character(0)
    choices <- unique(c(correct_pair, distractors))
    choices <- sample(choices, length(choices))
    return(list(choices = choices, correct = correct_pair))
  }

  if (task_code == "STACK_LOWEST") {
    ord <- counts %>% arrange(n)
    correct <- ord$offense_category[1]
    choices <- unique(c(correct, draw_distractors(correct, k)))
    choices <- sample(choices, length(choices))
    return(list(choices = choices, correct = correct))
  }

  if (task_code == "LINE_HIGH_SPIKE") {
    idx <- which.max(nums)
    correct <- labels[idx]
    choices <- unique(c(correct, draw_distractors(correct, k)))
    choices <- sample(choices, length(choices))
    return(list(choices = choices, correct = correct))
  }

  if (task_code == "LINE_CLOSE_TO_HIGHEST") {
    if (nrow(counts) < 2) {
      correct <- labels[1]
    } else {
      max_idx <- which.max(nums)
      diffs_to_max <- abs(nums - nums[max_idx])
      diffs_to_max[max_idx] <- Inf
      idx <- which.min(diffs_to_max)
      correct <- labels[idx]
    }
    choices <- unique(c(correct, draw_distractors(correct, k)))
    choices <- sample(choices, length(choices))
    return(list(choices = choices, correct = correct))
  }

  # fallback
  choices <- head(labels, k)
  correct <- choices[1]
  list(choices = choices, correct = correct)
}

# Mapping of chart -> two task codes and prompts (exact wording)
chart_tasks <- list(
  bar = list(
    codes = c("BAR_SECOND_HIGHEST", "BAR_NUM25"),
    prompts = c(
      "Bar Chart — 1. Which category was the second-highest?",
      "Bar Chart — 2. Which category had 25 number of incidents?"
    )
  ),
  treemap = list(
    codes = c("TREEMAP_SECOND_LOWEST", "TREEMAP_CLOSE_PAIR"),
    prompts = c(
      "Tree Map — 1. Which category was the second lowest?",
      "Tree Map — 2. Which two categories are close to each other in number of incidents?"
    )
  ),
  stack = list(
    codes = c("STACK_LOWEST", "STACK_NUM58"),
    prompts = c(
      "Stack Chart — 1. Which category was the lowest?",
      # modified prompt
      "Stack Chart - 2. What is the number of incidents happened in the Religion/Religious Practice category in the month of April?"
    )
  ),
  line = list(
    codes = c("LINE_HIGH_SPIKE", "LINE_CLOSE_TO_HIGHEST"),
    prompts = c(
      "Line Graph — 1. Which category had the highest spike?",
      "Line Graph — 2. Which category was close to the highest spike?"
    )
  )
)

# Plot builder (unchanged)
plot_for_year <- function(yr, chart_type, data_df) {
  x <- data_df %>% filter(year == yr)
  if (nrow(x) == 0) return(plot_ly() %>% layout(title = "No data for this year"))
  counts <- x %>% count(offense_category, name = "n") %>% arrange(desc(n))
  ord <- counts$offense_category
  counts <- counts %>% mutate(offense_category = factor(offense_category, levels = ord))

  title_text <- paste0(pretty_chart_name(chart_type), " (Year ", yr, ")")
  title_layout <- list(title = list(text = title_text, x = 0.5, xanchor = "center"),
                       margin = list(t = 80, l = 80, r = 40, b = 110))

  if (chart_type == "bar") {
    p <- plot_ly(counts, x = ~offense_category, y = ~n, type = "bar",
                 hovertemplate = "<b>%{x}</b><br>Incidents: %{y}<extra></extra>")
    p %>% layout(title = title_layout$title, margin = title_layout$margin,
                 xaxis = list(title = "offense_category", tickangle = -35, automargin = TRUE),
                 yaxis = list(title = "n", automargin = TRUE)) %>%
      config(displayModeBar = FALSE, responsive = TRUE)

  } else if (chart_type == "treemap") {
    boost <- 5
    nodes <- tibble(
      ids = as.character(counts$offense_category),
      labels = as.character(counts$offense_category),
      parents = "",
      values_adj = counts$n + boost,
      values_raw = counts$n,
      text = paste0(as.character(counts$offense_category), "\n", counts$n)
    )

    p <- plot_ly(
      nodes,
      type = "treemap",
      ids = ~ids,
      labels = ~labels,
      parents = ~parents,
      values = ~values_adj,
      text = ~text,
      customdata = ~values_raw,
      textinfo = "text",
      hovertemplate = "<b>%{label}</b><br>Incidents: %{customdata}<extra></extra>",
      marker = list(pad = list(t = 1, l = 1, r = 1, b = 1)),
      tiling = list(packing = "squarify")
    )

    p %>% layout(title = title_layout$title, margin = title_layout$margin) %>%
      config(displayModeBar = FALSE, responsive = TRUE)

  } else if (chart_type == "stack") {
    by_month <- x %>%
      count(offense_category, month, name = "n") %>%
      complete(offense_category = unique(x$offense_category), month = 1:12, fill = list(n = 0)) %>%
      mutate(offense_category = factor(offense_category, levels = ord),
             month = factor(month, levels = 1:12, labels = month.abb)) %>%
      arrange(offense_category, month)

    p <- plot_ly(by_month, x = ~n, y = ~offense_category, color = ~month, type = "bar", orientation = "h",
                 hovertemplate = "<b>%{y}</b><br>Month: %{fullData.name}<br>Incidents: %{x}<extra></extra>")
    p %>% layout(barmode = "stack",
                 title = title_layout$title,
                 margin = title_layout$margin,
                 xaxis = list(title = "Incidents", automargin = TRUE),
                 yaxis = list(title = "Offense Category", automargin = TRUE, tickangle = 0)) %>%
      config(displayModeBar = FALSE, responsive = TRUE)

  } else {
    p <- plot_ly(counts, x = ~offense_category, y = ~n, type = "scatter", mode = "lines+markers",
                 hovertemplate = "<b>%{x}</b><br>Incidents: %{y}<extra></extra>")
    p %>% layout(title = title_layout$title, margin = title_layout$margin,
                 xaxis = list(title = "offense_category", tickangle = -35, automargin = TRUE),
                 yaxis = list(title = "n", automargin = TRUE)) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  }
}

# ---------------------------
# UI
# ---------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background: #ffffff; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial; }
      .app-wrap { max-width: 1500px; margin: 0 auto; padding: 10px; }
      .big-title { font-size: 28px; font-weight: 700; margin: 6px 0; }
      .subtle { color: #666; }
      .card { border: 1px solid #e5e5e5; border-radius: 10px; padding: 14px; background: #fafafa; }
      .plot-container { display:flex; gap:18px; align-items:flex-start; }
      .plot-box { flex: 1 1 50%; border: 1px solid #eee; border-radius: 10px; padding: 8px; background: #fff; min-width: 300px; }
      .question-box { flex: 1 1 50%; border: 1px solid #e5e5e5; border-radius: 10px; padding: 14px; background: #fafafa; min-width: 300px; }
      .plot-height { height: 72vh; }    /* Taller graph column */
      .question-height { height: 44vh; } /* Shorter question column */
      .plot-height .plotly { height: 100% !important; }
      .question-box { overflow: auto; display:flex; flex-direction:column; justify-content: flex-start; }
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
      .timer-tile { margin-top:12px; border-radius:8px; border:1px solid #e6e6e6; padding:8px; background:#ffffff; }
      .timer-numeric { font-weight:800; font-size:20px; text-align:center; padding:4px 0; }
      .timer-bar { height:12px; background:#e9ecef; border-radius:8px; overflow:hidden; margin-top:8px; }
      .timer-bar-inner { height:100%; width:0%; background:#1e90ff; transition: width 0.2s linear; }
      .topbar { display:flex; gap: 10px; align-items:center; justify-content: space-between; margin-bottom: 12px;}
    "))
  ),

  div(class = "app-wrap",
      div(class = "topbar",
          div(
            div(class = "big-title", "NYPD Hate Crimes — Visualization Speed Test"),
            div(class = "subtle", "Per chart: 2 timed performance questions then 1 perception (confidence) question. Data: 2025 only.")
          ),
          div(class = "card",
              textInput("pid", "Participant ID", value = "P01"),
              downloadButton("download_log_xlsx", "Researcher: Download Results (Excel)")
          )
      ),

      uiOutput("screen_ui")
  ),

  # JS handlers: countdown + small auto-next handler + digital question timer
  tags$script(HTML(paste0("
    // countdown before starting sequence
    Shiny.addCustomMessageHandler('start_countdown', function(message) {
      var el = document.getElementById('countdown_text'); if (!el) return;
      var seq = ['3','2','1','START']; var i = 0;
      function tick(){ el.textContent = seq[i]; i++; if (i < seq.length) { setTimeout(tick, 900); } else { setTimeout(function(){ Shiny.setInputValue('countdown_done', Date.now(), {priority:'event'}); }, 600); } }
      tick();
    });

    // small auto-next after answer
    Shiny.addCustomMessageHandler('auto_next', function(message) {
      setTimeout(function(){ Shiny.setInputValue('auto_next_trigger', Date.now(), {priority:'event'}); }, ", AUTO_ADVANCE_MS, ");
    });

    // digital question timer: start_qtimer (seconds) and stop_qtimer
    (function(){
      var qInterval = null;
      var qEnd = null;
      var lastDuration = null;

      function clearQTimer(){
        if (qInterval) { clearInterval(qInterval); qInterval = null; }
        qEnd = null;
        lastDuration = null;
        var el = document.getElementById('digital_timer'); if (el) el.textContent = '';
        var barInner = document.getElementById('timer_bar_inner'); if (barInner) barInner.style.width = '0%';
      }

      Shiny.addCustomMessageHandler('start_qtimer', function(message){
        var dur = parseFloat(message.duration) || ", PERFORMANCE_TIMEOUT, ";
        lastDuration = dur;
        qEnd = Date.now() + Math.round(dur * 1000);
        function update(){
          var now = Date.now();
          var remainingMs = qEnd - now;
          if (remainingMs < 0) remainingMs = 0;
          var remainingSec = Math.ceil(remainingMs / 1000);
          var el = document.getElementById('digital_timer');
          if (el) el.textContent = remainingSec + 's';
          var barInner = document.getElementById('timer_bar_inner');
          if (barInner && lastDuration) {
            var pct = Math.max(0, Math.min(1, remainingMs / (lastDuration * 1000)));
            barInner.style.width = (pct * 100) + '%';
          }
          if (remainingMs <= 0) {
            clearQTimer();
          }
        }
        clearQTimer();
        update();
        qInterval = setInterval(update, 250);
      });

      Shiny.addCustomMessageHandler('stop_qtimer', function(message){
        clearQTimer();
      });

      window.addEventListener('beforeunload', function(){ clearQTimer(); });
    })();
  ")))
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
    correct_label = NA_character_,
    prompt = "",
    start_time = NULL,         # actual Sys.time() when performance trial presented
    answered = FALSE,          # to prevent double-logging
    # Note: removed perc_q & survey_question. Added per-chart conf columns + preference
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
      conf_treemap = integer(),
      conf_bar = integer(),
      conf_stack = integer(),
      conf_line = integer(),
      preference = character()
    )
  )

  # Researcher download (writes raw trial-level 'results' sheet and participant-level 'summary' sheet)
  output$download_log_xlsx <- downloadHandler(
    filename = function() paste0("nypd_results_", Sys.Date(), ".xlsx"),
    content = function(file) {
      results_raw <- isolate(rv$log)

      # Build participant-level summary:
      # For each participant:
      #   - conf_treemap/conf_bar/conf_stack/conf_line: first non-NA value found in results_raw for that participant
      #   - preference: first non-NA preference string
      if (nrow(results_raw) == 0) {
        summary_df <- tibble(
          participant = character(),
          conf_treemap = integer(),
          conf_bar = integer(),
          conf_stack = integer(),
          conf_line = integer(),
          preference = character()
        )
      } else {
        participants <- unique(results_raw$participant)
        first_non_na <- function(x) {
          y <- x[!is.na(x)]
          if (length(y) == 0) return(NA)
          return(y[1])
        }

        summary_list <- lapply(participants, function(p) {
          r <- results_raw %>% filter(participant == p)
          tibble(
            participant = p,
            conf_treemap = as.integer(first_non_na(r$conf_treemap)),
            conf_bar     = as.integer(first_non_na(r$conf_bar)),
            conf_stack   = as.integer(first_non_na(r$conf_stack)),
            conf_line    = as.integer(first_non_na(r$conf_line)),
            preference   = as.character(first_non_na(r$preference))
          )
        })

        summary_df <- bind_rows(summary_list)
      }

      writexl::write_xlsx(
        list(
          results = results_raw,
          summary = summary_df
        ),
        path = file
      )
    }
  )

  # Screen UI (unchanged)
  output$screen_ui <- renderUI({
    if (rv$screen == "instructions") {
      div(class = "card",
          h4("Instructions"),
          tags$ul(
            tags$li("For each visualization you'll answer two timed questions (custom per-chart)."),
            tags$li("After those, you'll answer one perception question: confidence (1–5)."),
            tags$li("Clicking an answer for questions logs time & correctness and auto-advances."),
            tags$li("Confidence questions are untimed; click a rating to continue."),
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
      div(class = "plot-container",
          div(class = "plot-box plot-height", plotlyOutput("quiz_plot", height = "100%")),
          div(class = "question-box question-height",
              div(class = "question-text", textOutput("task_prompt")),
              uiOutput("answer_ui"),
              tags$div(class = "timer-tile", style = "display:block;",
                       tags$div(id = "digital_timer", class = "timer-numeric", ""),
                       tags$div(class = "timer-bar", tags$div(id = "timer_bar_inner", class = "timer-bar-inner"))
              ),
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
    } else if (rv$screen == "done") {
      div(class = "card",
          h3("Done — thank you!"),
          p("You can close this tab or restart to collect data for another participant."),
          actionButton("restart", "Restart / New participant")
      )
    } else {
      div(class = "card", h3("Done — thank you!"), p("You can close this tab."))
    }
  })

  #### Start flow: build plan and countdown ####
  observeEvent(input$begin, {
    py <- pick_year_for_pid(isolate(input$pid), years)
    rv$participant_year <- py
    seed_val <- 490 + sum(utf8ToInt(str_trim(isolate(input$pid))))
    charts <- c("treemap", "bar", "stack", "line")
    set.seed(seed_val)
    charts_order <- sample(charts, length(charts))
    rows <- list(); idx <- 1
    for (ch in charts_order) {
      codes <- chart_tasks[[ch]]$codes
      rows[[idx]] <- tibble(trial_idx = idx, phase = "performance", chart = ch, year = as.integer(py), task_code = codes[1]); idx <- idx + 1
      rows[[idx]] <- tibble(trial_idx = idx, phase = "performance", chart = ch, year = as.integer(py), task_code = codes[2]); idx <- idx + 1
      rows[[idx]] <- tibble(trial_idx = idx, phase = "perception", chart = ch, year = as.integer(py), task_code = NA_character_); idx <- idx + 1
    }
    rv$plan <- bind_rows(rows)
    rv$idx <- 0
    rv$chart <- NA_character_; rv$story_year <- NA_integer_; rv$task_type <- NA_character_
    rv$choices <- NULL; rv$correct_label <- NA_character_; rv$prompt <- ""; rv$start_time <- NULL; rv$answered <- FALSE
    rv$screen <- "countdown"
    session$onFlushed(function() session$sendCustomMessage("start_countdown", list()), once = TRUE)
  })

  observeEvent(input$countdown_done, {
    rv$screen <- "quiz"
    rv$idx <- 0
    session$onFlushed(function() session$sendCustomMessage("auto_next", list()), once = TRUE)
  })

  #### Load next trial ####
  load_next_trial <- function() {
    req(!is.null(rv$plan), !is.na(rv$participant_year))
    rv$idx <- rv$idx + 1
    if (rv$idx > nrow(rv$plan)) {
      rv$screen <- "preference"
      session$sendCustomMessage("stop_qtimer", list())
      return()
    }

    row <- rv$plan[rv$idx, ]
    phase <- row$phase
    rv$chart <- as.character(row$chart)
    rv$story_year <- as.integer(row$year)
    rv$choices <- NULL
    rv$start_time <- NULL
    rv$answered <- FALSE
    rv$correct_label <- NA_character_

    if (phase == "performance") {
      counts <- cat_counts_for_year(df, rv$story_year) %>% filter(!is.na(offense_category) & offense_category != "")
      if (nrow(counts) < 1) {
        rv$log <- bind_rows(rv$log, tibble(
          participant = str_trim(isolate(input$pid)),
          trial_kind = "performance",
          chart = rv$chart, year = rv$story_year, trial_idx = rv$idx,
          task_type = as.character(row$task_code), correct_answer = NA_character_, submitted = NA_character_,
          correct = NA, seconds = NA_real_, timeout = NA,
          conf_treemap = NA_integer_, conf_bar = NA_integer_, conf_stack = NA_integer_, conf_line = NA_integer_, preference = NA_character_
        ))
        session$sendCustomMessage("auto_next", list())
        return()
      }
      task_code <- as.character(row$task_code)
      gc <- generate_choices_for_task(counts, task_code, k = 4, seed = 1000 + rv$idx)
      rv$choices <- gc$choices
      rv$correct_label <- gc$correct
      rv$start_time <- Sys.time()

      session$sendCustomMessage("start_qtimer", list(duration = PERFORMANCE_TIMEOUT))

      local_start <- rv$start_time
      local_idx <- rv$idx
      local_chart <- rv$chart
      local_year <- rv$story_year
      local_task <- task_code
      local_pid <- str_trim(isolate(input$pid))
      local_correct <- gc$correct

      later::later(function() {
        if (!is.null(isolate(rv$start_time)) && identical(isolate(rv$start_time), local_start) && !isolate(rv$answered)) {
          rv$log <- bind_rows(isolate(rv$log),
                              tibble(
                                participant = local_pid,
                                trial_kind = "performance",
                                chart = local_chart,
                                year = local_year,
                                trial_idx = local_idx,
                                task_type = local_task,
                                correct_answer = local_correct,
                                submitted = NA_character_,
                                correct = FALSE,
                                seconds = PERFORMANCE_TIMEOUT,
                                timeout = TRUE,
                                conf_treemap = NA_integer_, conf_bar = NA_integer_, conf_stack = NA_integer_, conf_line = NA_integer_, preference = NA_character_
                              ))
          rv$start_time <- NULL
          rv$answered <- TRUE
          session$sendCustomMessage("stop_qtimer", list())
          session$sendCustomMessage("auto_next", list())
        }
      }, delay = PERFORMANCE_TIMEOUT)

      chart_info <- chart_tasks[[rv$chart]]
      q_index <- match(task_code, chart_info$codes)
      if (is.na(q_index)) q_index <- 1
      rv$prompt <- chart_info$prompts[q_index]
    } else {
      rv$task_type <- NA_character_
      rv$choices <- NULL
      rv$start_time <- NULL
      rv$prompt <- paste0("How confident are you in your answers? (1 = not confident, 5 = very confident)")
      session$sendCustomMessage("stop_qtimer", list())
    }
  }

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

  # Performance click handler
  handle_perf_click <- function(i) {
    if (is.null(rv$start_time) || rv$answered) return()
    ch <- isolate(rv$choices)
    if (is.null(ch) || length(ch) < i) return()
    submitted <- ch[i]
    secs <- as.numeric(difftime(Sys.time(), isolate(rv$start_time), units = "secs"))
    is_correct <- FALSE
    if (!is.na(isolate(rv$correct_label))) {
      is_correct <- identical(submitted, isolate(rv$correct_label))
    }
    rv$log <- bind_rows(isolate(rv$log), tibble(
      participant = str_trim(isolate(input$pid)),
      trial_kind = "performance",
      chart = isolate(rv$chart),
      year = isolate(rv$story_year),
      trial_idx = isolate(rv$idx),
      task_type = as.character(isolate(rv$plan$task_code[rv$idx])),
      correct_answer = isolate(rv$correct_label),
      submitted = submitted,
      correct = is_correct,
      seconds = secs,
      timeout = FALSE,
      conf_treemap = NA_integer_, conf_bar = NA_integer_, conf_stack = NA_integer_, conf_line = NA_integer_, preference = NA_character_
    ))

    rv$answered <- TRUE
    rv$start_time <- NULL
    session$sendCustomMessage("stop_qtimer", list())
    session$sendCustomMessage("auto_next", list())
    invisible(TRUE)
  }

  observeEvent(input$ans_1, { handle_perf_click(1) }, ignoreInit = TRUE)
  observeEvent(input$ans_2, { handle_perf_click(2) }, ignoreInit = TRUE)
  observeEvent(input$ans_3, { handle_perf_click(3) }, ignoreInit = TRUE)
  observeEvent(input$ans_4, { handle_perf_click(4) }, ignoreInit = TRUE)

  # Perception handler: record confidence into the corresponding conf_* column
  handle_perc <- function(val) {
    if (is.null(rv$plan) || rv$idx == 0) return()
    row <- rv$plan[rv$idx, ]
    if (row$phase != "perception") return()

    # prepare empty confs
    confs <- list(conf_treemap = NA_integer_, conf_bar = NA_integer_, conf_stack = NA_integer_, conf_line = NA_integer_)
    # set the appropriate column for the current chart
    if (rv$chart == "treemap") confs$conf_treemap <- as.integer(val)
    if (rv$chart == "bar")     confs$conf_bar     <- as.integer(val)
    if (rv$chart == "stack")   confs$conf_stack   <- as.integer(val)
    if (rv$chart == "line")    confs$conf_line    <- as.integer(val)

    rv$log <- bind_rows(isolate(rv$log), tibble(
      participant = str_trim(isolate(input$pid)),
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
      conf_treemap = confs$conf_treemap,
      conf_bar = confs$conf_bar,
      conf_stack = confs$conf_stack,
      conf_line = confs$conf_line,
      preference = NA_character_
    ))

    session$sendCustomMessage("stop_qtimer", list())
    session$sendCustomMessage("auto_next", list())
  }

  observeEvent(input$lik_1, { handle_perc(1) }, ignoreInit = TRUE)
  observeEvent(input$lik_2, { handle_perc(2) }, ignoreInit = TRUE)
  observeEvent(input$lik_3, { handle_perc(3) }, ignoreInit = TRUE)
  observeEvent(input$lik_4, { handle_perc(4) }, ignoreInit = TRUE)
  observeEvent(input$lik_5, { handle_perc(5) }, ignoreInit = TRUE)

  # Preference handlers: record single-choice overall preference
  observeEvent(input$pref_treemap, {
    rv$log <- bind_rows(isolate(rv$log), tibble(
      participant = str_trim(isolate(input$pid)), trial_kind = "preference", chart = "Tree Map",
      year = rv$participant_year, trial_idx = NA_integer_, task_type = NA_character_, correct_answer = NA_character_,
      submitted = "Tree Map", correct = NA, seconds = NA_real_, timeout = NA,
      conf_treemap = NA_integer_, conf_bar = NA_integer_, conf_stack = NA_integer_, conf_line = NA_integer_, preference = "Tree Map"
    ))
    rv$screen <- "done"
  }, ignoreInit = TRUE)

  observeEvent(input$pref_bar, {
    rv$log <- bind_rows(isolate(rv$log), tibble(
      participant = str_trim(isolate(input$pid)), trial_kind = "preference", chart = "Bar Chart",
      year = rv$participant_year, trial_idx = NA_integer_, task_type = NA_character_, correct_answer = NA_character_,
      submitted = "Bar Chart", correct = NA, seconds = NA_real_, timeout = NA,
      conf_treemap = NA_integer_, conf_bar = NA_integer_, conf_stack = NA_integer_, conf_line = NA_integer_, preference = "Bar Chart"
    ))
    rv$screen <- "done"
  }, ignoreInit = TRUE)

  observeEvent(input$pref_stack, {
    rv$log <- bind_rows(isolate(rv$log), tibble(
      participant = str_trim(isolate(input$pid)), trial_kind = "preference", chart = "Stack Chart",
      year = rv$participant_year, trial_idx = NA_integer_, task_type = NA_character_, correct_answer = NA_character_,
      submitted = "Stack Chart", correct = NA, seconds = NA_real_, timeout = NA,
      conf_treemap = NA_integer_, conf_bar = NA_integer_, conf_stack = NA_integer_, conf_line = NA_integer_, preference = "Stack Chart"
    ))
    rv$screen <- "done"
  }, ignoreInit = TRUE)

  observeEvent(input$pref_line, {
    rv$log <- bind_rows(isolate(rv$log), tibble(
      participant = str_trim(isolate(input$pid)), trial_kind = "preference", chart = "Line Graph",
      year = rv$participant_year, trial_idx = NA_integer_, task_type = NA_character_, correct_answer = NA_character_,
      submitted = "Line Graph", correct = NA, seconds = NA_real_, timeout = NA,
      conf_treemap = NA_integer_, conf_bar = NA_integer_, conf_stack = NA_integer_, conf_line = NA_integer_, preference = "Line Graph"
    ))
    rv$screen <- "done"
  }, ignoreInit = TRUE)

  # Restart / Quit handlers (unchanged)
  observeEvent(input$restart, {
    rv$plan <- NULL
    rv$idx <- 0
    rv$participant_year <- NA_integer_
    rv$chart <- NA_character_
    rv$story_year <- NA_integer_
    rv$task_type <- NA_character_
    rv$choices <- NULL
    rv$prompt <- ""
    rv$start_time <- NULL
    rv$answered <- FALSE
    rv$screen <- "instructions"
  }, ignoreInit = TRUE)

  observeEvent(input$back_home, {
    rv$screen <- "instructions"
    rv$plan <- NULL
    rv$idx <- 0
    rv$participant_year <- NA_integer_
    rv$choices <- NULL
    rv$prompt <- ""
    rv$start_time <- NULL
    rv$answered <- FALSE
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
    p
  })

  # reactive dependency to prevent accidental double clicks
  observe({
    rv$answered
    invisible(NULL)
  })
}

shinyApp(ui, server)