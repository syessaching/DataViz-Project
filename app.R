# app.R
library(shiny)
library(readxl)
library(dplyr)
library(stringr)
library(plotly)
library(tibble)
library(ggplot2)

xlsx_path <- "NYPD_Hate_Crimes_20260128.xlsx"  # change path if needed

# ---------- Load data ----------
raw <- read_excel(xlsx_path, col_types = "text")
names(raw) <- str_trim(names(raw))

# Columns from your file (exact names)
COL_ID     <- "Full Complaint ID"
COL_YEAR   <- "Complaint Year Number"
COL_MONTH  <- "Month Number"
COL_PBORO  <- "Patrol Borough Name"
COL_COUNTY <- "County"
COL_OFFCAT <- "Offense Category"
COL_BIAS   <- "Bias Motive Description"
COL_PD     <- "PD Code Description"

needed <- c(COL_ID, COL_YEAR, COL_MONTH, COL_PBORO, COL_OFFCAT, COL_BIAS)
missing <- setdiff(needed, names(raw))
if (length(missing) > 0) stop("Missing columns: ", paste(missing, collapse = ", "))

df <- raw %>%
  transmute(
    complaint_id = .data[[COL_ID]],
    year = suppressWarnings(as.integer(.data[[COL_YEAR]])),
    month = suppressWarnings(as.integer(.data[[COL_MONTH]])),
    patrol_boro = ifelse(is.na(.data[[COL_PBORO]]) | .data[[COL_PBORO]] == "", "Unknown", .data[[COL_PBORO]]),
    county = if (COL_COUNTY %in% names(raw)) ifelse(is.na(.data[[COL_COUNTY]]) | .data[[COL_COUNTY]] == "", "Unknown", .data[[COL_COUNTY]]) else NA_character_,
    offense_category = ifelse(is.na(.data[[COL_OFFCAT]]) | .data[[COL_OFFCAT]] == "", "Unknown", .data[[COL_OFFCAT]]),
    bias_motive = ifelse(is.na(.data[[COL_BIAS]]) | .data[[COL_BIAS]] == "", "Unknown", .data[[COL_BIAS]]),
    pd_code = if (COL_PD %in% names(raw)) ifelse(is.na(.data[[COL_PD]]) | .data[[COL_PD]] == "", "Unknown", .data[[COL_PD]]) else NA_character_
  ) %>%
  mutate(
    # safe Year-Month date for time plots (use 1st of month)
    ym_date = suppressWarnings(as.Date(sprintf("%04d-%02d-01", year, month)))
  )

# ---------- UI ----------
ui <- fluidPage(
  titlePanel("NYPD Hate Crimes — Explorer (Treemap + Bar + Stack + Line)"),
  sidebarLayout(
    sidebarPanel(
      h4("Filters"),
      selectInput("year", "Year:", choices = c("All", sort(na.omit(unique(df$year)))), selected = "All"),
      selectInput("month", "Month:", choices = c("All", sort(na.omit(unique(df$month)))), selected = "All"),
      selectInput("pboro", "Patrol Borough:", choices = c("All", sort(unique(df$patrol_boro))), selected = "All"),
      selectInput("offcat", "Offense Category:", choices = c("All", sort(unique(df$offense_category))), selected = "All"),
      hr(),
      textInput("search", "Search (highlights bias/category):", ""),
      actionButton("go", "Highlight"),
      actionButton("clear", "Clear"),
      hr(),
      h4("Summary"),
      verbatimTextOutput("summary"),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Treemap (Hierarchy)",
                 plotlyOutput("treemap", height = "720px"),
                 hr(),
                 h4("Clicked selection (sample rows)"),
                 tableOutput("table_treemap")
        ),
        tabPanel("Bar chart (Interactive)",
                 plotlyOutput("barchart", height = "720px"),
                 hr(),
                 h4("Clicked bias motive (sample rows)"),
                 tableOutput("table_bar")
        ),
        tabPanel("Stack graph (Over time)",
                 plotlyOutput("stack_plot", height = "720px")
        ),
        tabPanel("Line graph (Over time)",
                 plotlyOutput("line_plot", height = "720px")
        )
      ),
      width = 9
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    highlight = "",
    clicked_treemap = NULL,
    clicked_bar = NULL
  )
  
  observeEvent(input$go, {
    rv$highlight <- tolower(str_trim(input$search))
  })
  observeEvent(input$clear, {
    rv$highlight <- ""
    updateTextInput(session, "search", value = "")
  })
  
  filtered <- reactive({
    x <- df
    if (input$year != "All")  x <- x %>% filter(year == as.integer(input$year))
    if (input$month != "All") x <- x %>% filter(month == as.integer(input$month))
    if (input$pboro != "All") x <- x %>% filter(patrol_boro == input$pboro)
    if (input$offcat != "All") x <- x %>% filter(offense_category == input$offcat)
    x
  })
  
  output$summary <- renderPrint({
    x <- filtered()
    cat("Incidents shown:", nrow(x), "\n")
    if (nrow(x) > 0) {
      top_bias <- x %>% count(bias_motive, name = "n") %>% arrange(desc(n)) %>% slice_head(n = 1)
      top_cat  <- x %>% count(offense_category, name = "n") %>% arrange(desc(n)) %>% slice_head(n = 1)
      cat("Top bias motive:", top_bias$bias_motive, " (", top_bias$n, ")\n", sep = "")
      cat("Top offense category:", top_cat$offense_category, " (", top_cat$n, ")\n", sep = "")
    }
    cat("\nHow to use:\n- Treemap shows hierarchy (Offense Category → Bias Motive)\n- Bar compares top bias motives\n- Stack graph shows stacked categories over time\n- Line graph shows total incidents over time\n")
  })
  
  # ---------------- Treemap (Hierarchy) ----------------
  treemap_nodes <- reactive({
    x <- filtered()
    counts <- x %>% count(offense_category, bias_motive, name = "n")
    
    q <- rv$highlight
    counts <- counts %>%
      mutate(highlight = q != "" & (str_detect(tolower(bias_motive), fixed(q)) |
                                      str_detect(tolower(offense_category), fixed(q))))
    
    root_id <- "root"
    offcats <- counts %>% group_by(offense_category) %>% summarise(n = sum(n), .groups = "drop")
    
    bind_rows(
      tibble(ids = root_id, parents = "", labels = "All Hate Crimes", values = sum(counts$n), level = "root", highlight = FALSE),
      tibble(ids = paste0("off:", offcats$offense_category), parents = root_id,
             labels = offcats$offense_category, values = offcats$n, level = "offcat", highlight = FALSE),
      tibble(
        ids = paste0("bias:", counts$offense_category, "||", counts$bias_motive),
        parents = paste0("off:", counts$offense_category),
        labels = counts$bias_motive,
        values = counts$n,
        level = "bias",
        highlight = counts$highlight
      )
    )
  })
  
  output$treemap <- renderPlotly({
    nodes <- treemap_nodes()
    
    node_colors <- ifelse(nodes$level == "root", "#BDBDBD",
                          ifelse(nodes$level == "offcat", "#64B5F6",
                                 ifelse(nodes$highlight, "#FF1744", "#81C784")))
    
    plot_ly(
      data = nodes,
      type = "treemap",
      ids = ~ids,
      labels = ~labels,
      parents = ~parents,
      values = ~values,
      marker = list(colors = node_colors, line = list(width = 1, color = "white")),
      hovertemplate = "<b>%{label}</b><br>Count: %{value}<extra></extra>"
    ) %>%
      layout(margin = list(t = 10, l = 0, r = 0, b = 0))
  })
  
  observeEvent(event_data("plotly_click"), {
    click <- event_data("plotly_click")
    if (!is.null(click$label)) rv$clicked_treemap <- click$label
  })
  
  output$table_treemap <- renderTable({
    x <- filtered()
    label <- rv$clicked_treemap
    
    cols_show <- c("year","month","patrol_boro","county","offense_category","bias_motive","pd_code","complaint_id")
    cols_show <- cols_show[cols_show %in% names(x)]
    
    if (is.null(label) || label == "All Hate Crimes") return(head(x %>% select(all_of(cols_show)), 20))
    if (label %in% unique(x$offense_category)) return(head((x %>% filter(offense_category == label)) %>% select(all_of(cols_show)), 30))
    head((x %>% filter(bias_motive == label)) %>% select(all_of(cols_show)), 30)
  })
  
  # ---------------- Bar chart (Interactive) ----------------
  output$barchart <- renderPlotly({
    x <- filtered()
    top_bias <- x %>% count(bias_motive, name = "n") %>% arrange(desc(n)) %>% slice_head(n = 20)
    
    plot_ly(
      data = top_bias,
      x = ~reorder(bias_motive, n),
      y = ~n,
      type = "bar",
      hovertemplate = "<b>%{x}</b><br>Count: %{y}<extra></extra>"
    ) %>%
      layout(
        title = "Top 20 Bias Motives (Filtered)",
        xaxis = list(title = "", tickangle = 30),
        yaxis = list(title = "Incidents"),
        margin = list(b = 160)
      )
  })
  
  observeEvent(event_data("plotly_click"), {
    click <- event_data("plotly_click")
    if (!is.null(click$x)) rv$clicked_bar <- as.character(click$x)
  })
  
  output$table_bar <- renderTable({
    x <- filtered()
    label <- rv$clicked_bar
    
    cols_show <- c("year","month","patrol_boro","county","offense_category","bias_motive","pd_code","complaint_id")
    cols_show <- cols_show[cols_show %in% names(x)]
    
    if (is.null(label)) return(head(x %>% select(all_of(cols_show)), 20))
    head((x %>% filter(bias_motive == label)) %>% select(all_of(cols_show)), 30)
  })
  
  # ---------------- Stack graph (Stacked area) ----------------
  output$stack_plot <- renderPlotly({
    x <- filtered() %>% filter(!is.na(ym_date))
    if (nrow(x) == 0) {
      p <- ggplot() + theme_minimal() + annotate("text", x = 0.5, y = 0.5, label = "No data after filters", size = 6)
      return(ggplotly(p))
    }
    
    # Top 6 offense categories (others -> "Other")
    top_cats <- x %>% count(offense_category, name = "n") %>% arrange(desc(n)) %>% slice_head(n = 6) %>% pull(offense_category)
    x2 <- x %>% mutate(cat2 = ifelse(offense_category %in% top_cats, offense_category, "Other"))
    
    trend <- x2 %>%
      count(ym_date, cat2, name = "n") %>%
      arrange(ym_date)
    
    p <- ggplot(trend, aes(x = ym_date, y = n, fill = cat2)) +
      geom_area(alpha = 0.9) +
      labs(
        title = "Stacked Area: Incidents Over Time by Offense Category (Top 6 + Other)",
        x = "Month",
        y = "Incidents",
        fill = "Offense Category"
      ) +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # ---------------- Line graph (Over time: total incidents) ----------------
  output$line_plot <- renderPlotly({
    x <- filtered() %>% filter(!is.na(ym_date))
    trend <- x %>% count(ym_date, name = "n") %>% arrange(ym_date)
    
    p <- ggplot(trend, aes(x = ym_date, y = n)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(
        title = "Incidents Over Time (Year-Month)",
        x = "Month",
        y = "Incidents"
      ) +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
}

shinyApp(ui, server)