# app.R
# NYPD Hate Crimes — Charts-only
# Bar + Treemap + Heatmap (Category x Year) + Line (one line per Year across Categories)
#
# Requirements: put NYPD_Hate_Crimes_20260128.xlsx in same folder
# install.packages(c("shiny","readxl","dplyr","stringr","plotly","tibble","tidyr"))

library(shiny)
library(readxl)
library(dplyr)
library(stringr)
library(plotly)
library(tibble)
library(tidyr)

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
  mutate(
    ym_date = suppressWarnings(as.Date(sprintf("%04d-%02d-01", year, month)))
  ) %>%
  filter(!is.na(year), !is.na(month))

years <- sort(unique(df$year))
cats  <- sort(unique(df$offense_category))
ROOT_LABEL <- "All Hate Crimes"

# ---------- UI ----------
ui <- fluidPage(
  titlePanel("NYPD Hate Crimes — Interactive Charts (Offense Category vs Incidents)"),
  sidebarLayout(
    sidebarPanel(
      h4("Conditions"),
      selectInput("cond_year", "Year (condition):", choices = c("All", years), selected = "All"),
      selectInput("category", "Category (Offense Category):", choices = c("All", cats), selected = "All"),
      hr(),
      helpText("Charts are interactive: hover, legend toggle, zoom/pan. ",
               "Bar shows category counts, Treemap shows relative sizes, ",
               "Heatmap shows Category × Year, Line shows counts across categories by year.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Bar (Category counts)",
                 plotlyOutput("bar_plot", height = "720px")
        ),
        tabPanel("Treemap (Category → Bias Motive)",
                 plotlyOutput("treemap", height = "720px")
        ),
        tabPanel("Heatmap: Category x Year",
                 plotlyOutput("stack_plot", height = "720px")
        ),
        tabPanel("Line: one line per Year (across Categories)",
                 plotlyOutput("line_plot", height = "720px")
        )
      )
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {

  filtered <- reactive({
    x <- df
    if (input$cond_year != "All") x <- x %>% filter(year == as.integer(input$cond_year))
    if (input$category  != "All") x <- x %>% filter(offense_category == input$category)
    x
  })

  # -------- Bar chart: Category counts --------
  output$bar_plot <- renderPlotly({
    x <- filtered()
    if (nrow(x) == 0) return(plot_ly() %>% layout(title = "No data for selected year/category"))

    counts <- x %>% count(offense_category, name = "n") %>% arrange(desc(n))

    plot_ly(
      data = counts,
      x = ~reorder(offense_category, n),
      y = ~n,
      type = "bar",
      hovertemplate = "<b>%{x}</b><br>Incidents: %{y}<extra></extra>"
    ) %>%
      layout(
        title = "Incidents by Offense Category",
        xaxis = list(title = "", tickangle = -45),
        yaxis = list(title = "Incidents"),
        margin = list(b = 160)
      )
  })

  # -------- Treemap: Category -> Bias Motive --------
  output$treemap <- renderPlotly({
    x <- filtered()
    if (nrow(x) == 0) return(plot_ly() %>% layout(title = "No data for selected year/category"))

    counts <- x %>% count(offense_category, bias_motive, name = "n")
    offcats <- counts %>% group_by(offense_category) %>% summarise(n = sum(n), .groups = "drop")

    nodes <- bind_rows(
      tibble(ids = "root", parents = "", labels = ROOT_LABEL, values = sum(counts$n), level = "root"),
      tibble(ids = paste0("off:", offcats$offense_category), parents = "root",
             labels = offcats$offense_category, values = offcats$n, level = "offcat"),
      tibble(ids = paste0("bias:", counts$offense_category, "||", counts$bias_motive),
             parents = paste0("off:", counts$offense_category),
             labels = counts$bias_motive, values = counts$n, level = "bias")
    )

    node_colors <- ifelse(nodes$level == "root", "#BDBDBD",
                          ifelse(nodes$level == "offcat", "#64B5F6", "#81C784"))

    plot_ly(
      data = nodes,
      type = "treemap",
      ids = ~ids,
      labels = ~labels,
      parents = ~parents,
      values = ~values,
      marker = list(colors = node_colors, line = list(width = 1, color = "white")),
      textinfo = "label+value+percent parent",
      hovertemplate = "<b>%{label}</b><br>Count: %{value}<br>%{percentParent:.1%} of parent<extra></extra>"
    ) %>% layout(margin = list(t = 10, l = 0, r = 0, b = 0))
  })

  # -------- Heatmap: Category on X, Year on Y --------
  output$stack_plot <- renderPlotly({
    x <- filtered()
    if (nrow(x) == 0) return(plot_ly() %>% layout(title = "No data for selected year/category"))

    counts <- x %>%
      count(year, offense_category, name = "n") %>%
      complete(year = sort(unique(df$year)), offense_category = sort(unique(df$offense_category)), fill = list(n = 0)) %>%
      arrange(year, offense_category)

    # Optional: limit categories to top K for readability (uncomment if needed)
    # K <- 30
    # top_cats <- df %>% count(offense_category) %>% arrange(desc(n)) %>% slice_head(n = K) %>% pull(offense_category)
    # counts <- counts %>% mutate(offense_category = ifelse(offense_category %in% top_cats, offense_category, "Other")) %>%
    #   group_by(year, offense_category) %>% summarise(n = sum(n), .groups = "drop")

    counts_wide <- counts %>%
      pivot_wider(names_from = offense_category, values_from = n, values_fill = 0)

    # reorder columns: first is year, remaining are categories
    years_order <- sort(unique(counts$year))
    cats_order  <- colnames(counts_wide)[-1]

    zmat <- as.matrix(counts_wide[ , -1, drop = FALSE])
    yvals <- as.character(counts_wide[[1]])
    xvals <- cats_order

    plot_ly(
      x = xvals,
      y = yvals,
      z = zmat,
      type = "heatmap",
      colorscale = "Viridis",
      hovertemplate = paste(
        "<b>Category:</b> %{x}<br>",
        "<b>Year:</b> %{y}<br>",
        "<b>Incidents:</b> %{z}<extra></extra>"
      )
    ) %>%
      layout(
        title = "Heatmap: Incidents (Category × Year)",
        xaxis = list(title = "Offense Category", tickangle = -45),
        yaxis = list(title = "Year")
      )
  })

  # -------- Line: one line per Year across Categories --------
  output$line_plot <- renderPlotly({
    x <- filtered()
    if (nrow(x) == 0) return(plot_ly() %>% layout(title = "No data for selected year/category"))

    counts <- x %>%
      count(offense_category, year, name = "n") %>%
      complete(year = sort(unique(df$year)), offense_category = sort(unique(df$offense_category)), fill = list(n = 0)) %>%
      arrange(year, offense_category)

    # order categories by overall count to prioritize top categories
    cat_order <- df %>% count(offense_category) %>% arrange(desc(n)) %>% pull(offense_category)

    # cap categories for readability in the line chart
    MAX_CATS <- 25
    if (length(cat_order) > MAX_CATS) {
      keep_cats <- cat_order[1:MAX_CATS]
      counts <- counts %>% filter(offense_category %in% keep_cats)
      cat_order <- keep_cats
    }

    years_present <- sort(unique(counts$year))
    plt <- plot_ly()

    for (yr in years_present) {
      dsub <- counts %>% filter(year == yr) %>%
        arrange(factor(offense_category, levels = cat_order))
      plt <- add_trace(plt,
                       x = dsub$offense_category,
                       y = dsub$n,
                       type = "scatter",
                       mode = "lines+markers",
                       name = as.character(yr),
                       hovertemplate = paste0("<b>Year:</b> ", yr, "<br><b>Category:</b> %{x}<br><b>Count:</b> %{y}<extra></extra>")
      )
    }

    plt %>%
      layout(
        title = "Counts by Category (one line per Year)",
        xaxis = list(title = "Offense Category", tickangle = -45),
        yaxis = list(title = "Incidents"),
        legend = list(title = list(text = "<b>Year</b>"))
      )
  })

}

shinyApp(ui, server)