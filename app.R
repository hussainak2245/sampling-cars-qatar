library(shiny)
library(tidyverse)
library(arrow)
library(plotly)
library(bslib)

# -----------------------------
# Load + prepare data
# -----------------------------
raw_df <- read_parquet("data.parquet") %>%
  select(-vehicle_flag) %>%
  filter(!is.na(model_year)) %>%
  mutate(
    model_year_num = as.integer(format(model_year, "%Y"))
  )

latest_year <- max(raw_df$model_year_num, na.rm = TRUE)

pop_df <- raw_df %>%
  filter(model_year_num >= latest_year - 4) %>%
  uncount(total)

N <- nrow(pop_df)

brand_choices <- sort(unique(pop_df$vehicle_manufacture))
default_brand <- ifelse("JET" %in% brand_choices, "JET", brand_choices[1])

# -----------------------------
# Sampling functions
# -----------------------------
sample_srs <- function(df, n) {
  slice_sample(df, n = min(n, nrow(df)))
}

sample_stratified <- function(df, n) {
  df %>%
    group_by(nationality_group) %>%
    slice_sample(prop = min(1, n / nrow(df))) %>%
    ungroup()
}

sample_cluster <- function(df, n) {
  clusters <- df %>%
    distinct(nationality_group, gender)
  
  selected_clusters <- clusters %>%
    slice_sample(n = min(3, nrow(clusters)))
  
  df %>%
    semi_join(selected_clusters, by = c("nationality_group", "gender")) %>%
    slice_sample(n = min(n, nrow(.)))
}

sample_systematic <- function(df, n) {
  df_sorted <- df %>%
    arrange(model_year_num, vehicle_manufacture)
  
  k <- floor(nrow(df_sorted) / n)
  
  if (k < 1) {
    return(df_sorted)
  }
  
  start <- sample(1:k, 1)
  
  df_sorted[seq(start, nrow(df_sorted), by = k), ] %>%
    slice_head(n = n)
}

sample_ranked_set <- function(df, n) {
  set_size <- 5
  cycles <- ceiling(n / set_size)
  
  map_dfr(1:cycles, function(i) {
    df %>%
      slice_sample(n = set_size) %>%
      arrange(desc(model_year_num)) %>%
      slice(sample(1:set_size, 1))
  }) %>%
    slice_head(n = n)
}

estimate_target <- function(sample_df, population_size, true_count) {
  p_hat <- mean(sample_df$is_target)
  
  tibble(
    estimate = round(p_hat * population_size),
    prop = p_hat,
    error = round((p_hat * population_size) - true_count)
  )
}

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  theme = bs_theme(
    bg = "#F6EFE4",
    fg = "#211A1A",
    primary = "#7A001E",
    base_font = font_google("Bree Serif"),
    heading_font = font_google("Bree Serif")
  ),
  
  tags$style(HTML("
    body {
      font-family: 'Bree Serif', serif;
      font-size: 13px;
    }

    .container-fluid {
      max-width: 1200px;
      padding-top: 24px;
    }

    .title-box {
      background: #7A001E;
      color: #F6EFE4;
      padding: 24px 26px 18px 26px;
      border-radius: 18px;
      margin-bottom: 16px;
    }

    .title-box h2 {
      margin: 0;
      font-size: 34px;
      font-weight: 700;
    }

    .title-box p {
      margin: 6px 0 0 0;
      font-size: 14px;
      opacity: 0.92;
    }

    .card {
      background: #FFF9F0;
      border: 1.5px solid #7A001E;
      border-radius: 14px;
      padding: 14px;
      margin-bottom: 12px;
      box-shadow: 1px 1px 6px rgba(0,0,0,0.07);
    }

    .metric-label {
      font-size: 13px;
      color: #5E4A4A;
      margin-bottom: 4px;
    }

    .metric-value {
      font-size: 28px;
      font-weight: bold;
      color: #7A001E;
      line-height: 1.1;
    }

    .small-note {
      font-size: 12px;
      color: #5E4A4A;
      line-height: 1.35;
      margin-top: 8px;
    }

    .btn {
      background-color: #7A001E !important;
      color: #F6EFE4 !important;
      border-radius: 10px;
      border: none;
      font-size: 13px;
      width: 100%;
    }

    .form-group {
      margin-bottom: 10px;
    }

    .irs--shiny .irs-bar,
    .irs--shiny .irs-single {
      background: #7A001E;
      border-color: #7A001E;
    }

    h4 {
      font-size: 19px;
      color: #7A001E;
      font-weight: 700;
      margin-top: 0;
      margin-bottom: 10px;
    }

    table {
      font-size: 12px;
    }
  ")),
  
  div(
    class = "title-box",
    h2("Sampling Scheme Simulator"),
    p("Data source: Qatar Open Data Portal"),
    p("Compare sampling designs by estimating the number of vehicles from a selected manufacturer in the last five model years.")
  ),
  
  fluidRow(
    column(
      width = 3,
      
      div(
        class = "card",
        h4("Controls"),
        
        selectInput(
          "brand",
          "Target manufacturer",
          choices = brand_choices,
          selected = default_brand
        ),
        
        sliderInput(
          "n",
          "Sample size",
          min = 50,
          max = 3000,
          value = 500,
          step = 50
        ),
        
        actionButton("run", "Run Simulation"),
        
        div(
          class = "small-note",
          "Each click redraws samples using five sampling schemes and compares their estimates with the true population value."
        )
      ),
      
      div(
        class = "card",
        h4("Sampling Designs"),
        div(class = "small-note",
            "Simple random: random vehicles from the full population."),
        div(class = "small-note",
            "Stratified: samples within nationality groups."),
        div(class = "small-note",
            "Cluster: samples selected gender-nationality clusters."),
        div(class = "small-note",
            "Systematic: samples every kth vehicle after sorting."),
        div(class = "small-note",
            "Ranked set: ranks small sets by model year before selection.")
      )
    ),
    
    column(
      width = 9,
      
      fluidRow(
        column(
          4,
          div(
            class = "card",
            div(class = "metric-label", "Population size"),
            div(class = "metric-value", textOutput("population_size"))
          )
        ),
        column(
          4,
          div(
            class = "card",
            div(class = "metric-label", "True target count"),
            div(class = "metric-value", textOutput("true_target_count"))
          )
        ),
        column(
          4,
          div(
            class = "card",
            div(class = "metric-label", "True target proportion"),
            div(class = "metric-value", textOutput("true_target_prop"))
          )
        )
      ),
      
      div(
        class = "card",
        h4("Estimated Count by Sampling Method"),
        plotlyOutput("estimate_plot", height = "360px")
      ),
      
      div(
        class = "card",
        h4("Numerical Results"),
        tableOutput("result_table")
      )
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output) {
  
  target_data <- reactive({
    pop_df %>%
      mutate(is_target = vehicle_manufacture == input$brand)
  })
  
  true_count <- reactive({
    sum(target_data()$is_target)
  })
  
  true_percentage <- reactive({
    true_count() / nrow(target_data())
  })
  
  output$population_size <- renderText({
    format(nrow(target_data()), big.mark = ",")
  })
  
  output$true_target_count <- renderText({
    format(true_count(), big.mark = ",")
  })
  
  output$true_target_prop <- renderText({
    paste0(round(true_percentage() * 100, 2), "%")
  })
  
  sim_results <- eventReactive(input$run, {
    
    df_target <- target_data()
    pop_size <- nrow(df_target)
    true_val <- true_count()
    
    methods <- list(
      "Simple Random Sampling" = sample_srs(df_target, input$n),
      "Stratified Sampling" = sample_stratified(df_target, input$n),
      "Cluster Sampling" = sample_cluster(df_target, input$n),
      "Systematic Sampling" = sample_systematic(df_target, input$n),
      "Ranked Set Sampling" = sample_ranked_set(df_target, input$n)
    )
    
    map_dfr(names(methods), function(method_name) {
      estimate_target(methods[[method_name]], pop_size, true_val) %>%
        mutate(
          method = method_name,
          sample_n = nrow(methods[[method_name]])
        )
    }) %>%
      select(method, sample_n, estimate, error, prop)
    
  }, ignoreNULL = FALSE)
  
  output$result_table <- renderTable({
    sim_results() %>%
      mutate(
        estimate = format(estimate, big.mark = ","),
        error = format(error, big.mark = ","),
        prop = paste0(round(prop * 100, 2), "%")
      )
  })
  
  output$estimate_plot <- renderPlotly({
    results <- sim_results()
    
    p <- ggplot(
      results,
      aes(
        x = reorder(method, estimate),
        y = estimate,
        text = paste(
          "Method:", method,
          "<br>Sample n:", sample_n,
          "<br>Estimate:", format(estimate, big.mark = ","),
          "<br>Error:", format(error, big.mark = ",")
        )
      )
    ) +
      geom_col(fill = "#7A001E", alpha = 0.84, width = 0.65) +
      geom_hline(
        yintercept = true_count(),
        linetype = "dashed",
        linewidth = 0.9,
        color = "#211A1A"
      ) +
      coord_flip() +
      labs(
        x = NULL,
        y = paste("Estimated", input$brand, "count")
      ) +
      theme_minimal(base_family = "Bree Serif", base_size = 12) +
      theme(
        plot.background = element_rect(fill = "#FFF9F0", color = NA),
        panel.background = element_rect(fill = "#FFF9F0", color = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "#211A1A", size = 11),
        axis.title.x = element_text(size = 12, margin = margin(t = 8)),
        plot.margin = margin(5, 5, 5, 5)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        font = list(family = "Bree Serif", size = 12),
        margin = list(l = 10, r = 10, t = 10, b = 10)
      )
  })
}

shinyApp(ui, server)