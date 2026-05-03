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
  mutate(model_year_num = as.integer(format(model_year, "%Y")))

latest_year <- max(raw_df$model_year_num, na.rm = TRUE)

pop_df <- raw_df %>%
  filter(model_year_num >= latest_year - 4) %>%
  uncount(total)

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
  df_sorted <- df %>% arrange(model_year_num, vehicle_manufacture)
  k <- floor(nrow(df_sorted) / n)
  
  if (k < 1) return(df_sorted)
  
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

estimate_target <- function(sample_df, population_size) {
  mean(sample_df$is_target) * population_size
}

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bg = "#F6EFE4",
    fg = "#211A1A",
    primary = "#7A001E",
    secondary = "#C8B7A6",
    success = "#7A001E",
    info = "#7A001E",
    warning = "#A66A00",
    danger = "#7A001E",
    base_font = font_google("Bree Serif"),
    heading_font = font_google("Bree Serif")
  ),
  
  tags$style(HTML("
    body {
      font-family: 'Bree Serif', serif;
      font-size: 13px;
      background-color: #F6EFE4;
      color: #211A1A;
    }

    .container-fluid {
      max-width: 1250px;
      padding-top: 24px;
      padding-bottom: 24px;
    }

    .title-box {
      background: #7A001E;
      color: #F6EFE4;
      padding: 22px 26px 18px 26px;
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
      opacity: 0.95;
    }

    .card-box {
      background: #FFF9F0;
      border: 1.5px solid #7A001E;
      border-radius: 14px;
      padding: 14px 18px;
      margin-bottom: 12px;
      box-shadow: 1px 1px 6px rgba(0,0,0,0.06);
    }

    .metric-label {
      font-size: 13px;
      color: #5E4A4A;
      margin-bottom: 4px;
    }

    .metric-value {
      font-size: 26px;
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

    .btn,
    .btn-default,
    .btn-primary {
      background-color: #7A001E !important;
      color: #F6EFE4 !important;
      border: 1px solid #7A001E !important;
      border-radius: 10px;
      font-size: 13px;
      width: 100%;
    }

    .btn:hover,
    .btn-default:hover,
    .btn-primary:hover {
      background-color: #5C0017 !important;
      color: #F6EFE4 !important;
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
      width: 100%;
      color: #211A1A;
    }

    th {
      color: #7A001E;
    }

    .form-label,
    label,
    .control-label {
      color: #211A1A;
      font-size: 13px;
    }
  ")),
  
  div(
    class = "title-box",
    h2("Sampling Convergence Simulator"),
    p("Data source: Qatar Open Data Portal"),
    p("Repeatedly draw samples and watch whether each sampling design stabilizes near the true manufacturer count.")
  ),
  
  fluidRow(
    column(
      width = 3,
      
      div(
        class = "card-box",
        h4("Controls"),
        
        selectInput(
          "brand",
          "Target manufacturer",
          choices = brand_choices,
          selected = default_brand
        ),
        
        sliderInput(
          "sample_n",
          "Sample size per simulation",
          min = 50,
          max = 3000,
          value = 500,
          step = 50
        ),
        
        sliderInput(
          "sim_n",
          "Number of simulations",
          min = 10,
          max = 500,
          value = 100,
          step = 10
        ),
        
        actionButton("run", "Run Simulation"),
        
        div(
          class = "small-note",
          "The line is the cumulative average estimate. The dashed horizontal line is the true population count."
        )
      ),
      
      div(
        class = "card-box",
        h4("Reading the Plot"),
        div(class = "small-note", "Convergence: the line moves toward the dashed true value."),
        div(class = "small-note", "Variability: jagged lines mean unstable estimates."),
        div(class = "small-note", "Bias: a line staying far from the dashed value suggests systematic error.")
      )
    ),
    
    column(
      width = 9,
      
      fluidRow(
        column(
          4,
          div(
            class = "card-box",
            div(class = "metric-label", "Population size"),
            div(class = "metric-value", textOutput("population_size"))
          )
        ),
        column(
          4,
          div(
            class = "card-box",
            div(class = "metric-label", "True target count"),
            div(class = "metric-value", textOutput("true_target_count"))
          )
        ),
        column(
          4,
          div(
            class = "card-box",
            div(class = "metric-label", "True target proportion"),
            div(class = "metric-value", textOutput("true_target_prop"))
          )
        )
      ),
      
      div(
        class = "card-box",
        h4("Convergence of Average Estimate"),
        plotlyOutput("convergence_plot", height = "420px")
      ),
      
      div(
        class = "card-box",
        h4("Final Simulation Summary"),
        tableOutput("summary_table")
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
  
  simulation_results <- eventReactive(input$run, {
    
    df_target <- target_data()
    pop_size <- nrow(df_target)
    true_val <- true_count()
    
    method_functions <- list(
      "Simple Random" = sample_srs,
      "Stratified" = sample_stratified,
      "Cluster" = sample_cluster,
      "Systematic" = sample_systematic,
      "Ranked Set" = sample_ranked_set
    )
    
    withProgress(message = "Running simulations...", value = 0, {
      map_dfr(seq_along(method_functions), function(j) {
        method_name <- names(method_functions)[j]
        method_fun <- method_functions[[j]]
        
        estimates <- map_dbl(1:input$sim_n, function(i) {
          incProgress(1 / (length(method_functions) * input$sim_n))
          sample_df <- method_fun(df_target, input$sample_n)
          estimate_target(sample_df, pop_size)
        })
        
        tibble(
          method = method_name,
          simulation = 1:input$sim_n,
          estimate = estimates,
          cumulative_mean = cummean(estimates),
          error = cumulative_mean - true_val
        )
      })
    })
  }, ignoreNULL = FALSE)
  
  output$convergence_plot <- renderPlotly({
    results <- simulation_results()
    true_val <- true_count()
    y_min <- min(c(results$cumulative_mean, true_val), na.rm = TRUE)
    y_max <- max(c(results$cumulative_mean, true_val), na.rm = TRUE)
    pad <- max((y_max - y_min) * 0.12, 1)
    
    p <- ggplot(
      results,
      aes(
        x = simulation,
        y = cumulative_mean,
        color = method,
        group = method,
        text = paste0(
          "Method: ", method,
          "<br>Simulation: ", simulation,
          "<br>Cumulative mean: ", format(round(cumulative_mean), big.mark = ","),
          "<br>Error: ", format(round(error), big.mark = ",")
        )
      )
    ) +
      geom_line(linewidth = 0.9, alpha = 0.95) +
      geom_hline(
        yintercept = true_val,
        linetype = "dashed",
        linewidth = 0.9,
        color = "#211A1A"
      ) +
      scale_color_manual(
        values = c(
          "Simple Random" = "#7A001E",
          "Stratified" = "#1F6F78",
          "Cluster" = "#C87941",
          "Systematic" = "#6B4E71",
          "Ranked Set" = "#5F7A3A"
        )
      ) +
      scale_y_continuous(
        limits = c(y_min - pad, y_max + pad),
        labels = scales::comma
      ) +
      labs(
        x = "Number of simulations",
        y = paste("Average estimated", input$brand, "count"),
        color = NULL
      ) +
      theme_minimal(base_family = "Bree Serif", base_size = 13) +
      theme(
        plot.background = element_rect(fill = "#FFF9F0", color = NA),
        panel.background = element_rect(fill = "#FFF9F0", color = NA),
        panel.grid.major = element_line(color = "#E8DCCB", linewidth = 0.35),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(color = "#211A1A", size = 11),
        axis.text = element_text(color = "#211A1A", size = 11),
        axis.title = element_text(color = "#211A1A", size = 13),
        plot.margin = margin(6, 8, 6, 6)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        paper_bgcolor = "#FFF9F0",
        plot_bgcolor = "#FFF9F0",
        font = list(family = "Bree Serif", size = 13, color = "#211A1A"),
        hoverlabel = list(
          bgcolor = "#FFF9F0",
          bordercolor = "#7A001E",
          font = list(family = "Bree Serif", size = 13, color = "#211A1A")
        ),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.22
        ),
        margin = list(l = 70, r = 25, t = 10, b = 90)
      ) %>%
      config(
        displayModeBar = FALSE,
        responsive = TRUE
      )
  })
  
  output$summary_table <- renderTable({
    simulation_results() %>%
      group_by(method) %>%
      summarise(
        final_average_estimate = round(last(cumulative_mean)),
        final_error = round(last(cumulative_mean) - true_count()),
        mean_absolute_error = round(mean(abs(estimate - true_count()))),
        sd_of_estimates = round(sd(estimate)),
        .groups = "drop"
      ) %>%
      mutate(
        final_average_estimate = format(final_average_estimate, big.mark = ","),
        final_error = format(final_error, big.mark = ","),
        mean_absolute_error = format(mean_absolute_error, big.mark = ","),
        sd_of_estimates = format(sd_of_estimates, big.mark = ",")
      )
  })
}

shinyApp(ui, server)
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
  mutate(model_year_num = as.integer(format(model_year, "%Y")))

latest_year <- max(raw_df$model_year_num, na.rm = TRUE)

pop_df <- raw_df %>%
  filter(model_year_num >= latest_year - 4) %>%
  uncount(total)

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
  df_sorted <- df %>% arrange(model_year_num, vehicle_manufacture)
  k <- floor(nrow(df_sorted) / n)
  
  if (k < 1) return(df_sorted)
  
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

estimate_target <- function(sample_df, population_size) {
  mean(sample_df$is_target) * population_size
}

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bg = "#F6EFE4",
    fg = "#211A1A",
    primary = "#7A001E",
    secondary = "#C8B7A6",
    success = "#7A001E",
    info = "#7A001E",
    warning = "#A66A00",
    danger = "#7A001E",
    base_font = font_google("Bree Serif"),
    heading_font = font_google("Bree Serif")
  ),
  
  tags$style(HTML("
    body {
      font-family: 'Bree Serif', serif;
      font-size: 13px;
      background-color: #F6EFE4;
      color: #211A1A;
    }

    .container-fluid {
      max-width: 1250px;
      padding-top: 24px;
      padding-bottom: 24px;
    }

    .title-box {
      background: #7A001E;
      color: #F6EFE4;
      padding: 22px 26px 18px 26px;
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
      opacity: 0.95;
    }

    .card-box {
      background: #FFF9F0;
      border: 1.5px solid #7A001E;
      border-radius: 14px;
      padding: 14px 18px;
      margin-bottom: 12px;
      box-shadow: 1px 1px 6px rgba(0,0,0,0.06);
    }

    .metric-label {
      font-size: 13px;
      color: #5E4A4A;
      margin-bottom: 4px;
    }

    .metric-value {
      font-size: 26px;
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

    .btn,
    .btn-default,
    .btn-primary {
      background-color: #7A001E !important;
      color: #F6EFE4 !important;
      border: 1px solid #7A001E !important;
      border-radius: 10px;
      font-size: 13px;
      width: 100%;
    }

    .btn:hover,
    .btn-default:hover,
    .btn-primary:hover {
      background-color: #5C0017 !important;
      color: #F6EFE4 !important;
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
      width: 100%;
      color: #211A1A;
    }

    th {
      color: #7A001E;
    }

    .form-label,
    label,
    .control-label {
      color: #211A1A;
      font-size: 13px;
    }
  ")),
  
  div(
    class = "title-box",
    h2("Sampling Convergence Simulator"),
    p("Data source: Qatar Open Data Portal"),
    p("Repeatedly draw samples and watch whether each sampling design stabilizes near the true manufacturer count.")
  ),
  
  fluidRow(
    column(
      width = 3,
      
      div(
        class = "card-box",
        h4("Controls"),
        
        selectInput(
          "brand",
          "Target manufacturer",
          choices = brand_choices,
          selected = default_brand
        ),
        
        sliderInput(
          "sample_n",
          "Sample size per simulation",
          min = 50,
          max = 3000,
          value = 500,
          step = 50
        ),
        
        sliderInput(
          "sim_n",
          "Number of simulations",
          min = 10,
          max = 500,
          value = 100,
          step = 10
        ),
        
        actionButton("run", "Run Simulation"),
        
        div(
          class = "small-note",
          "The line is the cumulative average estimate. The dashed horizontal line is the true population count."
        )
      ),
      
      div(
        class = "card-box",
        h4("Reading the Plot"),
        div(class = "small-note", "Convergence: the line moves toward the dashed true value."),
        div(class = "small-note", "Variability: jagged lines mean unstable estimates."),
        div(class = "small-note", "Bias: a line staying far from the dashed value suggests systematic error.")
      )
    ),
    
    column(
      width = 9,
      
      fluidRow(
        column(
          4,
          div(
            class = "card-box",
            div(class = "metric-label", "Population size"),
            div(class = "metric-value", textOutput("population_size"))
          )
        ),
        column(
          4,
          div(
            class = "card-box",
            div(class = "metric-label", "True target count"),
            div(class = "metric-value", textOutput("true_target_count"))
          )
        ),
        column(
          4,
          div(
            class = "card-box",
            div(class = "metric-label", "True target proportion"),
            div(class = "metric-value", textOutput("true_target_prop"))
          )
        )
      ),
      
      div(
        class = "card-box",
        h4("Convergence of Average Estimate"),
        plotlyOutput("convergence_plot", height = "420px")
      ),
      
      div(
        class = "card-box",
        h4("Final Simulation Summary"),
        tableOutput("summary_table")
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
  
  simulation_results <- eventReactive(input$run, {
    
    df_target <- target_data()
    pop_size <- nrow(df_target)
    true_val <- true_count()
    
    method_functions <- list(
      "Simple Random" = sample_srs,
      "Stratified" = sample_stratified,
      "Cluster" = sample_cluster,
      "Systematic" = sample_systematic,
      "Ranked Set" = sample_ranked_set
    )
    
    withProgress(message = "Running simulations...", value = 0, {
      map_dfr(seq_along(method_functions), function(j) {
        method_name <- names(method_functions)[j]
        method_fun <- method_functions[[j]]
        
        estimates <- map_dbl(1:input$sim_n, function(i) {
          incProgress(1 / (length(method_functions) * input$sim_n))
          sample_df <- method_fun(df_target, input$sample_n)
          estimate_target(sample_df, pop_size)
        })
        
        tibble(
          method = method_name,
          simulation = 1:input$sim_n,
          estimate = estimates,
          cumulative_mean = cummean(estimates),
          error = cumulative_mean - true_val
        )
      })
    })
  }, ignoreNULL = FALSE)
  
  output$convergence_plot <- renderPlotly({
    results <- simulation_results()
    true_val <- true_count()
    y_min <- min(c(results$cumulative_mean, true_val), na.rm = TRUE)
    y_max <- max(c(results$cumulative_mean, true_val), na.rm = TRUE)
    pad <- max((y_max - y_min) * 0.12, 1)
    
    p <- ggplot(
      results,
      aes(
        x = simulation,
        y = cumulative_mean,
        color = method,
        group = method,
        text = paste0(
          "Method: ", method,
          "<br>Simulation: ", simulation,
          "<br>Cumulative mean: ", format(round(cumulative_mean), big.mark = ","),
          "<br>Error: ", format(round(error), big.mark = ",")
        )
      )
    ) +
      geom_line(linewidth = 0.9, alpha = 0.95) +
      geom_hline(
        yintercept = true_val,
        linetype = "dashed",
        linewidth = 0.9,
        color = "#211A1A"
      ) +
      scale_color_manual(
        values = c(
          "Simple Random" = "#7A001E",
          "Stratified" = "#1F6F78",
          "Cluster" = "#C87941",
          "Systematic" = "#6B4E71",
          "Ranked Set" = "#5F7A3A"
        )
      ) +
      scale_y_continuous(
        limits = c(y_min - pad, y_max + pad),
        labels = scales::comma
      ) +
      labs(
        x = "Number of simulations",
        y = paste("Average estimated", input$brand, "count"),
        color = NULL
      ) +
      theme_minimal(base_family = "Bree Serif", base_size = 13) +
      theme(
        plot.background = element_rect(fill = "#FFF9F0", color = NA),
        panel.background = element_rect(fill = "#FFF9F0", color = NA),
        panel.grid.major = element_line(color = "#E8DCCB", linewidth = 0.35),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(color = "#211A1A", size = 11),
        axis.text = element_text(color = "#211A1A", size = 11),
        axis.title = element_text(color = "#211A1A", size = 13),
        plot.margin = margin(6, 8, 6, 6)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        paper_bgcolor = "#FFF9F0",
        plot_bgcolor = "#FFF9F0",
        font = list(family = "Bree Serif", size = 13, color = "#211A1A"),
        hoverlabel = list(
          bgcolor = "#FFF9F0",
          bordercolor = "#7A001E",
          font = list(family = "Bree Serif", size = 13, color = "#211A1A")
        ),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.22
        ),
        margin = list(l = 70, r = 25, t = 10, b = 90)
      ) %>%
      config(
        displayModeBar = FALSE,
        responsive = TRUE
      )
  })
  
  output$summary_table <- renderTable({
    simulation_results() %>%
      group_by(method) %>%
      summarise(
        final_average_estimate = round(last(cumulative_mean)),
        final_error = round(last(cumulative_mean) - true_count()),
        mean_absolute_error = round(mean(abs(estimate - true_count()))),
        sd_of_estimates = round(sd(estimate)),
        .groups = "drop"
      ) %>%
      mutate(
        final_average_estimate = format(final_average_estimate, big.mark = ","),
        final_error = format(final_error, big.mark = ","),
        mean_absolute_error = format(mean_absolute_error, big.mark = ","),
        sd_of_estimates = format(sd_of_estimates, big.mark = ",")
      )
  })
}

shinyApp(ui, server)
