# app.R
library(shiny)
library(dplyr)
library(ggplot2)
library(future)
library(bslib)
library(promises)
source(here::here("R/utils_db.R"))
future::plan("multisession")

# Sample dataset
sales_data <- data.frame(
  date = seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day"),
  product = sample(c("Product A", "Product B", "Product C"), 366, replace = TRUE),
  region = sample(c("North", "South", "East", "West"), 366, replace = TRUE),
  sales = runif(366, 1000, 5000)
)

ui <- fluidPage(
  titlePanel("Sales Dashboard"),

  sidebarLayout(
    sidebarPanel(
      # Date range input
      dateRangeInput("date_range",
                     "Select Date Range:",
                     start = min(sales_data$date),
                     end = max(sales_data$date)),

      # Product selection
      selectInput("product",
                  "Select Product:",
                  choices = c("All", unique(sales_data$product))),

      # Region selection
      selectInput("region",
                  "Select Region:",
                  choices = c("All", unique(sales_data$region))),

      # Update button
      actionButton("btn", "Redo graphs", class = "btn-primary"),
    ),

    mainPanel(
      # Output plots
      plotOutput("sales_trend"),
      plotOutput("sales_by_region")
    )
  )
)

server <- function(input, output) {
  # Create a reactive value to store the filtered data
  filtered_data <- reactiveVal()

  # Observe the button click and update the filtered data
  observeEvent(input$btn, {
    input_date_range_1 <- input$date_range[1]
    input_date_range_2 <- input$date_range[2]
    input_product <- input$product
    input_region <- input$region
    con <- utils_db_connect()
    df_mun_cod_ibge_regsaude <- dplyr::tbl(con, "df_mun_cod_ibge_regsaude") |> dplyr::collect()

    # Create a future promise for the data filtering
    future_promise({
      data <- sales_data

      df_mun_cod_ibge_regsaude_ <- df_mun_cod_ibge_regsaude |>
        dplyr::filter(nome_rs == "Zona da Mata")

      # Apply date filter
      data <- data %>%
        filter(date >= input_date_range_1,
               date <= input_date_range_2)

      # Apply product filter
      if (input_product != "All") {
        data <- data %>% filter(product == input_product)
      }

      # Apply region filter
      if (input_region != "All") {
        data <- data %>% filter(region == input_region)
      }
      ggplot(data, aes(x = date, y = sales)) +
        geom_line(color = "blue") +
        geom_smooth(method = "loess", se = FALSE, color = "red") +
        labs(title = "Sales Trend Over Time",
             x = "Date",
             y = "Sales") +
        theme_minimal()

      data
    }) %...>% filtered_data()
  })

  # Initialize the filtered data on app start
  observe({
    if (is.null(filtered_data())) {
      filtered_data(sales_data)
    }
  })

  # Sales trend plot
  output$sales_trend <- renderPlot({
    req(filtered_data())

    ggplot(filtered_data(), aes(x = date, y = sales)) +
      geom_line(color = "blue") +
      geom_smooth(method = "loess", se = FALSE, color = "red") +
      labs(title = "Sales Trend Over Time",
           x = "Date",
           y = "Sales") +
      theme_minimal()
  })

  # Sales by region plot
  output$sales_by_region <- renderPlot({
    req(filtered_data())

    filtered_data() %>%
      group_by(region) %>%
      summarise(total_sales = sum(sales)) %>%
      ggplot(aes(x = region, y = total_sales, fill = region)) +
      geom_bar(stat = "identity") +
      labs(title = "Total Sales by Region",
           x = "Region",
           y = "Total Sales") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
