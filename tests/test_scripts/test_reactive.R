# app.R
library(shiny)
library(dplyr)
library(ggplot2)
library(future)
library(bslib)
library(promises)
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
      input_task_button("btn", "Redo graphs"),
    ),

    mainPanel(
      # Output plots
      plotOutput("sales_trend"),
      plotOutput("sales_by_region")
    )
  )
)

server <- function(input, output) {
  debug <- F
  new_data <- eventReactive(input$btn, {
    if(debug){
      data <- sales_data

      # Apply date filter
      data <- data %>%
        filter(date >= input$date_range[1],
               date <= input$date_range[2])

      # Apply product filter
      if (input$product != "All") {
        data <- data %>% filter(product == input$product)
      }

      # Apply region filter
      if (input$region != "All") {
        data <- data %>% filter(region == input$region)
      }
    }else{
      browser()
      data <- new_data_promise$invoke(sales_data, input$date_range[1], input$date_range[2],
                                      input$product, input$region)
      browser()
    }
    data
  }) # This makes it run once at startup
  # Reactive dataset that only updates when button is clicked

  filtered_data <- eventReactive(new_data(), {
    browser()
    if(debug){
      new_data <- new_data()
    }else{
      new_data <- new_data_promise$result()
    }
    new_data
  })

  ## Future
  new_data_promise <- ExtendedTask$new(
    function(data, date_range_1, date_range_2, product, region){
      browser()
      # Apply date filter
      data_ <- data %>%
        filter(date >= date_range_1,
               date <= date_range_2)

      # Apply product filter
      if (product != "All") {
        data_ <- data_ %>% filter(product == product)
      }

      # Apply region filter
      if (region != "All") {
        data_ <- data_ %>% filter(region == region)
      }
      future_promise({
        # Apply date filter
        data <- data %>%
          filter(date >= date_range_1,
                 date <= date_range_2)

        # Apply product filter
        if (product != "All") {
          data <- data %>% filter(product == product)
        }

        # Apply region filter
        if (region != "All") {
          data <- data %>% filter(region == region)
        }
        data
      })
    }) |> bind_task_button("btn")
  # Sales trend plot
  output$sales_trend <- renderPlot({
    browser()
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
