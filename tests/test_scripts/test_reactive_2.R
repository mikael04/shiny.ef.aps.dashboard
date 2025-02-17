library(shiny)
library(bslib)
library(future)
library(promises)
future::plan(multisession)

ui <- page_fluid(
  p("The time is ", textOutput("current_time", inline=TRUE)),
  hr(),
  numericInput("x", "x", value = 1),
  numericInput("y", "y", value = 2),
  input_task_button("btn", "Add numbers"),
  textOutput("sum")
)

server <- function(input, output, session) {
  output$current_time <- renderText({
    invalidateLater(1000)
    format(Sys.time(), "%H:%M:%S %p")
  })
  sum_values <- reactiveVal(0)

  # sum_values <- ExtendedTask$new(function(x, y) {
  #
  # }) |> bind_task_button("btn")

  observeEvent(input$btn, {
    future_promise({
      # Sys.sleep(5)
      x + y
    }) %...>% sum_values()
    # value(5)
  })

  # Initialize the filtered data on app start
  observe({
    if (is.null(sum_values())) {
      sum_values(sales_data)
    }
  })

  output$sum <- renderText({
    value()
  })
}

shinyApp(ui, server)
