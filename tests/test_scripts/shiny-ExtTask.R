library(shiny)
library(bslib)
library(future)
library(promises)
source("../R/fct_check_has_data.R")
future::plan(multisession)

# func_sum <- function(x, y){
#   x + y
# }

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

  sum_values <- ExtendedTask$new(
    # function(x, y) {
    function(
    df, uf_sel, mun_regsaud_sel, type, ef_proc_res,
    func_check_has_data
    ) {

    future_promise({
      func_check_has_data(df, uf_sel,
                          mun_regsaud_sel, type, ef_proc_res)
      # func_sum(input$x, input$y)
    })
  }) |> bind_task_button("btn")

  observeEvent(input$btn, {
    # sum_values$invoke(input$x, input$y)
    browser()
    df_dados_mun_uf_reg_saud_filter <- readRDS(file = "../data/database_data/df_dados_mun_uf_reg_saud_filter.rds")
    input_sel_uf_1 <- "Acre"
    input_sel_mun_1 <- "Rio Branco"
    input_type <- "mun"
    input_seletor_ef <- TRUE
    # func_check_has_data(df_dados_mun_uf_reg_saud_filter, input_sel_uf_1,
    #                     input_sel_mun_1, input_type, input_seletor_ef)
    sum_values$invoke(df_dados_mun_uf_reg_saud_filter, input_sel_uf_1,
                      input_sel_mun_1, input_type, input_seletor_ef,
                      func_check_has_data)
  })

  output$sum <- renderText({
    browser()
    sum_values$result()
  })
}

shinyApp(ui, server)

