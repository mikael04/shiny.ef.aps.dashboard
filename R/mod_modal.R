#' modal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_modal_ui <- function(id){
  ns <- NS(id)
  tagList(
  )
}

#' modal_sem_dados Server Functions
#'
#' @noRd
mod_modal_server <- function(id, title, first_test, mid_text, end_text){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    showModal(modalDialog(
      title = title,
      HTML(class="span", paste0(first_test, mid_text, end_text)),
      footer = modalButton("Fechar"),
      easyClose = TRUE
    ))
  })
}

## To be copied in the UI
# mod_modal_ui("modal_1")

## To be copied in the server
# mod_modal_server("modal_1")
