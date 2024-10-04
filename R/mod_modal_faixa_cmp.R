#' modal_faixa_cmp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_modal_faixa_cmp_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' modal_faixa_cmp Server Functions
#'
#' @noRd
mod_modal_faixa_cmp_server <- function(id, title, text){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    showModal(modalDialog(
      title = title,
      HTML(text),
      footer = modalButton("Fechar"),
      easyClose = TRUE
    ))
  })
}

## To be copied in the UI
# mod_modal_faixa_cmp_ui("modal_faixa_cmp_1")

## To be copied in the server
# mod_modal_faixa_cmp_server("modal_faixa_cmp_1")
