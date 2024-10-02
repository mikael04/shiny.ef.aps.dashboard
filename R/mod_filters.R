#' filters UI Function
#'
#' @description A shiny module that create the filters part of the UI
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_filters_ui <- function(id){
  ns <- NS(id)
  tagList(
    ## Cabeçalho de seleção padrão ----
    selectInput("type", "Buscar por:",
                c("Selecione um tipo de busca" = "no_sel",
                  "Município" = "mun",
                  "UF" = "uf",
                  # "Região" = "regiao",
                  "Região de saúde" = "reg_saude"
                  # "Macro Região de saúde" = "macro_reg_saude"
                ),
                width = "100%"
    ),
    ## Duas Uis (seletores) que serão dinâmicos conforme o tipo selecionado por "type"
    uiOutput("selector_type_1_uf_sel"),
    uiOutput("selector_type_1_mun_regs_sel"),
    uiOutput("cmp_button"),
    uiOutput("selector_type_2_uf_sel"),
    uiOutput("selector_type_2_mun_regs_sel"),
    # uiOutput("selector_type_3")
  )
}

#' filters Server Functions
#'
#' @noRd
mod_filters_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_filters_ui("filters_1")

## To be copied in the server
# mod_filters_server("filters_1")
