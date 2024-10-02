#' mapa UI Function
#'
#' @description Um módulo que criará o mapa do painel
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mapa_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinycssloaders::withSpinner(ggiraph::girafeOutput(ns("mapa_1")), type = 6, color = "white")
  )
}

#' mapa Server Functions
#'
#' @noRd
mod_mapa_server <- function(id, initial_state, ggiraph_map, ef_proc_res){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # browser()
    if(initial_state){
      ## ef_proc_res = T -> Eficiência de processos, F -> Eficiência de resultados
      if(ef_proc_res){
        ggiraph_map <- initial_map_p
      }else{
        ggiraph_map <- initial_map_r
      }
    }
    if(!initial_state){
      ## Mapa recebido da promise
      ggiraph_map <- ggiraph_map
    }
    # browser()
    ## Apenas o output do mapa, o resto é feito na promise
    output$mapa_1 <- ggiraph::renderGirafe({
      ggiraph_map
    })
  })
}

## To be copied in the UI
# mod_mapa_ui("mapa_1")

## To be copied in the server
# mod_mapa_server("mapa_1")
