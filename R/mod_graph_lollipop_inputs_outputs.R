#' graph_lollipop_inputs_outputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_graph_lollipop_inputs_outputs_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinycssloaders::withSpinner(ggiraph::girafeOutput(ns("graph_lollipop_inputs_outputs")), type = 6, color = "white")
    # plotly::plotlyOutput(ns("graph_lollipop_inputs_outputs"))
  )
}

#' graph_lollipop_inputs_outputs Server Functions
#'
#' @noRd
mod_graph_lollipop_inputs_outputs_server <- function(id, initial_state, ef_proc_res, list_graphs_inputs_outputs, newcase){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    teste_interno <- F
    # browser()
    if(teste_interno){
      # ef_df <- data.table::fread(here::here("data-raw/ef_df.csv"))
      graph_type <- 0
      quad_sel <- 4
    }
    if(initial_state){
      ## ef_proc_res = T -> Eficiência de processos, F -> Eficiência de resultados
      if(ef_proc_res){
        graf_inputs_outputs <- ggiraph::girafe(ggobj = initial_graf_inputs_outputs_p,
                                               width_svg = 10, height_svg = 3)
      }else{
        graf_inputs_outputs <- ggiraph::girafe(ggobj = initial_graf_inputs_outputs_r,
                                               width_svg = 10, height_svg = 3)
      }
    }
    if(!initial_state){
      # browser()
      ## Se for NULL, significa que estamos vendo região de saúde ou UF, não apresenta gráfico de eficiência
      if(is.null(list_graphs_inputs_outputs)){
        graf_inputs_outputs <- NULL
        ## Caso contrário, caso seja selecionado o município, apresenta o gráfico de eficiência
      }else{
        graf_ggplot_inputs_outputs <- func_aux_lollipop_patchwork(list_graphs_inputs_outputs, newcase)
        graf_inputs_outputs <- ggiraph::girafe(ggobj = graf_ggplot_inputs_outputs,
                                               width_svg = 10, height_svg = 3)
      }
    }
    output$graph_lollipop_inputs_outputs <- ggiraph::renderGirafe({
      graf_inputs_outputs
    })
  })
}

## To be copied in the UI
# mod_graph_lollipop_inputs_outputs_ui("graph_lollipop_inputs_outputs_1")

## To be copied in the server
# mod_graph_lollipop_inputs_outputs_server("graph_lollipop_inputs_outputs_1")
