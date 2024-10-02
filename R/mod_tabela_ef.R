#' tabela_ef UI Function
#'
#' @description Módulo para gerar tabela
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tabela_ef_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinycssloaders::withSpinner(gt::gt_output(ns("tabela")), type = 6, color = "white")
  )
}

#' tabela_ef Server Functions
#'
#' @noRd
mod_tabela_ef_server <- function(id, initial_state, gt_tabela, ef_proc_res){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    teste_interno <- F
    if(teste_interno){
      df_tabela <- ef_df_ufs
    }
    # browser()
    if(initial_state){
      ## ef_proc_res = T -> Eficiência de resultados, F -> Eficiência de processos
      if(ef_proc_res){
        gt_tabela <- initial_gt_tabela_r
      }else{
        gt_tabela <- initial_gt_tabela_p
      }
    }
    if(!initial_state){
      gt_tabela <- gt_tabela
    }

    ## Tabela de saída ----
    output$tabela <- gt::render_gt(expr = gt_tabela)
  })
}

## To be copied in the UI
# mod_tabela_ef_ui("tabela_ef_1")

## To be copied in the server
# mod_tabela_ef_server("tabela_ef_1")
