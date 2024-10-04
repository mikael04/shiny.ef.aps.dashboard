#' reset_graphs
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
func_reset_graphs <- function(session, output, initial_state, ef_proc_res){
  ## Mapa do brasil
  mod_mapa_server("mapa_1", initial_state, ggiraph_map = NULL, ef_proc_res)
  ## Tabela de eficiência das UFs
  mod_tabela_ef_server("tabela_ef_1", initial_state, gt_tabela = NULL, ef_proc_res)
  ### Inputs e outputs
  mod_graph_lollipop_inputs_outputs_server("graph_lollipop_inputs_outputs_1",
                                           initial_state, ef_proc_res, list_graphs_inputs_outputs = NULL)
  output$box_graf_ef <- renderUI({
    div(
      id = "graf-ef",
      class="row",
      div(class= "col-12",
          bslib::card(
            id = "cardInputsOutputs",
            class = "ui-cards",
            fill = F,
            max_height = "55vh",
            uiOutput("title_ef"),
            # ## Gráfico de inputs e outputs
            mod_graph_lollipop_inputs_outputs_ui("graph_lollipop_inputs_outputs_1"),
            HTML('
                        <div class="legenda">
                          <span class="dot dot1"></span> <span class="nomelegenda"> Município eficiente</span>
                          <span class="dot dot2"></span> <span class="nomelegenda"> Valor para 100% de eficiência</span>
                          <span class="dot dot3"></span> <span class="nomelegenda"> Valor do município selecionado</span>
                          <span class="dot dot4"></span> <span class="nomelegenda"> Valor município de comparação</span>
                        </div>
                      ')
          )
      )
    )
  })
}
