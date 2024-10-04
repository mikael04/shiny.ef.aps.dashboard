#' aux_lollipop_patchwork
#'
#' @description Uma função que auxiliará a unir os gráficos de entrada e saída em um único gráfico
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
func_aux_lollipop_patchwork <- function(list_graphs_inputs_outputs_2){
  # browser()
  if(F){
    list_graphs_inputs_outputs_2 <- initial_list_graphs_inputs_outputs
  }
  graph_input_1 <- list_graphs_inputs_outputs_2[[1]]
  graph_input_2 <- list_graphs_inputs_outputs_2[[2]]
  graph_arrow <- list_graphs_inputs_outputs_2[[3]]
  graph_lollipop_outputs <- list_graphs_inputs_outputs_2[[4]]

  ## Design antigo, inputs + arrow + outputs ----
  # design <- "AAABBCCCCC"
  # graph_inputs_outputs <- graph_lollipop_inputs + graph_arrow + graph_lollipop_outputs +
  #   plot_layout(design = design)

  # graph_lollipop_outputs <- graph_lollipop_outputs +
  #   theme(plot.title = ggtext::element_markdown(size = 18))
  # graph_lollipop_outputs
  # browser()
  ## Design novo, input_1 + input_2 + arrow + outputs ----
  design <- "ABCCDDDDD"
  graph_inputs_outputs <- graph_input_1 + graph_input_2 + graph_arrow + graph_lollipop_outputs +
    patchwork::plot_layout(design = design) +
    patchwork::plot_annotation(title="Entradas",
                               theme=theme(plot.title=element_text(hjust=0.1, vjust=-7.5)))
  # graph_inputs_outputs
  return(graph_inputs_outputs)
}
