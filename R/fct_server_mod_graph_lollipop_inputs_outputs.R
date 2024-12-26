#' server_mod_graph_lollipop_inputs_outputs
#'
#' @description Uma função para calcular o gráfico gerado para a função de módulo do lolipop
#' entrada e saída juntas
#'
#' @return Uma função do tipo ggiraph com patchwork
#'
#' @noRd
func_server_mod_graph_lollipop_inputs_outputs <- function(
    graph_type, input_sel_period_name, ef_df,
    flag_cmp, ef_df_cmp, ef_proc_res){
  # browser()
  ## Versão antiga, usando o retorno de lista em uma única função
  ## Gerencia os tipos de gráficos de inputs
  # list_graph_lollipop_inputs <- func_aux_graph_lollipop_inputs(graph_type, quad_sel, ef_df,
  #                                                              flag_cmp = F,
  #                                                              ef_df_cmp = NULL)
  # graph_input_1 <- list_graph_lollipop_inputs[[1]]
  # graph_input_2 <- list_graph_lollipop_inputs[[2]]
  ## Versão nova, usando uma função por gráfico
  # browser()
  ## Se true, eficiencia de processos, se false, eficiencia de resultados

  ## Testa true, roda ef de processos
  if(ef_proc_res){
    browser()
    ## Gerencia os tipos de gráficos de inputs
    graph_input_1 <- func_aux_graph_lollipop_input_proc_1(graph_type, input_sel_period_name, ef_df,
                                                          flag_cmp,
                                                          ef_df_cmp, ef_proc_res)
    ## Não temos mais segundo input
    # graph_input_2 <- func_aux_graph_lollipop_input_proc_2(graph_type, input_sel_period_name, ef_df,
    #                                                       flag_cmp,
    #                                                       ef_df_cmp, ef_proc_res)

    ## Gerencia os tipos de gráficos de outputs
    graph_lollipop_outputs <- func_aux_graph_lollipop_outputs_proc(graph_type, input_sel_period_name, ef_df,
                                                                     flag_cmp, ef_df_cmp, ef_proc_res)

  }
  ## Testa false, roda ef de resultados
  if(!ef_proc_res){
    ## Gerencia os tipos de gráficos de inputs
    graph_input_1 <- func_aux_graph_lollipop_input_res_1(graph_type, input_sel_period_name, ef_df,
                                                         flag_cmp,
                                                         ef_df_cmp, ef_proc_res)
    # graph_input_2 <- func_aux_graph_lollipop_input_res_2(graph_type, input_sel_period_name, ef_df,
    #                                                      flag_cmp,
    #                                                      ef_df_cmp, ef_proc_res)

    ## Gerencia os tipos de gráficos de outputs
    graph_lollipop_outputs <- func_aux_graph_lollipop_outputs_res(graph_type, input_sel_period_name, ef_df,
                                                                  flag_cmp, ef_df_cmp, ef_proc_res)
  }

  df_arrow <- data.frame(lineend = "round",
                         linejoin = "bevel",
                         y = 7)
  graph_arrow <- ggplot(df_arrow, aes(x = 1, y = y, xend = 2, yend = y)) +
    geom_segment(
      size = 1, arrow = arrow(length = unit(3, "mm"), ends = "last"), arrow.fill = "black",
      color = "#0260A6"
    ) +
    geom_text(aes(x = 1, y = y, label = "Eficiência"),
              hjust = -0.40, vjust = -0.4, size = 5, color = "#014c84") +
    theme_void()
  # graph_arrow

  # graph_inputs_outputs
  # browser()
  ## Retornando uma lista com os gráficos para montagem no mod_graph_lollipop
  return(list(graph_input_1, graph_arrow, graph_lollipop_outputs))
}
