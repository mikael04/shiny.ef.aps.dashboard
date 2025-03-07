#' grafs_iniciais
#'
#' @description Uma função que criará os gráficos iniciais dependendo de alguns parâmetros
#' newcase -> Se é o novo caso onde temos apenas um input no gráfico de eficiência
#'
#' @return Os gráficos criados, o mapa, a tabela e o gráfico de eficiência
#'
#' @noRd
func_init_grafs <- function(df_mapa_inicial_ufs_sf, df_tabela_inicial_ufs, ef_br, sel_period,
                            ef_proc_res, newcase){
  if(F){
    # ef_proc_res = T -> Processos; = F -> Resultados
    ef_proc_res <- T
    sel_period <- sel_period_proc
    df_mapa_inicial_ufs_sf <- df_mapa_inicial_ufs_sf_proc
    df_tabela_inicial_ufs <- df_tabela_inicial_ufs_proc
    ef_br <- ef_br_proc
    ## Resultados
    ef_proc_res <- F
    sel_period <- sel_period_res
    df_mapa_inicial_ufs_sf <- df_mapa_inicial_ufs_sf_res
    df_tabela_inicial_ufs <- df_tabela_inicial_ufs_res
    ef_br <- ef_br_res

  }

  var_ef <- "ef_BCC"
  if(ef_proc_res){
    title_ef <- "Processos"
    input_sel_period_name <- "3° Quad/2023"
    with_mini_plot <- T
  }else{
    title_ef <- "Resultados"
    input_sel_period_name <- "2023"
    with_mini_plot <- F
  }
  ### Criando mapa inicial
  initial_map <-func_server_mod_mapa(
    df_mapa_inicial_ufs_sf, map_type = 0, mun_sel = 0, nome_mun_sel = NULL,
    title_ef = title_ef, sel_period = 6, ef_proc_res)

  ## Tabela ----

  ## Ordenar pela eficiência selecionada
  df_tabela_inicial_ufs <- func_order_by_ef_sel(df_tabela_inicial_ufs, ef_proc_res)

  initial_gt_tabela <- func_server_mod_tabela_ef(
    df_tabela_inicial_ufs, sel_period, flag_brasil_mun = T, graph_type = 0, mun_sel = 0,
    with_miniplot = with_mini_plot, ef_proc_res, tab_title_ef = title_ef,
    tab_title_extra = "nas UFs",
    flag_cmp = F, mun_cmp = 0
  )

  ## Gráfico de eficiência ----
  ef_br_f <- ef_br |>
    dplyr::filter(quad_cod == sel_period)

  ## A eficiência é calculada apenas uma vez, portanto caso estejamos calculando a eficiência de resultados
  ## para os gráficos iniciais, nós apenas não calcularemos

  initial_list_graphs_inputs_outputs <- func_server_mod_graph_lollipop_inputs_outputs(
    graph_type = 0, input_sel_period_name, ef_br_f,
    flag_cmp = F, ef_df_cmp = F, ef_proc_res)
  graf_inicial_inputs_outputs <- func_aux_lollipop_patchwork(initial_list_graphs_inputs_outputs, newcase)

  return(list(initial_map, initial_gt_tabela, graf_inicial_inputs_outputs))

}
