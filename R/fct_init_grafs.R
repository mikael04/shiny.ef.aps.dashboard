#' grafs_iniciais
#'
#' @description Uma função que criará os gráficos iniciais dependendo de alguns parâmetros
#'
#' @return Os gráficos criados, o mapa, a tabela e o gráfico de eficiência
#'
#' @noRd
func_init_grafs <- function(df_mapa_inicial_ufs_sf, df_tabela_inicial_ufs, ef_br, sel_period,
                            ef_proc_res){
  if(F){
    # ef_proc_res = T -> Processos; = F -> Resultados
    ef_proc_res <- F
    sel_period <- sel_period_res
    df_mapa_inicial_ufs_sf <- df_mapa_inicial_ufs_res_sf
    df_tabela_inicial_ufs <- df_tabela_inicial_res_ufs
  }

  var_ef <- "ef_BCC"
  if(ef_proc_res){
    title_ef <- "Processos"
    input_sel_period_name <- "3° Quad/2023"
  }else{
    title_ef <- "Resultados"
    input_sel_period_name <- "2023"
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
    with_miniplot = T, ef_proc_res, tab_title_ef = title_ef,
    tab_title_extra = "nas UFs",
    flag_cmp = F, mun_cmp = 0
  )

  ## Gráfico de eficiência ----
  ef_br_f <- ef_br |>
    dplyr::filter(quad_cod == sel_period)

  initial_list_graphs_inputs_outputs <- func_server_mod_graph_lollipop_inputs_outputs(
    graph_type = 0, input_sel_period_name, ef_br_f,
    flag_cmp = F, ef_df_cmp = F, ef_proc_res)
  graf_inicial_inputs_outputs <- func_aux_lollipop_patchwork(initial_list_graphs_inputs_outputs)

  return(list(initial_map, initial_gt_tabela, graf_inicial_inputs_outputs))

}
