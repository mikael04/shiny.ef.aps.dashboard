#' init_dfs_iniciais
#'
#' @description Uma função que criará os dfs para os gráficos iniciais
#'
#' @return Os dfs para o mapa, tabela e gráfico de eficiência iniciais
#'
#' @noRd
func_init_dfs_iniciais <- function(ef_ufs, uf_sf, ef_ufs_quad, ef_br, sel_period,
                                   ef_proc_res){
  if(F){
    # ef_proc_res = T -> Processos; = F -> Resultados
    ef_proc_res <- T
  }
  # browser()

  var_ef <- "ef_BCC"
  if(ef_proc_res){
    title_ef <- "Processos"
  }else{
    title_ef <- "Resultados"
  }

  ## Mapa ----
  df_mapa_inicial_ufs_sf <- ef_ufs |>
    dplyr::filter(quad_cod == sel_period) |>
    dplyr::left_join(uf_sf, by = c("CO_UF" = "cod_stt")) |>
    sf::st_as_sf() |>
    sf::st_transform(crs = '+proj=longlat
+datum=WGS84')

  ## Tabela ----
  ## Selecionando os quadrimestres atuais
  if(ef_proc_res){
    df_tabela_quad_sel <- ef_ufs_quad |>
      dplyr::select(tidyr::ends_with(paste0("processos_", sel_period))) |>
      dplyr::rename(ef_BCC = 1)
  }else{
    df_tabela_quad_sel <- ef_ufs_quad |>
      dplyr::select(tidyr::ends_with(paste0("resultados_", sel_period))) |>
      dplyr::rename(ef_BCC = 1)

  }

  ## Unindo novamente com a base, para termos o valor atual separado
  df_tabela <- dplyr::bind_cols(ef_ufs_quad, df_tabela_quad_sel) |>
    dplyr::select(-CO_UF, UF = nome_uf)

  ## Ordenar pela eficiência selecionada
  df_tabela_inicial_ufs <- func_order_by_ef_sel(df_tabela, ef_proc_res)

  return(list(df_mapa_inicial_ufs_sf, df_tabela_inicial_ufs))

}
