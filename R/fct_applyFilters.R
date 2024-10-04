#' applyFilters
#'
#' @description Uma função que irá aplicar os filtros na tabela
#'
#' @return Retornando uma lista com o gráfico de eficiência, a tabela e o mapa
#'
#' @export
#'
#' @noRd
func_applyFilters <- function(
    tipo_quad, graph_type, title_ef_def,
    list_dfs, con, data_from_bd,
    input_seletor_ef, input_type, input_sel_period,
    input_sel_period_name, input_sel_uf_1, input_sel_uf_2,
    input_sel_reg_saude_1, input_sel_reg_saude_2,
    input_sel_mun_1, input_sel_mun_2,
    flag_cmp
    ## Não usado dessa forma senão for promise
    # func_server_mod_tabela_ef, func_order_by_ef_sel,
    # func_server_mod_mapa,
    # func_get_ef_mun_data,
    # func_server_mod_graph_lollipop_inputs_outputs,
    # func_aux_graph_lollipop_input_proc_1,
    # func_aux_graph_lollipop_input_proc_2,
    # func_aux_graph_lollipop_outputs_proc_2,
    # func_create_tooltip_ef
    ){
  # browser()
  # Formato novo, usando a seleção por quadrimestres, apenas calculando valores
  # para passar por parâmetro
  if(tipo_quad){
    sel_period <- as.numeric(input_sel_period)
    ## Para cada valor além de 1 em calc_ano, teremos um ano além do ano inicial (2022)
    ## Ou seja, calc_ano = 1, ano 2023, calc_ano = 2, ano 2024, etc...
    calc_ano <- ceiling(as.numeric(sel_period)/3)-1
    ano = 2022+calc_ano
    quad = sel_period-calc_ano*3
  }

  ## Buscando dados no banco de dados
  df_mun_cod_ibge_regsaude <- dplyr::tbl(con, "df_mun_cod_ibge_regsaude")
  ef_muns_proc <- dplyr::tbl(con, "ef_muns_proc")
  ef_muns_quad_proc <- dplyr::tbl(con, "ef_muns_quad_proc")
  ef_muns_res <- dplyr::tbl(con, "ef_muns_res")
  ef_muns_ano_res <- dplyr::tbl(con, "ef_muns_ano_res")

  if(data_from_bd){
    df_mun_cod_ibge_regsaude <- df_mun_cod_ibge_regsaude |>
      dplyr::select(-row.names)
    ef_muns_proc <- ef_muns_proc |>
      dplyr::select(-row.names)
    ef_muns_quad_proc <- ef_muns_quad_proc |>
      dplyr::select(-row.names)
    ef_muns_res <- ef_muns_res |>
      dplyr::select(-row.names)
    ef_muns_ano_res <- ef_muns_ano_res |>
      dplyr::select(-row.names)
  }

  ## Mudando conforme o seletor -> Se True, Resultados; Se False, Processos
  if(input_seletor_ef){
    # browser()
    tab_title_ef <- "Resultados"
    ef_proc_res <- F
    sel_period <-(sel_period-2022)*3+1
    ef_muns_period <- ef_muns_ano_res
    ef_muns_ef <- ef_muns_res
  }else{
    tab_title_ef <- "Processos"
    ef_proc_res <- T
    ef_muns_period <- ef_muns_quad_proc
    ef_muns_ef <- ef_muns_proc
  }

  ## Município ----
  if(input_type == "mun"){
    flag_brasil_mun <- F
    graph_type <- 2
    ## Filtrando dados do município selecionado
    df_mun_sel <- df_mun_cod_ibge_regsaude |>
      dplyr::filter(nome_uf == input_sel_uf_1, nome_mun == input_sel_mun_1) |>
      dplyr::select(cod_ibge, nome_mun, CO_REGSAUD) |>
      dplyr::collect()

    ### Comparação ----
    if(!flag_cmp){
      df_mun_cmp <- NULL
    }else{
      ## Dados do município de comparação
      df_mun_cmp <- df_mun_cod_ibge_regsaude |>
        dplyr::filter(nome_uf == input_sel_uf_2, nome_mun == input_sel_mun_2) |>
        dplyr::select(cod_ibge, nome_mun, CO_REGSAUD) |>
        dplyr::collect()
      ## Flag de comparação
      # flag_cmp <- T
    }
    # func_test_res <- func_test_if_has_data(df_mun_sel)
    ## Se não houver dados para o município selecionado
    if(!func_test_if_has_data(df_mun_sel)){
      return(list(NULL, NULL, NULL))
      ## Se houver, continuar função
    }else{
      ## Se o município de comparação existir e não houver dados
      if(flag_cmp && !func_test_if_has_data(df_mun_cmp)){
        return(list(NULL, NULL, NULL))
      }
      # browser()
      ## Buscando cod_ibge do município selecionado
      cod_ibge_mun_sel <- df_mun_sel |>
        dplyr::distinct(cod_ibge) |>
        dplyr::pull(cod_ibge)

      ## Buscando região de saúde do município selecionado
      reg_saude_mun_sel <- df_mun_sel |>
        dplyr::pull(CO_REGSAUD)

      ef_df_quad_sel <- ef_muns_period |>
        dplyr::filter(CO_REGSAUD == reg_saude_mun_sel) |>
        dplyr::collect()

      ### Buscando dados do município de comparação ----
      if(flag_cmp){
        cod_ibge_cmp <- unique(df_mun_cmp$cod_ibge)
        ef_df_cmp_quad_sel <- ef_muns_period |>
          dplyr::filter(cod_ibge == cod_ibge_cmp) |>
          dplyr::collect()

        ### Adicionando comparação
        ef_df_quad_sel <- rbind(ef_df_quad_sel, ef_df_cmp_quad_sel)
      }

      # browser()
      ef_df_quad_sel <- ef_df_quad_sel

      ## Tabela ----
      gt_tabela <- func_server_mod_tabela_ef(
        ef_df_quad_sel, sel_period, flag_brasil_mun, graph_type, cod_ibge_mun_sel,
        with_miniplot = F, ef_proc_res, tab_title_ef,
        tab_title_extra = "nos Municípios dentro da Região de saúde",
        flag_cmp, df_mun_cmp$cod_ibge
      )

      ## Mapa (da região de saúde) (ef_df_reg_saude_sel) ----
      ## Adicionando geometria

      ef_muns_ef <- ef_muns_ef |>
        dplyr::filter(quad_cod == sel_period) |>
        dplyr::collect()

      ## Adicionando SFs aos municípios para o mapa
      ef_muns_ef_sf <- ef_muns_ef |>
        dplyr::filter(CO_REGSAUD == reg_saude_mun_sel) |>
        dplyr::left_join(mun_sf, by=c("cod_ibge" = "cod")) |>
        sf::st_as_sf()

      ef_muns_ef_sf <- sf::st_transform(ef_muns_ef_sf, crs = '+proj=longlat
+datum=WGS84')

      ## Função que criará o mapa
      ggiraph_map <-func_server_mod_mapa(
        ef_muns_ef_sf, graph_type, cod_ibge_mun_sel, input_sel_mun_1,
        title_ef_def, sel_period, ef_proc_res)

      ## Eficiência (gráficos de Inputs e outputs) ----
      ## Definindo qual banco de eficiência será usado (processos ou resultados)
      ## Buscando dados do municipio selecionado
      ef_df_muns <- ef_muns_ef |>
        dplyr::filter(quad_cod == sel_period) |>
        dplyr::collect()
      ef_mun_sel <- func_get_ef_mun_data(ef_df_muns, cod_ibge_mun_sel)
      ## Buscando dados do municipio de comparação
      if(flag_cmp){
        ef_cmp <- func_get_ef_mun_data(ef_df_muns, cod_ibge_cmp)
      }else{
        ef_cmp <- NULL
      }
      # if(!func_test_if_has_data(ef_mun_ref)){
      #   mod_modal_sem_dados_server("modal_sem_dados_1", "O município de comparação",
      #                              "Por favor, selecione outro município para comparação")
      #   ## Se houver, continuar função
      # }else{
      # browser()
      # ## Gráficos de pirulito (entradas e saídas)
      # ### Inputs
      # mod_graph_lollipop_inputs_server("graph_lollipop_inputs_1", graph_type, sel_period, ef_mun_sel, flag_cmp = F, ef_df_cmp = NULL)
      # ### Outputs
      # mod_graph_lollipop_outputs_server("graph_lollipop_outputs_1", graph_type, sel_period, ef_mun_sel, flag_cmp = F, ef_df_cmp = NULL)

      ## Função que criará o gráfico do tipo lollipop de eficiências
      list_graphs_inputs_outputs <- func_server_mod_graph_lollipop_inputs_outputs(
        graph_type, input_sel_period_name, ef_mun_sel,
        flag_cmp, ef_cmp, ef_proc_res)
      # graph_inputs_outputs <- NULL
      # }
    }
  }

  # Região de saúde ----
  if(input_type == "reg_saude"){
    # browser()
    ## Flag para apresentar mapa e tabela de município
    flag_brasil_mun <- F
    graph_type <- 3

    df_rs_sel <- df_mun_cod_ibge_regsaude |>
      dplyr::filter(nome_uf == input_sel_uf_1, nome_rs == input_sel_reg_saude_1) |>
      dplyr::collect()

    ## Testa se existe dados para aquela região de saúde
    if(nrow(df_rs_sel) == 0){
      return(list(NULL, NULL, NULL))
    }else{
      # browser()
      ## Buscando cod_ibge dos municípios dentro da região de saúde
      cod_ibge_mun_sel <- df_rs_sel |>
        dplyr::distinct(cod_ibge) |>
        dplyr::pull(cod_ibge)

      ## Buscando região de saúde do município selecionado
      reg_saude_mun_sel <- df_rs_sel |>
        dplyr::pull(CO_REGSAUD)

      reg_saude_mun_sel <- reg_saude_mun_sel[1]

      ef_df_quad_sel <- ef_muns_period |>
        dplyr::filter(CO_REGSAUD == reg_saude_mun_sel) |>
        dplyr::collect()

      # browser()
      ## Tabela ----
      gt_tabela <- func_server_mod_tabela_ef(
        ef_df_quad_sel, sel_period, flag_brasil_mun, graph_type,
        cod_ibge_mun_sel, with_miniplot = F, ef_proc_res, tab_title_ef,
        tab_title_extra = "nos Municípios da Região de saúde selecionada",
        flag_cmp, df_mun_cmp$cod_ibge
      )

      ## Mapa (da região de saúde) (ef_df_reg_saude_sel) ----
      ## Adicionando geometria

      # browser()
      ef_rs_ef <- ef_muns_ef |>
        dplyr::filter(quad_cod == sel_period) |>
        dplyr::collect()

      ## Adicionando SFs aos municípios para o mapa
      ef_rs_ef_sf <- ef_rs_ef |>
        dplyr::filter(CO_REGSAUD == reg_saude_mun_sel) |>
        dplyr::left_join(mun_sf, by=c("cod_ibge" = "cod")) |>
        sf::st_as_sf()

      ef_rs_ef_sf <- sf::st_transform(ef_rs_ef_sf, crs = '+proj=longlat
+datum=WGS84')

      ## Função que criará o mapa
      ggiraph_map <-func_server_mod_mapa(
        ef_rs_ef_sf, graph_type, cod_ibge_mun_sel, input_sel_mun_1,
        title_ef_def, sel_period, ef_proc_res)

      # browser()
      list_graphs_inputs_outputs <- NULL
    }
  }

  # UF ----
  if(input_type == "uf"){
    # browser()
    ## Flag para apresentar mapa e tabela de município
    flag_brasil_mun <- F
    graph_type <- 1


    ef_df_quad_sel <- ef_muns_period |>
      dplyr::filter(nome_uf == input_sel_uf_1) |>
      dplyr::collect()

    df_uf_sel <- df_mun_cod_ibge_regsaude |>
      dplyr::filter(nome_uf == input_sel_uf_1) |>
      dplyr::collect()

    cod_ibge_mun_sel <- NULL

    # browser()
    ## Tabela ----
    gt_tabela <- func_server_mod_tabela_ef(
      ef_df_quad_sel, sel_period, flag_brasil_mun, graph_type,
      cod_ibge_mun_sel, with_miniplot = F, ef_proc_res, tab_title_ef,
      tab_title_extra = "nos Municípios da UF selecionada",
      flag_cmp, df_mun_cmp$cod_ibge
    )

    ## Mapa (da região de saúde) (ef_df_reg_saude_sel) ----
    ## Adicionando geometria

    # browser()
    ef_uf_ef <- ef_muns_ef |>
      dplyr::filter(quad_cod == sel_period) |>
      dplyr::collect()

    ## Adicionando SFs aos municípios para o mapa
    ef_uf_ef_sf <- ef_uf_ef |>
      dplyr::filter(nome_uf == input_sel_uf_1) |>
      dplyr::left_join(mun_sf, by=c("cod_ibge" = "cod")) |>
      sf::st_as_sf()

    ef_uf_ef_sf <- sf::st_transform(ef_uf_ef_sf, crs = '+proj=longlat
+datum=WGS84')

    ## Função que criará o mapa
    ggiraph_map <-func_server_mod_mapa(
      ef_uf_ef_sf, graph_type, cod_ibge_mun_sel, input_sel_mun_1,
      title_ef_def, sel_period, ef_proc_res)

    # browser()
    list_graphs_inputs_outputs <- NULL
  }
  list_tabela <- list(gt_tabela)
  list_mapa <- list(ggiraph_map)
  list_graph_ef <- list(list_graphs_inputs_outputs)
  # list_mapa <- list(NULL)
  # list_graph_ef <- list(NULL)
  return (list(list_tabela, list_mapa, list_graph_ef))
  # return(list(NULL))
}
