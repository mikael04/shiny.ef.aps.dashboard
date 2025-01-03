#' server_mod_tabela_ef
#'
#' @description Uma função para calcular a tabela gerado para a função de módulo da tabela
#' tabela de eficiências
#'
#' @return A tabela do tipo gt usada
#'
#' @noRd
func_server_mod_tabela_ef <- function(
    df_tabela, sel_period, flag_brasil_mun, graph_type, mun_sel,
    with_miniplot, ef_proc_res, tab_title_ef, tab_title_extra,
    flag_cmp, mun_cmp){
  if(F){
    df_tabela <-df_tabela_inicial_ufs
    sel_period <- 6
    flag_brasil_mun <- T
    graph_type <- 0
    mun_sel <- 0
    with_miniplot <- F
    ef_proc_res <- T
    tab_title_ef = "Processos"
    tab_title_extra = "nas UFs"
    flag_cmp = F
    mun_cmp = 0
  }
  ## Se flag_brasil, mapa inicial dos estados do país, se F, mapa do município
  flag_brasil <- ifelse(flag_brasil_mun, T, F)
  flag_mun <- ifelse(flag_brasil_mun, F, T)
  type_ef <- ifelse(ef_proc_res, "processos", "resultados")
  title_period <- ifelse(ef_proc_res, "quadrimestre", "ano")
  # browser()
  ## Com miniplot ----
  if(with_miniplot){
    ### Brasil ----
    ## Tabela do Brasil, já pré-processada
    if(flag_brasil){
      df_tabela <- df_tabela
    }
    ### Município ----
    ## Tabela do município
    if(flag_mun){
      # browser()
      ## Selecionando os período mais atuais
      if(ef_proc_res){
        df_tabela_quad_sel <- df_tabela |>
          dplyr::select(tidyr::ends_with(paste0("processos_", sel_period))) |>
          dplyr::rename(ef_BCC = 1)
      }else{
        df_tabela_quad_sel <- df_tabela |>
          dplyr::select(tidyr::ends_with(paste0("resultados_", sel_period))) |>
          dplyr::rename(ef_BCC = 1)
      }

      ## Unindo novamente com a base, para termos o valor atual separado
      df_tabela <- dplyr::bind_cols(df_tabela, df_tabela_quad_sel) |>
        dplyr::select(-CO_REGSAUD, `Município` = nome_mun)

      ## Ordenar pela eficiência selecionada
      df_tabela <- func_order_by_ef_sel(df_tabela, ef_proc_res)
    }
  }
  ## Sem miniplot ----
  if(!with_miniplot){
    if(flag_brasil){
      df_tabela <- df_tabela |>
        dplyr::select(UF, tidyr::ends_with(paste0(type_ef, "_", sel_period))) |>
        dplyr::rename(UF = 1, ef_BCC = 2)
    }
    if(flag_mun){
      df_tabela_quad_sel <- df_tabela |>
        dplyr::select(tidyr::ends_with(paste0(type_ef, "_", sel_period))) |>
        dplyr::select(ef_BCC = 1)

      ## Unindo novamente com a base, para termos o valor atual separado
      df_tabela <- dplyr::bind_cols(df_tabela, df_tabela_quad_sel) |>
        dplyr::select(-CO_REGSAUD, `Município` = nome_mun)

      ## Ordenar pela eficiência selecionada
      df_tabela <- func_order_by_ef_sel(df_tabela, ef_proc_res)
    }
  }
  ## Organizando município selecionado para primeiro da lista
  if(flag_mun){
    if(graph_type == 2){
      df_tabela_mun_sel <- df_tabela[df_tabela$cod_ibge == mun_sel,]
      if(flag_cmp){
        if(length(mun_cmp) > 1){
          mun_cmp <- mun_cmp[1]
        }
        df_tabela_mun_cmp <- df_tabela[df_tabela$cod_ibge == mun_cmp,]
        df_tabela_mun_sel <- rbind(df_tabela_mun_sel, df_tabela_mun_cmp)
        mun_sel <- c(mun_sel, mun_cmp)
      }
      df_tabela_others <- df_tabela |>
        dplyr::filter(!(cod_ibge %in% mun_sel))
      df_tabela <- rbind(df_tabela_mun_sel, df_tabela_others)
    }
    ## Removendo coluna do código do IBGE, tanto para município quanto região de saúde
    df_tabela <- df_tabela |>
      dplyr::select(-cod_ibge, -nome_uf)
  }
  ## Com miniplot ----
  if(with_miniplot){
    # browser()
    ## Se for eficiência de PROCESSOS
    if(ef_proc_res){
      gt_tabela <- df_tabela |>
        dplyr::ungroup() |>
        gt::gt(locale = "pt") |>
        gt::cols_label(
          ef_BCC = gt::md("Eficiência no quadrimestre")
        ) |>
        gt::tab_header(gt::md(paste0("Eficiência dos ", "**", tab_title_ef, "**", " ", tab_title_extra))) |>
        gt::opt_interactive(use_compact_mode = TRUE,
                            use_search = TRUE) |>
        gt::cols_nanoplot(
          columns = starts_with("processos_"),
          new_col_name = "nanoplots",
          new_col_label = gt::md(paste0("Evolução da eficiência"))
        ) |>
        gt::cols_align(align = "center", columns = nanoplots) |>
        gt::cols_hide(
          columns = starts_with("resultados_")
        ) |>
        gt::tab_options(
          heading.title.font.size = "140%"
        )
      ## Se for eficiência de RESULTADOS
    }else{
      gt_tabela <- df_tabela |>
        dplyr::ungroup() |>
        gt::gt(locale = "pt") |>
        gt::cols_label(
          ef_BCC = gt::md("Eficiência no ano")
        ) |>
        gt::tab_header(gt::md(paste0("Eficiência dos ", "**", tab_title_ef, "**", " ", tab_title_extra))) |>
        gt::opt_interactive(use_compact_mode = TRUE,
                            use_search = TRUE) |>
        gt::cols_nanoplot(
          columns = starts_with("resultados_"),
          new_col_name = "nanoplots",
          new_col_label = gt::md(paste0("Evolução da eficiência"))
        ) |>
        gt::cols_align(align = "center", columns = nanoplots) |>
        gt::cols_hide(
          columns = starts_with("processos_")
        ) |>
        gt::tab_options(
          heading.title.font.size = "140%"
        )
    }

    # gt_tabela
  }
  ## Sem miniplot ----
  if(!with_miniplot){
    # browser()
    gt_tabela <- df_tabela |>
      dplyr::ungroup() |>
      gt::gt(locale = "pt") |>
      gt::cols_label(
        ef_BCC = gt::md(paste0("Eficiência no ", title_period))
      ) |>
      gt::fmt_number(
        columns = ef_BCC,
        decimals = 2
      ) |>
      gt::tab_header(gt::md(paste0("Eficiência dos ", "**", tab_title_ef, "**", " ", tab_title_extra))) |>
      gt::opt_interactive(use_compact_mode = TRUE,
                          use_search = TRUE) |>
      gt::cols_hide(
        columns = starts_with(paste0(type_ef, "_"))
      ) |>
      gt::tab_options(
        heading.title.font.size = "140%"
      )
  }

  ## Adicionando estilo para o município selecionado
  if(flag_mun){
    if(graph_type == 2){
      gt_tabela <- gt_tabela |>
        gt::tab_style(
          style = gt::cell_text(color = "#008000", weight = "bold"),
          locations = gt::cells_body(
            rows = c(1) # Targeting the first row
          )
        )
      ## Se comparação existe, adicionar estilo para o município de comparação
      if(flag_cmp){
        gt_tabela <- gt_tabela |>
          gt::tab_style(
            style = gt::cell_text(color = "#FF8C00", weight = "bold"),
            locations = gt::cells_body(
              rows = c(2) # Targeting the seccond row
            )
          )
      }
    }
  }
  # browser()
  # initial_gt_tabela <- gt_tabela
  # usethis::use_data(initial_gt_tabela, overwrite = TRUE)
  ## Retornando tabela criada
  return(gt_tabela)
}
