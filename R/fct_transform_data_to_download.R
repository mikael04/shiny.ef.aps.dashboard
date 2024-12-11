#' transform_data_to_download
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
func_transform_data_to_download <- function(ef_muns_proc, ef_muns_res){

  ## Processos ----
  ### Transformando para uma tabela mais limpa para download
  df_ef_proc <- ef_muns_proc |>
    dplyr::select(
      ## Dados geográficos
      nome_mun, cod_ibge, nome_rs, nome_uf, ano, ano_quad,
      ## Dados sociodemográficos
      pop_2022, ivs, faixa_porte_2022, ied,
      ## Dados de eficiência
      ef_BCC,
      ## Inputs
      desp_por_hab_cob,
      ### Quanto falta nos inputs
      v_desp_por_hab_cob,
      ## Outputs
      ind1_gest, ind2_teste, ind3_odonto, ind4_cito, ind5_vacina, ind6_hiper, ind7_diab,
      ### Quanto falta nos outputs
      v_ind1_pre_natal, v_ind2_tr, v_ind3_odonto_gest, v_ind4_cito, v_ind5_vac,
      v_ind6_has, v_ind7_dm,
      ## Município de referência
      mun_ref
    )

  ## Resultados ----

  ### Transformando para uma tabela mais limpa para download
  df_ef_res <- ef_muns_res |>
    dplyr::select(
      ## Dados geográficos
      nome_mun, cod_ibge, nome_rs, nome_uf, ano,
      ## Dados de eficiência
      ef_BCC,
      ## Dados sociodemográficos
      pop_2022, ivs, faixa_porte_2022, ied,
      ## Inputs
      desp_por_hab_cob, eq_por_hab_cad,
      ## Quanto falta nos inputs
      v_desp_por_hab_cob, v_eq_por_hab_cad,
      ## Outputs
      tx_mort, tx_inter,
      ### Quanto falta nos outputs
      v_tx_mort, v_tx_inter,
      ## Município de referência
      mun_ref
    )

  ### Retornando dois dfs
  list_dfs <- list(df_ef_proc, df_ef_res)
  list_dfs
}
