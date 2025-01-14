#' transf_data_res
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
transf_data_res <- function(df_res) {
  df_res <- data.table::fread(here::here("data-raw/eficiencia_resultados_corrigida_2022_2023.csv"))

  cols_needed_ef_res <- c("ibge", "nome_mun", "indice_de_equidade_e_dimensionamento_ied",
                          "ef_BCC",
                          ## inputs
                          "desp_por_hab", "eq_por_hab",
                          ## Quanto falta nos inputs
                          "v_desp_por_hab", "v_eq_por_hab",
                          ## outputs
                          "tx_mort", "tx_inter",
                          ## Quanto falta nos outputs
                          "v_tx_mort", "v_tx_inter",
                          ## Outros indicadores
                          "indice_de_vulnerabilidade_social",
                          "numero_de_habitantes_segundo_o_ibge_2022",
                          "faixa_de_porte_populacional_segundo_o_ibge_2022",
                          ## Município de referência
                          "mun_ref")

  ## Alterando nome de colunas
  df_res <- df_res |>
    dplyr::rename(ef_BCC = ef_bcc_corrigido, desp_por_hab = desp_final)

  df_proc <- data.table::fread(here::here("data-raw/eficiencia_processos_corrigida_2022_2023.csv")) |>
    dplyr::select(quad, ibge, nome_mun =  nome_mun,
                  numero_de_habitantes_segundo_o_ibge_2022,
                  indice_de_equidade_e_dimensionamento_ied) |>
    dplyr::distinct(ibge, numero_de_habitantes_segundo_o_ibge_2022, indice_de_equidade_e_dimensionamento_ied)

  ## Verificando se algum valor se repete
  df_proc$ibge[duplicated(df_proc$ibge)]

  ## Adicionar pop_2022 e ied da base de processos
  df_res <- df_res |>
    dplyr::left_join(df_proc, by = "ibge")

  data.table::fwrite(df_res, here::here("data-raw/eficiencia_resultados_corrigida_2022_2023.csv"))
}
