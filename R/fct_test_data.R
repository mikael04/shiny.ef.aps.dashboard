#' test_data
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
func_test_data <- function() {
  result <- tryCatch({
    ### Lendo as bases
    ef_muns_proc <- data.table::fread(here::here("data-raw/eficiencia_processos_corrigida_2022_2023.csv"))
    ef_muns_res <- data.table::fread(here::here("data-raw/eficiencia_resultados_corrigida_2022_2023.csv"))

    ### Definindo as variáveis da base e as necessárias
    colnames_ef_muns_proc <- colnames(ef_muns_proc)
    cols_needed_ef_proc <- c("ibge", "nome_mun", "indice_de_equidade_e_dimensionamento_ied",
                             "ef_BCC", "ef_BCC_corrigido",
                             "quad", "ano",
                             ## inputs
                             "desp_por_eq_2", #desp_por_hab_cob
                             ## Quanto falta nos inputs
                             "v_desp_final",
                             ## outputs
                             "ind1_gest", "ind2_teste", "ind3_odonto", "ind4_cito",
                             "ind5_vacina", "ind6_hiper", "ind7_diab",
                             ## Quanto falta nos outputs
                             "v_ind1_pre_natal",
                             "v_ind2_tr", "v_ind3_odonto_gest", "v_ind4_cito", "v_ind5_vac",
                             "v_ind6_has", "v_ind7_dm",
                             ## Outros indicadores
                             "indice_de_vulnerabilidade_social",
                             "numero_de_habitantes_segundo_o_ibge_2022",
                             "faixa_de_porte_populacional_segundo_o_ibge_2022",
                             ## Município de referência
                             "mun_ref")
    has_col_names_proc <- all(cols_needed_ef_proc %in% colnames_ef_muns_proc)
    if(!has_col_names_proc){
      not_in_cols_proc <- paste0(cols_needed_ef_proc[!cols_needed_ef_proc %in% colnames_ef_muns_proc], sep=" ", collapse=", ")
      stop("Parando execução, colunas da base de eficiência de PROCESSOS não encontradas, adicionar colunas faltantes ou transformar colunas.")
      error_code <- 2
    }
    not_in_cols_proc <- ""
    colnames_ef_muns_res <- colnames(ef_muns_res)
    cols_needed_ef_res <- c("ibge", "nome_mun", "indice_de_equidade_e_dimensionamento_ied",
                            "ef_BCC",
                            ## inputs
                            "desp_por_hab",
                            ## Quanto falta nos inputs
                            "v_desp_por_hab",
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
    has_col_names_res <- all(cols_needed_ef_res %in% colnames_ef_muns_res)
    if(!has_col_names_res){
      not_in_cols_res <- paste0(cols_needed_ef_res[!cols_needed_ef_res %in% colnames_ef_muns_res], sep=" ", collapse=", ")
      stop("Parando execução, colunas da base de eficiência de RESULTADOS não encontradas, adicionar colunas faltantes ou transformar colunas.")
      error_code <- 3
    }
    not_in_cols_res <- ""
    error_code <- 0

  }, error = function(e) {
    # Handle the error
    cat("Um erro aconteceu:", conditionMessage(e), "\n")
    if(!has_col_names_proc){
      cat("Colunas faltantes em in ef_muns_proc: ", not_in_cols_proc, "\n")
      error_code <- 2
    }
    if(!has_col_names_res){
      cat("Colunas faltantes em ef_muns_res: ", not_in_cols_res, "\n")
      error_code <- 3
    }
  })
  result
}
