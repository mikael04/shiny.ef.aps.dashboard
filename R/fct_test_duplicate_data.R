#' test_duplicate_data
#'
#' @description Uma função que irá testar se os dados possuem duplicatas, se possuírem, a função irá remover as duplicatas
#'
#' @return Retorna o tipo de erro associado (0 sendo não-erro, demais valores erros)
#'
#' @noRd
func_test_duplicate_data <- function(overwrite, verbose) {
  if(F){
    overwrite <- T
    verbose <- T
  }
  result <- tryCatch({
    if(verbose)
      cat("Iniciando teste de duplicatas nas bases de eficiência", "\n")
    ### Lendo as bases
    ef_muns_proc <- data.table::fread(here::here("data-raw/eficiencia_processos_corrigida_2022_2023.csv"))
    ef_muns_res <- data.table::fread(here::here("data-raw/eficiencia_resultados_corrigida_2022_2023.csv"))

    ### Verificando dados duplicados (município aparece mais vezes do que deveria)
    #### Processos
    if(verbose)
      cat("Verificando duplicatas na base de eficiência de PROCESSOS", "\n")
    ef_muns_proc_dup <- ef_muns_proc |>
      dplyr::group_by(ibge, quad) |>
      dplyr::mutate(n = dplyr::n()) |>
      dplyr::filter(n > 1) |>
      dplyr::select(ibge, quad, ef_BCC, n)

    if(nrow(ef_muns_proc_dup) > 0){
      if(verbose){
        cat("Municípios duplicados em ef_muns_proc: ", "\n")
        cat(capture.output(print(ef_muns_proc_dup)), sep = "\n")
      }
      if(overwrite){
        ef_muns_proc <- ef_muns_proc |>
          dplyr::group_by(ibge, quad) |>
          dplyr::arrange(ef_BCC) |>
          dplyr::distinct(ibge, quad, .keep_all = TRUE)

        cat("Escrevendo nova base de eficiência de PROCESSOS sem duplicatas", "\n")
        data.table::fwrite(ef_muns_proc, here::here("data-raw/eficiencia_processos_corrigida_2022_2023.csv"))
      } else {
        stop("Encontrado um ou mais municípios duplicados na base de eficiência de PROCESSOS, corrigir duplicatas ou usar overwrite = TRUE.")
        error_code <- 1
      }
    }else{
      if(verbose)
        cat("Não foram encontrados municípios duplicados na base de eficiência de PROCESSOS", "\n")
    }

    #### Resultados
    ef_muns_res_dup <- ef_muns_res |>
      dplyr::group_by(ibge, ano) |>
      dplyr::mutate(n = dplyr::n()) |>
      dplyr::filter(n > 1) |>
      dplyr::select(ibge, ano, ef_BCC, n) |>
      dplyr::ungroup()

    if(nrow(ef_muns_res_dup) > 0){
      if(verbose){
        cat(paste0("Municípios duplicados em ef_muns_res: ","\n"))
        cat(capture.output(print(ef_muns_res_dup)), sep = "\n")
      }
      if(overwrite){
        ef_muns_res <- ef_muns_res |>
          dplyr::group_by(ibge, ano) |>
          dplyr::arrange(ef_BCC) |>
          dplyr::distinct(ibge, ano, .keep_all = TRUE)

        cat("Escrevendo nova base de eficiência de RESULTADOS sem duplicatas", "\n")
        data.table::fwrite(ef_muns_res, here::here("data-raw/eficiencia_resultados_corrigida_2022_2023.csv"))
      } else {

        stop("Encontrado um ou mais municípios duplicados na base de eficiência de RESULTADOS, corrigir duplicatas ou usar overwrite = TRUE.")
        error_code <- 2
      }
    }else{
      if(verbose)
        cat("Não foram encontrados municípios duplicados na base de eficiência de RESULTADOS", "\n")
    }



    ## Não houve erros
    error_code <- 0

  }, error = function(e) {
    # Handle the error
    cat("Um erro aconteceu:", conditionMessage(e), "\n")
    error_code <- 1
  })
  if(verbose)
    cat("Finalizando teste de duplicatas nas bases de eficiência", "\n")
  result
}
