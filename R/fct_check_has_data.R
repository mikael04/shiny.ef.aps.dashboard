#' check_has_data
#'
#' @description Uma função para checar se existe dados na base, para aquele município ou região de saúde
#'
#' @return Retorna T se encontrar um valor, F se não encontrar
#'
#' @noRd
func_check_has_data <- function(df, uf_sel, mun_regsaud_sel, type, ef_proc_res){
  ## Determinando qual é o tipo de eficiência
  if(!ef_proc_res){
    ## Eficiência de resultados
    has_data <- "has_data_res"
  }else{
    ## Eficiência de processos
    has_data <- "has_data_proc"
  }
  ## Filtrando por município ou região de saúde
  if(type == "mun"){
    ## Eficiência de processos
    df |>
      dplyr::filter(nome_uf == uf_sel,
                    nome_mun == mun_regsaud_sel) |>
      dplyr::pull(!!as.name(has_data))
  }else if(type == "reg_saude"){
    df |>
      dplyr::filter(nome_uf == uf_sel,
                    nome_rs == mun_regsaud_sel) |>
      dplyr::pull(has_data)
  }else if(type == "uf"){
    ## Sempre vai devolver T, porque todos os estados possuem dados, não precisa fazer a checagem
    TRUE
  }
}
