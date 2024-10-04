#' download_data
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
func_download_data <- function(dados_ef, input_seletor_ef, filters){
  type = filters[[1]]
  if(type == "mun"){
    uf_sel <- filters[[2]]
    mun_sel <- filters[[4]]

    dados_ef <- dados_ef |>
      dplyr::filter(nome_uf == uf_sel & nome_mun == mun_sel) |>
      dplyr::collect()
  }
  if(type == "reg_saude"){
    uf_sel <- filters[[2]]
    reg_saud_sel <- filters[[2]]

    dados_ef <- dados_ef |>
      dplyr::filter(nome_uf == uf_sel & nome_rs == reg_saud_sel) |>
      dplyr::collect()
  }
  if(type == "uf"){
    uf_sel <- filters[[2]]

    dados_ef <- dados_ef |>
      dplyr::filter(nome_uf == uf_sel) |>
      dplyr::collect()
  }
  dados_ef
}
