#' get_mun_ied
#'
#' @description Uma função que recebe o município selecionado e uf para achar o seu ied (faixa)
#'
#' @return Retorna o ied (valor) do município selecionado
#'
#' @noRd
func_get_mun_ied <- function(df_mun_ied, uf_mun_sel, mun_sel){
  # browser()
  ## Buscando o cod_ibge do município selecionado
  ied_mun_sel <- df_mun_ied |>
    dplyr::filter(nome_uf == uf_mun_sel, nome_mun == mun_sel) |>
    dplyr::distinct(ied) |>
    dplyr::pull(ied)

  return(ied_mun_sel)
}
