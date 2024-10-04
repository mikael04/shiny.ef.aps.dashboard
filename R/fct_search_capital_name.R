#' search_capital_name
#'
#' @description Buscar o nome da capital para o filtro do selectInput
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
func_nome_capital <- function(df_cap_uf_ibge, sel_uf){
  df_cap_uf_ibge |>
    dplyr::filter(nome_uf == sel_uf) |>
    dplyr::pull(nome_mun)
}
