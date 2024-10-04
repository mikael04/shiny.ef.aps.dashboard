#' get_ef_mun_data
#'
#' @description A fct function to return the ef data of the county selected
#'
#' @return The return value, if any, from executing the function.
#'
#' @export
#'
#' @noRd

func_get_ef_mun_data <- function(ef_df, cod_ibge_sel){
  teste_interno <- F
  if(teste_interno){
    ef_df <- ef_df
    cod_ibge_sel <- cod_mun_sel
  }
  ef_df |>
    dplyr::filter(cod_ibge == cod_ibge_sel)
}
