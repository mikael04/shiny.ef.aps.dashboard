#' test_if_has_data
#'
#' @description Uma função que recebe um df e testa se existe dados nele (nrow)
#'
#' @return Retorna TRUE se existir dados, FALSE senão existir
#'
#' @export
#'
#' @noRd

func_test_if_has_data <- function(ef_df_mun_sel){
  return(nrow(ef_df_mun_sel) > 0)
}
