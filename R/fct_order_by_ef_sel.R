#' order_by_ef_sel
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
func_order_by_ef_sel <- function(df_tabela, ef_proc_res){
  ## Se ef_proc_res = T, estamos utilizando eficiência de processos, se F, estamos usando eficiência de resultados
  df_tabela <- df_tabela |>
    dplyr::arrange(desc(ef_BCC))
  return(df_tabela)
}
