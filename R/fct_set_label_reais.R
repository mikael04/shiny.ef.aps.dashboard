#' set_label_reais
#'
#' @description A function that sets the label for the brazilian currency format
#'
#' @return The value in the brazilian currency format
#'
#' @noRd
func_set_label_reais <- function(value) {
  label_br <- scales::label_currency(prefix = "R$", suffix = "", decimal.mark = ",", big.mark = ".")
  label_br(value)
}
