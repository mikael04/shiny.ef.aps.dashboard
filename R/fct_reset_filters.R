#' reset_filters
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
func_reset_filters <- function(session, output, input_type){
  # browser()
  updateSelectInput(session, "type", selected = "no_sel")
}
