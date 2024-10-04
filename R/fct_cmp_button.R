#' cmp_button
#'
#' @description A fct function that manipulates the compare button
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
func_cmp_button <- function(session, output, type, text){
  if(type == "mun"){
    output$cmp_button <- renderUI({
      shinyWidgets::awesomeCheckbox(
        inputId = "cmp_1",
        label = paste0("Comparar com ", text),
        value = FALSE
      )
    })
  }else{
    output$cmp_button <- renderUI({
      NULL
    })
  }
}
