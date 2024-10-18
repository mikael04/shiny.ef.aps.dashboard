#' environment
#'
#' @description A utils function that gets the environment configs
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
utils_environment_config <- function() {
  config::get("run_settings", file = "inst/golem-config.yml")
}
