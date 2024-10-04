#' db
#'
#' @description A utils function that will connect to database
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
utils_db_config <- function() {
  config::get("db", file = "inst/golem-config.yml")
}

utils_db_connect <- function() {
  # Sys.getenv("DB_HOST")
  db_config <- utils_db_config()
  DBI::dbConnect(
    RPostgreSQL::PostgreSQL(),
    host = db_config$host,
    port = db_config$port,
    dbname = db_config$dbname,
    user = db_config$user,
    password = db_config$password
  )
}
