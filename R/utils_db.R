#' db
#'
#' @description A utils function that will connect to database
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
utils_db_config <- function(local) {
  if(local){
    config::get("db", file = "../shiny.ef.aps.dashboard.config/golem-config.yml")
  }
  docker = !local
  if(docker){
    host <- Sys.getenv("POSTGRES_HOST", "localhost")  # Fallback to "localhost" if not set
    port <- Sys.getenv("POSTGRES_PORT")
    dbname <- Sys.getenv("POSTGRES_DB")
    user <- Sys.getenv("POSTGRES_USER")
    password <- Sys.getenv("POSTGRES_PASSWORD")
    list(host = host, port = port, dbname = dbname, user = user, password = password)
  }
}

utils_db_connect <- function(local) {
  # Sys.getenv("DB_HOST")
  db_config <- utils_db_config(local)
  DBI::dbConnect(
    RPostgreSQL::PostgreSQL(),
    host = db_config$host,
    port = db_config$port,
    dbname = db_config$dbname,
    user = db_config$user,
    password = db_config$password
  )
}
