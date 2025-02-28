# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file


## Lidando com argumentos
args <- commandArgs(trailingOnly = TRUE)
## Rodando Shiny
pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
source("R/utils_environment.R")
# print(paste0("Working directory = ", getwd()))
## Buscando configurações do ambiente
env_settings <- utils_environment_config()

## Refazendo gráficos iniciais
cat("Refazendo gráficos iniciais\n")
source("R/fct_transform_data.R")
func_transform_data(verbose = F, overwrite_data = T, update_db = F)

cat("\nIniciando servidor\n")
## Se estiver rodando em modo dev, setar a porta pelo argumento da linha de comando
if(shiny::isTruthy(args) && args[1] == "dev"){
  print("app.R in dev mode")
  print(paste0("args = ", args))
  options( "golem.app.prod" = env_settings$prod)
  options(shiny.port = as.integer(args[2]))
} else {
  print(paste0("Env = ", Sys.getenv("R_CONFIG_ACTIVE")))
  print(paste0("Prod = ", env_settings$prod))
  print(paste0("Port = ", env_settings$port))
  options( "golem.app.prod" = env_settings$prod)
  options(shiny.port = env_settings$port)
}
shiny.ef.aps.dashboard::run_app() # add parameters here (if any)
