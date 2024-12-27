# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
source("R/utils_environment.R")
print(paste0("Working directory = ", getwd()))
env_settings <- utils_environment_config() ##utils_environtment
print(paste0("Env = ", Sys.getenv("R_CONFIG_ACTIVE")))
print(paste0("Prod = ", env_settings$prod))
print(paste0("Port = ", env_settings$port))
options( "golem.app.prod" = env_settings$prod)
options(shiny.port = env_settings$port)
shiny.ef.aps.dashboard::run_app() # add parameters here (if any)
