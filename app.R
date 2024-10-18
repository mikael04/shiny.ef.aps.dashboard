# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

env_settings <-  utils_environment_config() ##utils_environtment
pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = env_settings$prod)
options(shiny.port = env_settings$shiny_port)
shiny.ef.aps.dashboard::run_app() # add parameters here (if any)
