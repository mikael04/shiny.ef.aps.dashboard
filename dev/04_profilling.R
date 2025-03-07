# Setup Golem app -----
# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
# options(shiny.port = httpuv::randomPort())
options(shiny.port = 4001)

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# # Reactlog ----
# ## tell shiny to log all reactivity
# reactlog_enable()

# ## Run app
#  run_app()

# ## once app has closed, display reactlog from shiny
# shiny::reactlogShow()

# Profvis ----
profilling <- profvis::profvis(print(run_app()))
