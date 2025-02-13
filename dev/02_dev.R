# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
attachment::att_amend_desc()
# usethis::use_package("bslib") ## UI utilizada
# usethis::use_package("leaflet") ## Mapa
# usethis::use_package("shinyWidgets") ## Widgets utilizados
# usethis::use_package("lubridate") ## Datas
# usethis::use_package("dplyr") ## Manipulação de dados
# usethis::use_package("tidyr") ## Manipulação de dados
# usethis::use_package("data.table") ## Manipulação de dados
# usethis::use_package("sf") ## Trabalhar com dados geográficos
# usethis::use_package("here") ## Lidar com diretório de execução
usethis::use_package("ggplot2") ## Gráficos
# usethis::use_package("ggiraph") ## Gráficos interativos
# usethis::use_package("ggtext") ## Customizar títulos e texto no ggplot2
# usethis::use_package("patchwork") ## Junção de gráficos
# usethis::use_package("gt") ## Tabela
# # usethis::use_package("future") ## Lidando com tasks assíncronas
# # usethis::use_package("promises") ## Lidando com tasks assíncronas
# # usethis::use_package("patchwork") ## Lidando com múltiplos gráficos
# # usethis::use_package("shinycssloaders") ## Lidando com tasks assíncronas (loader gif)
# usethis::use_package("DBI") ## Conexão com banco de dados
# usethis::use_package("RPostgreSQL") ## Conexão com banco de dados
# usethis::use_package("dbplyr") ## Manipulações no SQL usando dplyr
usethis::use_package("future") ## Assync
usethis::use_package("promises") ## Assync

  ## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "mapa", with_test = F) # Name of the module
golem::add_module(name = "filters", with_test = F) # Name of the module
golem::add_module(name = "graph_lollipop_inputs_outputs", with_test = F) # Name of the module
golem::add_module(name = "tabela_ef", with_test = F) # Name of the module
golem::add_module(name = "modal", with_test = F) # Name of the module
golem::add_module(name = "modal_faixa_cmp", with_test = F) # Name of the module

## Add helper functions ----
## Creates fct_* and utils_*
## Transformação de dados
golem::add_fct("transform_data", with_test = F)
golem::add_fct("transf_data_res", with_test = F)
### Gráficos iniciais
golem::add_fct("init_dfs_iniciais", with_test = F)
golem::add_fct("init_grafs", with_test = F)

## UI
golem::add_fct("ui_panel", with_test = F)

## Server logic
### Apply & manipulations
golem::add_fct("applyFilters", with_test = F)
golem::add_fct("test_if_has_data", with_test = F)
golem::add_fct("server_mod_tabela_ef", with_test = F)
golem::add_fct("server_mod_mapa", with_test = F)
golem::add_fct("server_mod_mapa", with_test = F)
golem::add_fct("get_ef_mun_data", with_test = F)
golem::add_fct("server_mod_graph_lollipop_inputs_outputs", with_test = F)
golem::add_fct("aux_graph_lollipop_input_proc_1", with_test = F)
golem::add_fct("aux_graph_lollipop_input_proc_2", with_test = F)
golem::add_fct("aux_graph_lollipop_outputs_proc", with_test = F)
golem::add_fct("aux_graph_lollipop_input_res_1", with_test = F)
golem::add_fct("aux_graph_lollipop_input_res_2", with_test = F)
golem::add_fct("aux_graph_lollipop_outputs_res", with_test = F)
golem::add_fct("aux_lollipop_patchwork", with_test = F)
golem::add_fct("create_tooltip_ef", with_test = F)
### Server UI
golem::add_fct("selector_type_1", with_test = F)
golem::add_fct("selector_type_2", with_test = F)
golem::add_fct("selector_att_type", with_test = F)
golem::add_fct("cmp_button", with_test = F)
golem::add_fct("selector_type_cmp_uf_faixa", with_test = F)
golem::add_fct("search_capital_name", with_test = F)
golem::add_fct("order_by_ef_sel", with_test = F)
golem::add_fct("check_has_data", with_test = F)
golem::add_fct("get_mun_ied", with_test = F)
golem::add_fct("reset_filters", with_test = F)
golem::add_fct("reset_graphs", with_test = F)
golem::add_fct("transform_data_to_download", with_test = F)
golem::add_fct("download_data", with_test = F)

## DB
### Conexão
golem::add_utils("db", with_test = F)
## Configurações de environment
golem::add_utils("environment", with_test = F)
### Atualização dos dados (usando a transform_data_rda que chama a update_data_PostgreSQL)
golem::add_fct("update_data_PostgreSQL", with_test = F)

## Test
golem::add_fct("test_cols_data", with_test = F)
golem::add_fct("test_duplicate_data", with_test = F)


## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("shiny.ef.aps.dashboard")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis()
usethis::use_travis_badge()

# AppVeyor
usethis::use_appveyor()
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
