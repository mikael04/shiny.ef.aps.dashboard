#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shiny.router
#' @noRd

## Datasets ----
# df_meses <- data.frame(id=c(1:24),
#                        meses=c("Jan/22", "Fev/22", "Mar/22", "Abr/22", "Mai/22", "Jun/22",
#                                "Jul/22", "Ago/22", "Set/22", "Out/22", "Nov/22", "Dez/22",
#                                "Jan/23", "Fev/23", "Mar/23", "Abr/23", "Mai/23", "Jun/23",
#                                "Jul/23", "Ago/23", "Set/23", "Out/23", "Nov/23", "Dez/23"))
#
#
# meses <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
#            "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")

df_quad <- data.frame(id=c(1:6),
                      ano=c(rep(2022, 3),
                            rep(2023, 3)),
                      quad_lab=c("1° Quad/2022", "2° Quad/2022", "3° Quad/2022",
                                 "1° Quad/2023", "2° Quad/2023", "3° Quad/2023")
)

## Criando vetor com label e cod para selectInput
vector_quad_select_lab <- df_quad$quad_lab
vector_quad_select_cod <- df_quad$id
choices_quad <- vector_quad_select_cod
names(choices_quad) <- vector_quad_select_lab

## Usando alertas do SweetAlert
# useSweetAlert()

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    ## Test
    tags$script(src = "https://cdn.jsdelivr.net/gh/Appsilon/shiny.tictoc@v0.2.0/shiny-tic-toc.min.js"),
    # Your application UI logic
    fluidPage(
      class = "pageLayout",
      theme = bslib::bs_theme(version = 5),
      func_ui_panel(session, "panel")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "shiny.ef.aps"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
