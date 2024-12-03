#' ui_panel
#'
#' @description A function that creates the panel UI
#'
#' @return No return
#'
#' @noRd
func_ui_panel <- function(session, id){
  div(
    class="panel-body",
    div(
      class="row",
      div(
        class="col-3",
        div(
          class="column-left",
          div(
            # mod_valueBox_ui("valueBox_0")
          ),
          # Filtro lateral ----
          div(
            class="filtro-lateral",
            ## Seletor de tipo de eficiência
            div(
              div(
                class="h4 tit-eficiencia",
                "Eficiência de"
              ),
              div(
                class = "seletor_ef",
                tags$span(id="left", class="pointer", "PROCESSOS"),
                shinyWidgets::materialSwitch(inputId = "seletor_ef", label = "", inline = TRUE),
                tags$span(id="right", class="pointer", "RESULTADOS")
              )
            ),
            ## Seletor do filtro de equidade
            div(
              div(
                class="h4 tit-equidade",
                "Equidade"
              ),
              div(
                class = "seletor_ef",
                tags$span(id="left", class="pointer", "LIGADO"),
                shinyWidgets::materialSwitch(inputId = "seletor_eq", label = "", inline = TRUE),
                tags$span(id="right", class="pointer", "DESLIGADO")
              )
            ),
            selectizeInput(
              inputId = "sel_period",
              label = "Selecione o quadrimestre:",
              choices = choices_quad,
              selected = choices_quad[nrow(df_quad)],
              width = "100%"
            ),
            selectizeInput(
              inputId = "sel_period",
              label = "Selecione o quadrimestre:",
              choices = choices_quad,
              selected = choices_quad[nrow(df_quad)],
              width = "100%"
            ),
            ## Filtros dinâmicos UF/RegSaude/Mun ----
            mod_filters_ui("filters_1"),
            ## Filtro de equidade
            # uiOutput("checkbox_filtro_eq"),
            ## Switch, não usado
            # div(
            #   div(
            #     class="h4 tit-eq",
            #     "Filtro de equidade"
            #   ),
            #   div(
            #     class = "seletor-f-eq",
            #     tags$span(id="left-eq", class="pointer", "LIGADO"),
            #     shinyWidgets::materialSwitch(inputId = "filtro_eq", label = "", inline = T),
            #     tags$span(id="right-eq", class="pointer", "DESLIGADO")
            #   )
            # ),
            ## Botões aplicar/reinicar ----
            div(
              class="row botoes-aplicar-resetar",
              div(class="col-6 text-center botaoResetar",
                  shinyWidgets::actionBttn(
                    inputId = "resetFilters",
                    label = "Reiniciar gráficos",
                    size = "md",
                    style = "jelly",
                    color = "danger"
                  )
              ),
              div(class="col-6 text-center botaoAplicar",
                  shinyWidgets::actionBttn(
                    inputId = "applyFilters",
                    label = HTML("Aplicar filtros"),
                    size = "md",
                    style = "jelly",
                    color = "default"
                  )
              ),
            ),
            br(),
            div(
              class="text-center download",
              shinyWidgets::actionBttn(
                inputId = "downloadData",
                label = "Baixar os dados",
                style = "material-flat",
                size = "md",
                color = "success"
              )
            ),
            br()
          )
        )
      ),
      # Gráficos ----
      div(
        class="col-9",
        ## 1a linha de gráficos ----
        div(
          class="panel-graphs-info",
          div(
            class="row",
            ### 1a linha, 1a col de gráficos ----
            div(class= "col-6",
                # mod_mapa_brasil_ui("mapa_1"),
                # mod_mapa_mun_ui("mapa_mun_1")
                # leafletOutput("mapa_1")
                bslib::card(
                  id = "cardMap",
                  class = "ui-cards",
                  fill = F,
                  max_height = "55vh",
                  min_height = "55vh",
                  uiOutput("title_map"),
                  mod_mapa_ui("mapa_1")
                )
            ),
            ### 1a linha, 2a col de gráficos ----
            div(class= "col-6",
                # ),
                # div(
                #   class="row",
                # uiOutput("graph_linha_1")
                bslib::card(
                  id = "cardTable",
                  class = "ui-cards",
                  fill = F,
                  max_height = "55vh",
                  min_height = "55vh",
                  mod_tabela_ef_ui("tabela_ef_1")
                  # mod_graph_linha_ui("graph_linha_1")
                )
            )
          ),
          ## 2a linha de gráficos ----
          ### Box com gráfico de eficiência que será apresentado para o Brasil (graf inicial)
          ### e para os municípios
          uiOutput("box_graf_ef"),
          ### Dois gráficos separados
          # div(
          #   class="row",
          #   ### 2a linha, 1a col de gráficos ----
          #   div(class= "col-6",
          #       # mod_mapa_brasil_ui("mapa_1"),
          #       # mod_mapa_mun_ui("mapa_mun_1")
          #       # leafletOutput("mapa_1")
          #       bslib::card(
          #         id = "cardInputs",
          #         class = "ui-cards",
          #         full_screen = T,
          #         fill = T,
          #         ## Gráfico de inputs
          #         mod_graph_lollipop_inputs_ui("graph_lollipop_inputs_1")
          #         ## Gráficos exemplo
          #         # plotOutput("plot")
          #       )
          #   ),
          #   ### 2a linha, 2a col de gráficos ----
          #   div(class= "col-6",
          #       # ),
          #       # div(
          #       #   class="row",
          #       # uiOutput("graph_linha_1")
          #       bslib::card(
          #         id = "cardOutputs",
          #         class = "ui-cards",
          #         full_screen = T,
          #         fill = T,
          #         mod_graph_lollipop_outputs_ui("graph_lollipop_outputs_1")
          #       )
          #   )
          # )
        )
      )
    )
  )
}
