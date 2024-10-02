#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # 0.1 Variáveis iniciais ----
  # Usando dados no formato .rda, carregados no pacote
  data_rda <- F
  # Formato novo, onde os dados e o seletor são filtrado por quadrimestre e não por meses
  tipo_quad <- T
  # list_flags <- list(flag_mapa_regiao_saude, flag_mapa_uf, )
  ## Definindo tipo de gráfico inicial, 0 -> Brasil, mapa inicials da dashboard
  graph_type <- 0
  ## graph_type = 1 <- UFs, 2 <- Municípios, 3 <- Regiões de saúde
  ## Definindo último quadrimestre disponível, para gráficos que apresentaram
  ## inicialmente dados do último quadrimestre (1 quad -> 1 quad de 2022, 4 quad -> 1 quad de 2023)
  last_quad <- 6
  ## Tipo de eficiência padrão
  ### Nomes reativos
  title_ef_def <- reactiveVal("Processos")
  title_map_tab_loc <- reactiveVal("nas UFs")
  title_ef_loc <- reactiveVal(paste0(
    "no <span style=color:#006400><b>", "Brasil", "</b></span>"))
  flag_cmp <- F
  ef_df_cmp <- NULL
  ### Filtro de equidade
  filt_eq <- reactiveVal(T)
  ### Flag para verificar se houve troca de tipo na última interação
  flag_change_type <- reactiveVal(F)
  ### Se recém chamou o seletor de município ou região de saúde, ele chama novamente porque o próprio observe altera o input
  flag_mun_regsaude_first <- reactiveVal(F)
  ## 0.1.1 Debug ----
  debug <- T
  read_data_rds <- F
  read_data_postgres <- T
  ## Quadrimestres usados para Processos
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

  ## Quadrimestre selecionado inicialmente (sempre o último disponível)
  sel_period_name_quad <- names(choices_quad[nrow(df_quad)])
  sel_period <- as.integer(choices_quad[nrow(df_quad)])

  ## Ano selecionado inicialmente (sempre o último disponível)
  choices_ano <- unique(df_quad$ano)
  sel_period_name_ano <- as.character(choices_ano[length(choices_ano)])
  ## Variáveis iniciais para gráficos de pirulito
  def_per_title <- sel_period_name_quad
  ## Título do período inicial, reativo
  title_period <- reactiveVal(sel_period)

  # 0.2 DB Con ----
  con <- utils_db_connect() ##utils_db

  # 1. Dados ----
  ## Atualmente os dados são lidos automaticamente, por estarem na pasta data
  ## Qualquer outro dado que precise ser adicionado, deve usar a função fct_transform_data_rda

  ## Para um teste lendo os dados do csv
  if(data_rda){
    list_dfs <- func_read_data_csv()
    mun_sf = list_dfs[[1]]
    uf_sf = list_dfs[[2]]
    reg_saude_sf = list_dfs[[3]]
    df_mun_cod_ibge = list_dfs[[4]]
    df_cap_uf_ibge = list_dfs[[5]]
    dados_longitudinais = list_dfs[[7]]
    dados_munic_uf_regiao_regsaude = list_dfs[[8]]
    ef_muns = list_dfs[[9]]
    ef_df_ufs = list_dfs[[10]]
    ef_br = list_dfs[[11]]
    ef_mun_cod_ibge = list_dfs[[12]]
  }
  if(read_data_rds){
    dados_longitudinais <- readRDS(file = "data/dados_longitudinais.rds")
    dados_munic_uf_regiao_regsaude <- readRDS(file = "data/dados_munic_uf_regiao_regsaude.rds")
    df_mun_cod_ibge <- readRDS(file = "data/df_mun_cod_ibge.rds")
    df_mun_cod_ibge_regsaude <- readRDS(file = "data/df_mun_cod_ibge_regsaude.rds")
    df_dados_mun_uf_reg_saud_filter <- readRDS(file = "data/df_dados_mun_uf_reg_saud_filter.rds")
    df_mun_ied <- readRDS(file = "data/df_mun_ied.rds")
    df_cap_uf_ibge <- readRDS(file = "data/df_cap_uf_ibge.rds")
    ef_muns_proc <- readRDS(file = "data/ef_muns_proc.rds")
    ef_muns_quad_proc <- readRDS(file = "data/ef_muns_quad_proc.rds")
    ef_muns_res <- readRDS(file = "data/ef_muns_res.rds")
    ef_muns_ano_res <- readRDS(, file = "data/ef_muns_ano_res.rds")
    ef_ufs_proc <- readRDS(file = "data/ef_ufs_proc.rds")
    ef_ufs_proc_quad <- readRDS(file = "data/ef_ufs_proc_quad.rds")
    ef_br_proc <- readRDS(file = "data/ef_br_proc.rds")
    ef_br_proc_quad <- readRDS(file = "data/ef_br_proc_quad.rds")
    ef_ufs_res <- readRDS(file = "data/ef_ufs_res.rds")
    ef_ufs_res_quad <- readRDS(file = "data/ef_ufs_res_quad.rds")
    ef_br_res <- readRDS(file = "data/ef_br_res.rds")
    ef_br_quad <- readRDS(file = "data/ef_br_quad.rds")
  }
  if(read_data_postgres){
    # dados_longitudinais <- dplyr::tbl(con, "dados_longitudinais")
    dados_munic_uf_regiao_regsaude <- dplyr::tbl(con, "dados_munic_uf_regiao_regsaude")
    df_mun_cod_ibge <- dplyr::tbl(con, "df_mun_cod_ibge")
    # df_mun_cod_ibge_regsaude <- dplyr::tbl(con, "df_mun_cod_ibge_regsaude")
    df_dados_mun_uf_reg_saud_filter <- dplyr::tbl(con, "df_dados_mun_uf_reg_saud_filter")
    df_mun_ied <- dplyr::tbl(con, "df_mun_ied")
    df_cap_uf_ibge <- dplyr::tbl(con, "df_cap_uf_ibge")
    ef_muns_proc <- dplyr::tbl(con, "ef_muns_proc")
    ef_muns_quad_proc <- dplyr::tbl(con, "ef_muns_quad_proc")
    ef_muns_res <- dplyr::tbl(con, "ef_muns_res")
    ef_muns_ano_res <- dplyr::tbl(con, "ef_muns_ano_res")
    # ef_ufs_proc <- dplyr::tbl(con, "ef_ufs_proc")
    # ef_ufs_proc_quad <- dplyr::tbl(con, "ef_ufs_proc_quad")
    # ef_br_proc <- dplyr::tbl(con, "ef_br_proc")
    # ef_br_proc_quad <- dplyr::tbl(con, "ef_br_proc_quad")
    # ef_ufs_res <- dplyr::tbl(con, "ef_ufs_res")
    # ef_ufs_res_quad <- dplyr::tbl(con, "ef_ufs_res_quad")
    # ef_br_res <- dplyr::tbl(con, "ef_br_res")
    # ef_br_quad <- dplyr::tbl(con, "ef_br_quad")
    data_from_bd <- read_data_postgres
  }

  ## UFs e seus códigos
  uf <- dados_munic_uf_regiao_regsaude |>
    dplyr::select(CO_UF, nome_uf)
  uf_ordered <- uf |>
    dplyr::distinct(nome_uf) |>
    dplyr::arrange(nome_uf) |>
    dplyr::pull()
  ## município e seus códigos
  mun <- dados_munic_uf_regiao_regsaude |>
    dplyr::select(cod_ibge, nome_mun) |>
    dplyr::pull()
  ## Regiões de saúde e seus códigos
  reg_saude <- dados_munic_uf_regiao_regsaude |>
    dplyr::select(CO_REGSAUD, nome_rs)
  reg_saude_ordered <- reg_saude |>
    dplyr::distinct(nome_rs) |>
    dplyr::arrange(nome_rs) |>
    dplyr::pull()


  ## 1.1 Valores reativos ----
  reactive_values <- reactiveValues()
  reactive_values$cmp_1 <- FALSE
  reactive_values$cmp_2 <- FALSE
  ## Variáveis para identificar primeiro acesso
  initial_state <- reactiveVal(T)

  # 2 Gráficos iniciais ----
  ### Título da caixa do mapa

  ## Reativos porque podemos mudar a eficiência e consequentemente os gráficos e títulos
  observeEvent(input$seletor_ef, {
    ## Versão inicial dos gráficos
    ## Se for a versão inicial da aplicação, apenas alterar os gráficos
    if(initial_state() && input$type == "no_sel"){
      if(!input$seletor_ef){
        title_ef_def <- title_ef_def("Processos")
        ## Mapa do brasil
        mod_mapa_server("mapa_1", initial_state(), ggiraph_map = NULL, ef_proc_res = T)
        ## Tabela de eficiência das UFs
        mod_tabela_ef_server("tabela_ef_1", initial_state(), gt_tabela = NULL, input$seletor_ef)
        ### Inputs e outputs
        mod_graph_lollipop_inputs_outputs_server("graph_lollipop_inputs_outputs_1",
                                                 initial_state = T, ef_proc_res = T, list_graphs_inputs_outputs = NULL)
        output$box_graf_ef <- renderUI({
          div(
            id = "graf-ef",
            class="row",
            div(class= "col-12",
                bslib::card(
                  id = "cardInputsOutputs",
                  class = "ui-cards",
                  fill = F,
                  max_height = "55vh",
                  uiOutput("title_ef"),
                  # ## Gráfico de inputs e outputs
                  mod_graph_lollipop_inputs_outputs_ui("graph_lollipop_inputs_outputs_1"),
                  HTML('
                        <div class="legenda">
                          <span class="dot dot1"></span> <span class="nomelegenda"> Município eficiente</span>
                          <span class="dot dot2"></span> <span class="nomelegenda"> Valor para 100% de eficiência</span>
                          <span class="dot dot3"></span> <span class="nomelegenda"> Valor do município selecionado</span>
                          <span class="dot dot4"></span> <span class="nomelegenda"> Valor município de comparação</span>
                        </div>
                      ')
                )
            )
          )
        })
      }else{
        title_ef_def <- title_ef_def("Resultados")
        ## Mapa do brasil
        mod_mapa_server("mapa_1", initial_state(), ggiraph_map = NULL, ef_proc_res = F)
        ## Tabela de eficiência das UFs
        mod_tabela_ef_server("tabela_ef_1", initial_state(), gt_tabela = NULL, input$seletor_ef)
        ### Inputs e outputs
        mod_graph_lollipop_inputs_outputs_server("graph_lollipop_inputs_outputs_1",
                                                 initial_state = T, ef_proc_res = F, list_graphs_inputs_outputs = NULL)
      }
      ## Senão for a versão inicial da aplicação, apresentar popup para troca
    }else{
      ## Alterando o título dos gráficos de eficiência
      if(!input$seletor_ef){
        title_ef_def <- title_ef_def("Processos")

      }else{
        title_ef_def <- title_ef_def("Resultados")
      }
      mod_modal_server("modal_2", title = "Troca de eficiência",
                       first_test = "Você deseja trocar a eficiência?",
                       mid_text = " Ao trocar a eficiência, <b>os gráficos apresentados serão alterados.</b>",
                       end_text = " Se sim, </b>clique em 'Aplicar filtros'</b> novamente para ver os novos gráficos.")
    }
    if(!input$seletor_ef){
      updateSelectizeInput(session,
                           "sel_period",
                           label = "Selecione o quadrimestre:",
                           choices = choices_quad, selected = choices_quad[nrow(df_quad)])
      title_period = title_period(sel_period_name_quad)

    }else{
      updateSelectizeInput(session,
                           "sel_period",
                           label = "Selecione ano:",
                           choices = c("2022", "2023"), selected = "2023")
      title_period = title_period(sel_period_name_ano)
    }
  })
  ## Versão inicial da aplicação dos títulos de mapa e gráfico de eficiência
  ## Atualizando nome do período
  output$title_map <- renderUI({
    div(class="titles-graph title-map",
        span(HTML(paste0("Eficiência de <b>", title_ef_def(), "</b> ", title_map_tab_loc()))),
        br(),
        span(class = "subtitle", ifelse(!input$seletor_ef, "No período ", "No ano "), title_period())
    )})
  ### Título do gráfico de eficiência
  output$title_ef <- renderUI({
    div(class="titles-graph title-ef",
        span(HTML(paste0("Métricas de Eficiência "))),
        span(class="title-mun-sel", HTML(paste0(title_ef_loc()))),
        br(),
        span(class = "subtitle", ifelse(!input$seletor_ef, "No período ", "No ano "), title_period())
    )
  })
  ## 2.2 Títulos reativos, adição/remoção do gráfico de eficiência  ----
  observeEvent(input$applyFilters,{
    ## Título do mapa, atualizando nome do período
    output$title_map <- renderUI({
      div(class="titles-graph title-map",
          span(HTML(paste0("Eficiência de <b>", title_ef_def(), "</b> ", title_map_tab_loc()))),
          br(),
          span(class = "subtitle", ifelse(!input$seletor_ef, "No período ", "No ano "), title_period())
      )
    })
    ## Título do gráfico de eficiência
    output$title_ef <- renderUI({
      div(class="titles-graph title-ef",
          span(HTML(paste0("Métricas de Eficiência "))),
          span(class="title-mun-sel", HTML(paste0(title_ef_loc()))),
          br(),
          span(class = "subtitle", ifelse(!input$seletor_ef, "No período ", "No ano "), title_period())
      )
    })
    ## Criando gráfico de eficiência ou apenas box vazio
    if(initial_state()){
      # browser()
      ## 2.2.1 Gráfico de eficiência ----
      output$box_graf_ef <- renderUI({
        div(
          id = "graf-ef",
          class="row",
          div(class= "col-12",
              bslib::card(
                id = "cardInputsOutputs",
                class = "ui-cards",
                fill = F,
                max_height = "55vh",
                uiOutput("title_ef"),
                # ## Gráfico de inputs e outputs
                mod_graph_lollipop_inputs_outputs_ui("graph_lollipop_inputs_outputs_1"),
                HTML('
                        <div class="legenda">
                          <span class="dot dot1"></span> <span class="nomelegenda"> Município eficiente</span>
                          <span class="dot dot2"></span> <span class="nomelegenda"> Valor para 100% de eficiência</span>
                          <span class="dot dot3"></span> <span class="nomelegenda"> Valor do município selecionado</span>
                          <span class="dot dot4"></span> <span class="nomelegenda"> Valor município de comparação</span>
                        </div>
                      ')
              )
          )
        )
      })
    }else{
      ## UF
      if(input$type == "uf"){
        output$box_graf_ef <- renderUI({
          NULL
        })
      }
      ## Município
      if(input$type == "mun"){
        output$box_graf_ef <- renderUI({
          div(
            id = "graf-ef",
            class="row",
            div(class= "col-12",
                bslib::card(
                  id = "cardInputsOutputs",
                  class = "ui-cards",
                  fill = F,
                  max_height = "55vh",
                  uiOutput("title_ef"),
                  # ## Gráfico de inputs e outputs
                  mod_graph_lollipop_inputs_outputs_ui("graph_lollipop_inputs_outputs_1"),
                  HTML('
                        <div class="legenda">
                          <span class="dot dot1"></span> <span class="nomelegenda"> Município eficiente</span>
                          <span class="dot dot2"></span> <span class="nomelegenda"> Valor para 100% de eficiência</span>
                          <span class="dot dot3"></span> <span class="nomelegenda"> Valor do município selecionado</span>
                          <span class="dot dot4"></span> <span class="nomelegenda"> Valor município de comparação</span>
                        </div>
                      ')
                )
            )
          )
        })
      }
      # Região de saúde
      if(input$type == "reg_saude"){
        func_att_selector_type(session, output, input$sel_uf_1, "sel_reg_saude_1", df_dados_mun_uf_reg_saud_filter, mun_regsaude = 2, df_cap_uf_ibge)
        # func_cmp_button(session, output, input$type, "outra região de saúde")
        output$box_graf_ef <- renderUI({
          NULL
        })
      }
    }
  })

  # Your application server logic
  # 7. Download dados (Modal) ----
  ## 7.1 Download dados filtrados ----
  ### Município ou região de saúde selecionada
  output$downloadFilteredData <- downloadHandler(
    filename = function() {
      nome_ef <- ifelse(!input$seletor_ef, "resultados", "processos")
      paste('dados-ef-', nome_ef, "-", ifelse(input$type == "mun", input$sel_mun_1, input$sel_reg_saude_1), '.csv', sep='')
    },
    content = function(con_data) {
      ## Se FALSE, processos, se TRUE, resultados
      if(!input$seletor_ef){
        dados_ef <- dplyr::tbl(con, "df_muns_proc_download")
      }else{
        dados_ef <- dplyr::tbl(con, "df_muns_res_download")
      }
      filters <- list(input$type, input$sel_uf_1, input$sel_reg_saude_1, input$sel_mun_1)
      ## Função para download dos dados filtrados
      data_download <- func_download_data(dados_ef, input$seletor_ef, filters)
      write.csv(data_download, con_data)
    }
  )
  ### UF selecionada
  output$downloadFilteredUF <- downloadHandler(
    filename = function() {
      nome_ef <- ifelse(!input$seletor_ef, "resultados", "processos")
      paste('dados-ef-', nome_ef, "-", input$sel_uf_1, '.csv', sep='')
    },
    content = function(con_data) {
      ## Se FALSE, processos, se TRUE, resultados
      if(!input$seletor_ef){
        dados_ef <- dplyr::tbl(con, "df_muns_proc_download")
      }else{
        dados_ef <- dplyr::tbl(con, "df_muns_res_download")
      }
      # browser()
      filters <- list(input$type, input$sel_uf_1, NULL, NULL)
      ## Função para download dos dados filtrados
      data_download <- func_download_data(dados_ef, input$seletor_ef, filters)
      write.csv(data_download, con_data)
    }
  )
  ### Todos os dados
  output$downloadAllData <- downloadHandler(
    filename = function() {
      nome_ef <- ifelse(!input$seletor_ef, "resultados", "processos")
      paste('dados-ef-', nome_ef, '.csv', sep='')
    },
    content = function(con_data) {
      ## Se FALSE, processos, se TRUE, resultados
      if(!input$seletor_ef){
        dados_ef <- dplyr::tbl(con, "df_muns_proc_download")
      }else{
        dados_ef <- dplyr::tbl(con, "df_muns_res_download")
      }
      write.csv(dados_ef, con_data)
    }
  )
  ## 7.2 Download dos dados completos ----

  # 8. Encerrar conexões, limpar dados ----
  onStop(function(){
    ## Desconectando do banco de dados
    DBI::dbDisconnect(con)
  })
  ## Encerrar a sessão
  session$onSessionEnded(function() {
    DBI::dbDisconnect(con)
  })
}
