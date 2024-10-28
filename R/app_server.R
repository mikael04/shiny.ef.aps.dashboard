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
  ## 2.0 Modal de carregamento inicial ----
  ## Modal para avisar que dados estão carregando
  shinyalert::shinyalert(
    html = TRUE,
    title = "Carregando painel",
    showConfirmButton = F,
    text = tagList(
      HTML("<div class='loader_data'></div>")
    ),
    closeOnEsc = T,
    closeOnClickOutside = T
  )
  ## 2.1 Gráficos iniciais ----
  ### Título da caixa do mapa
  ## Reativos porque podemos mudar a eficiência e consequentemente os gráficos e títulos
  observeEvent(input$seletor_ef, {
    # browser()
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
      # browser()
      ## Fechando modal de carregamento inicial
      shinyalert::closeAlert()
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
        func_selector_att_type(session, output, input$sel_uf_1, "sel_reg_saude_1", df_dados_mun_uf_reg_saud_filter, mun_regsaude = 2, df_cap_uf_ibge)
        # func_cmp_button(session, output, input$type, "outra região de saúde")
        output$box_graf_ef <- renderUI({
          NULL
        })
      }
    }
  })

  # 3. Aplicar (aplicando filtros reativos) ----
  observeEvent(input$applyFilters, {
    initial_state(F)
    ## Inputs passados para promises
    input_seletor_ef <- input$seletor_ef
    input_type <- input$type
    input_sel_period <- sel_period
    input_sel_uf_1 <- input$sel_uf_1
    input_sel_reg_saude_1 <- input$sel_reg_saude_1
    input_sel_mun_1 <- input$sel_mun_1
    ## Inputs de comparação
    input_sel_uf_2 <- input$sel_uf_2
    input_sel_reg_saude_2 <- input$sel_reg_saude_2
    input_sel_mun_2 <- input$sel_mun_2
    input_sel_period_name <- choices_quad[input_sel_period]

    ## Flag de comparação
    if(is.null(input_sel_mun_2)){
      flag_cmp <- F
    }else{
      flag_cmp <- T
    }
    ## Alterando título dos gráficos
    if(input_type == "uf"){
      title_ef_loc("na UF selecionada")
      title_map_tab_loc("na UF selecionada")
      input_2 <- NULL
      output$box_graf_ef <- renderUI({
        NULL
      })
    }
    if(input_type == "reg_saude"){
      title_ef_loc("na Região de Saúde selecionada")
      title_map_tab_loc("na Região de Saúde selecionada")
      input_2 <- input$sel_reg_saude_1
      output$box_graf_ef <- renderUI({
        NULL
      })
    }
    if(input_type == "mun"){
      title_ef_loc(paste0("em <span style=color:#006400><b>", input_sel_mun_1, "</b></span>"))
      title_map_tab_loc("na Região de Saúde")
      input_2 <- input$sel_mun_1
    }
    ## Checando para ver se município selecionado possui dados
    check <- func_check_has_data(df_dados_mun_uf_reg_saud_filter, input_sel_uf_1, input_2, input_type, input_seletor_ef)
    if(!isTruthy(check)){
      mod_modal_server("modal_1",
                       title = "Não há dados",
                       first_test = "O município selecionado",
                       mid_text = " não possui dados disponíveis na nossa base de dados.",
                       end_text = " Por favor, selecione outro município")
    }else{
      # browser()
      if(input_seletor_ef){
        ef_muns_period <- ef_muns_ano_res
        ef_muns_ef <- ef_muns_res
      }else{
        ef_muns_period <- ef_muns_quad_proc
        ef_muns_ef <- ef_muns_proc
      }
      list_dfs <- list(ef_muns_period, ef_muns_ef)
      ## Alerta de carregamento widget
      shinyalert::shinyalert(
        html = TRUE,
        title = "Carregando novos dados...",
        showConfirmButton = F,
        text = tagList(
          HTML("<div class='loader_data'></div>")
        ),
        closeOnEsc = F,
      )
      if(F){
        cat(paste0("Tipo quad: ", tipo_quad, "\n" ,"Graph type: ", graph_type, "\n", "Title ef def: ", title_ef_def(), "\n",
                   "Input type: ", input_type, "\n", "Input sel period: ", input_sel_period, "\n", "Input sel period name: ", input_sel_period_name, "\n",
                   "Input sel uf 1: ", input_sel_uf_1, "\n", "Input sel uf 2: ", input_sel_uf_2, "\n",
                   "Input sel reg saude 1: ", input_sel_reg_saude_1, "\n", "Input sel reg saude 2: ", input_sel_reg_saude_2, "\n",
                   "Input sel mun 1: ", input_sel_mun_1, "\n", "Input sel mun 2: ", input_sel_mun_2, "\n",
                   "Flag cmp: ", flag_cmp))
      }
    }
    # browser()
    ## Se o primeiro resultado for nulo, quer dizer que não foram encontrados dados
    ## para o município
    ## Debug
    if(debug){
      promise <- "debug"
    }
    input_2 <- ifelse(input$type == "uf", "NULL",
                      ifelse(input$type == "reg_saude", input$sel_reg_saude_1, input$sel_mun_1))
    ## Checando para ver se município selecionado possui dados
    check <- func_check_has_data(df_dados_mun_uf_reg_saud_filter, input$sel_uf_1, input_2, input$type, input$seletor_ef)
    if(!isTruthy(check)){
      ## Se o município não possuir dados, exibir aviso e parar execução
      promise <- NULL
    }
    # browser()
    if(is.null(promise)){
      if(input$cmp_1){
        mod_modal_server("modal_1",
                         title = "Não há dados",
                         first_test = "O município selecionado ou o de comparação",
                         mid_text = " </b>não possui dados disponíveis na nossa base de dados.</b>",
                         end_text = " Por favor, selecione outro município ou de comparação")
      }
      mod_modal_server("modal_1",
                       title = "Não há dados",
                       first_test = "O município selecionado",
                       mid_text = " </b>não possui dados disponíveis na nossa base de dados.</b>",
                       end_text = " Por favor, selecione outro município")
    }else{
      # browser()
      ## Usado para debug
      if(debug){
        # Dados tabela
        gt_tabela <- NULL
        ## Dados mapa
        ggiraph_map <- NULL
        # ## Dados gráfico eficiência
        list_graphs_inputs_outputs <- NULL
      }
      ## Senão estiver em modo debug, pegar da resposta da promise
      if(!debug){
        # Dados tabela
        gt_tabela <- result_promise$result()[[1]][[1]]
        ## Dados mapa
        ggiraph_map <- result_promise$result()[[2]][[1]]
        # ## Dados gráfico eficiência
        list_graphs_inputs_outputs <- result_promise$result()[[3]][[1]]
      }

      ## Debug promises
      if(debug){
        # list_graphs_inputs_outputs[[1]]
        ## Inputs passados para promises
        input_seletor_ef <- input$seletor_ef
        input_type <- input$type
        input_sel_period <- input$sel_period
        input_sel_uf_1 <- input$sel_uf_1
        input_sel_reg_saude_1 <- input$sel_reg_saude_1
        input_sel_mun_1 <- input$sel_mun_1
        ## Inputs de comparação
        input_sel_uf_2 <- input$sel_uf_2
        input_sel_reg_saude_2 <- input$sel_reg_saude_2
        input_sel_mun_2 <- input$sel_mun_2
        # browser()
        input_sel_period_name <- ifelse(input$seletor_ef, input_sel_period, names(choices_quad[as.integer(input_sel_period)]))
        # input_sel_period_name <- names(choices_quad[as.integer(input_sel_period)])
        if(FALSE){
          ## Inputs passados para promises
          cat(paste0("Tipo quad: ", tipo_quad, "\n" ,"Graph type: ", graph_type, "\n", "Title ef def: ", title_ef_def(), "\n",
                     "Input type: ", input_type, "\n", "Input sel period: ", input_sel_period, "\n", "Input sel period name: ", input_sel_period_name, "\n",
                     "Input sel uf 1: ", input_sel_uf_1, "\n", "Input sel uf 2: ", input_sel_uf_2, "\n",
                     "Input sel reg saude 1: ", input_sel_reg_saude_1, "\n", "Input sel reg saude 2: ", input_sel_reg_saude_2, "\n",
                     "Input sel mun 1: ", input_sel_mun_1, "\n", "Input sel mun 2: ", input_sel_mun_2, "\n",
                     "Flag cmp: ", flag_cmp))
        }
        ## flag de comparação, não iniciada ( == NULL)
        if(is.null(input$cmp_1)){
          flag_cmp <- F
        }else{
          ## flag de comparação, iniciada mas falsa ( == F)
          if(!input$cmp_1){
            flag_cmp <- F
          }
          ## flag de comparação, iniciada verdadeira
          if(input$cmp_1){
            flag_cmp <- T
          }
        }
        list_dfs <- list(NULL, NULL)
        # browser()
        result_func <- func_applyFilters(
          tipo_quad, graph_type, title_ef_def(),
          list_dfs, con, data_from_bd,
          input_seletor_ef, input_type, input_sel_period,
          input_sel_period_name, input_sel_uf_1, input_sel_uf_2,
          input_sel_reg_saude_1, input_sel_reg_saude_2,
          input_sel_mun_1, input_sel_mun_2,
          flag_cmp
          # func_server_mod_tabela_ef, func_order_by_ef_sel,
          # func_server_mod_mapa,
          # func_get_ef_mun_data,
          # func_server_mod_graph_lollipop_inputs_outputs,
          # func_aux_graph_lollipop_input_proc_1, func_aux_graph_lollipop_input_proc_2,
          # func_aux_graph_lollipop_outputs_proc_2,
          # func_create_tooltip_ef
        )


        # browser()
        ## Tabela
        gt_tabela <- result_func[[1]][[1]]
        ## Mapa
        ggiraph_map <- result_func[[2]][[1]]
        ## Eficiência
        list_graphs_inputs_outputs <- result_func[[3]][[1]]
        flag_cmp = F
        ef_df_cmp = NULL
      }
      ### 3.1.1 Tabela do município ----
      ## Colocada aqui, porque depois das tranformações surgiram problemas no pivot_wider
      mod_tabela_ef_server("tabela_ef_1", F, gt_tabela)

      # browser()
      ##  3.1.2 Mapa da região de saúde (ef_df_reg_saude_sel) ----
      mod_mapa_server("mapa_1", initial_state = F, ggiraph_map)

      # browser()
      ## 3.1.3 Gráfico de Inputs e outputs ----
      ### Inputs e outputs
      mod_graph_lollipop_inputs_outputs_server("graph_lollipop_inputs_outputs_1",
                                               initial_state = F, ef_proc_res,
                                               list_graphs_inputs_outputs)
      shinyalert::closeAlert()
    }
  })
  # 4. Reativ. UI ----
  ## 4.1 Caixas de seleção (Mun, Reg Saud, UF) ----
  ## Buscar por:
  ## Reatividade da caixa de seleção de "Buscar por" município, região de saúde ou UF
  observeEvent(input$type, {
    # browser()
    func_selector_type_1(session, output, input$type, id_selector, uf_ordered, reg_saude_ordered)
    func_cmp_button(session, output, "uf", "")
    if(input$type == "uf" || input$type == "reg_saude"){
      ## Removendo gráfico de eficiência
      output$checkbox_filtro_eq <- NULL
      ## Removendo seleção de estado em município em comparação
      func_selector_type_2(session, output, input$type, input$cmp,
                           id_selector = NULL, uf_ordered = NULL, reg_saude_ordered = NULL,
                           df_cap_uf_ibge = NULL, sel_uf = NULL)
    }
    ## Alterando a flag para sabermos que não houve uma troca de tipo recentemente
    flag_change_type(T)
    ## Alterando a flag para sabermos que houve uma troca de UF, portanto resetamos a flag de first de mun_regsaude
    flag_mun_regsaude_first(T)
  })

  ## 4.2 UF (1a caixa pós seleção de tipo) ----
  ## Reatividade primeira caixa de seleção (para apresentar municípios ou regiões de saúde, conforme UF
  observeEvent(input$sel_uf_1, {
    # browser()
    ## UF
    if(input$type == "uf"){
      func_selector_att_type(session, output, input$sel_uf_1, "sel_uf_1", df_dados_mun_uf_reg_saud_filter, mun_regsaude = 0, df_cap_uf_ibge)
    }
    ## Município
    if(input$type == "mun"){
      # browser()
      func_selector_att_type(session, output, input$sel_uf_1, "sel_mun_1", df_dados_mun_uf_reg_saud_filter, mun_regsaude = 1, df_cap_uf_ibge)
      func_cmp_button(session, output, input$type, "outro município")
    }
    # Região de saúde
    if(input$type == "reg_saude"){
      func_selector_att_type(session, output, input$sel_uf_1, "sel_reg_saude_1", df_dados_mun_uf_reg_saud_filter, mun_regsaude = 2, df_cap_uf_ibge)
      # func_cmp_button(session, output, input$type, "outra região de saúde")
    }
    ## Alterando a flag para sabermos que não houve uma troca de tipo recentemente
    flag_change_type(F)
    ## Alterando a flag para sabermos que houve uma troca de UF, portanto resetamos a flag de first de mun_regsaude
    flag_mun_regsaude_first(T)
  })
  ## 4.3 Comparação  ----
  ## Reatividade do botão de seleção para adicionar à comparação
  observeEvent(input$cmp_1, {
    ## Se o input de comparação for verdadeiro, continuar
    if(isTruthy(input$cmp_1)){
      # browser()
      ied_mun_sel <- func_get_mun_ied(df_mun_ied, input$sel_uf_1, input$sel_mun_1)
      ## Verifica se existem dados para o município selecionado
      if(isTruthy(ied_mun_sel)){
        func_selector_type_2(session, output, input$type, input$cmp_1, id_selector, uf_ordered, reg_saude_ordered, df_cap_uf_ibge, input$sel_uf_2)
        ## Botão para seleção do filtro de comparação
        output$checkbox_filtro_eq <- renderUI({
          shinyWidgets::awesomeCheckbox(
            inputId = "filtro_eq",
            label = paste0("Filtro de equidade"),
            value = TRUE
          )
        })

        ## Apresentando modal explicando comparação entre municípios
        title <- dplyr::case_when(
          input$type == "uf" ~ "Comparação entre UFs",
          input$type == "mun" ~ "Comparação entre municípios",
          input$type == "reg_saude" ~ "Comparação entre região de saúde"
        )
        text <- dplyr::case_when(
          input$type == "uf" ~ "Lembrando que a comparação será feita com a média dos municípios das UFs comparadas.",
          input$type == "mun" ~ "O município por padrão poderá ser comparado com municípios de mesmo Índice de equidade e dimensionamento (IED),
        portanto, caso queira comparar com outros municípios, desative o filtro de equidade. Porém, recomendamos que,
        para efeitos de comparações mais justas se façam comparações apenas com municípios de mesmo IED.",
          input$type == "reg_saude" ~ "Lembrando que a comparação será feita com a média das região de saúde comparadas."
        )
        mod_modal_faixa_cmp_server("modal_faixa_cmp_1", title,
                                   text)
        ## Senão houver dados para o município selecionado, apresentar o modal com aviso
      }else{
        mod_modal_server("modal_1",
                         title = "Não há dados",
                         first_test = "O município selecionado",
                         mid_text = " não possui dados disponíveis na nossa base de dados.",
                         end_text = " Por favor, selecione outro município para que possa ser feita uma comparação.")
        updateCheckboxInput(session, "cmp_1", value = FALSE)
      }
    }else{
      # browser()
      output$checkbox_filtro_eq <- renderUI({
        NULL
      })
      func_selector_type_2(session, output, input$type, input$cmp,
                           id_selector = NULL, uf_ordered = NULL, reg_saude_ordered = NULL,
                           df_cap_uf_ibge = NULL, sel_uf = NULL)
    }
  })
  ## Reatividade da caixa de seleção de comparação (para apresentar municípios ou regiões de saúde, conforme UF)
  ## (Adiciona duas caixas p/ seleção de comparação)
  observeEvent(input$sel_uf_2, {
    # browser()
    ## Filtrando a faixa (ied)
    ied_mun_sel <- func_get_mun_ied(df_mun_ied, input$sel_uf_1, input$sel_mun_1)
    ## Município
    if(input$type == "mun"){
      ## filter_uf_ied == 10 -> Não filtrar por IED, filter_uf_ied == 11, filtrar por ied
      filter_ied_sel <- ifelse(filt_eq(), 11, 10)
      func_att_selector_type_cmp_uf_faixa(session, output, "sel_mun_2", input$sel_uf_2, df_mun_ied,
                                          mun_regsaude = 1, ied_mun_sel, filter_ied = filter_ied_sel,
                                          debug)
    }
    # Região de saúde
    if(input$type == "reg_saude"){
      # browser()
      func_selector_att_type(session, output, input$sel_uf_2, "sel_reg_saude_2", df_dados_mun_uf_reg_saud_filter, mun_regsaude = 2, df_cap_uf_ibge)
    }
  })
  ### 4.3.1 Equidade -----
  ### Reatividade botão filtro de equidade
  observeEvent(input$filtro_eq, {
    # browser()
    filt_eq <- reactiveVal(input$filtro_eq)
    ## Se botão de comparação estiver ativo
    # browser()
    if(isTruthy(input$cmp_1)){
      ## Se for comparação de municípios
      if(input$type == "mun"){
        ## Se o filtro estiver ativado, passar 11 (filtrar por ied), senão passar 10 (não filtrar)
        filter_ied_sel <- ifelse(filt_eq(), 11, 10)
        # browser()
        ## se o filtro estiver ativado, passar o ied do município selecionado, senão passar 0
        ied_mun_sel_f <- ifelse(filter_ied_sel == 11, func_get_mun_ied(df_mun_ied, input$sel_uf_1, input$sel_mun_1), 0)
        func_att_selector_type_cmp_uf_faixa(session, output, "sel_mun_2", input$sel_uf_2, df_mun_ied,
                                            mun_regsaude = 1, ied_mun_sel = ied_mun_sel_f, filter_ied = filter_ied_sel,
                                            debug)
      }
    }
    if(!isTruthy(filt_eq())){
      mod_modal_server("modal_1",
                       title = "Filtro de equidade",
                       first_test = "O filtro de equidade foi desativado. ",
                       mid_text = "Agora você poderá comparar o município selecionado com qualquer outro município. Caso deseje ativar o filtro novamente, clique no botão de comparação. ",
                       end_text = "Por favor, esteja ciente de que esta comparação não leva em conta a metodologia utilizada no trabalho para <b>a comparação</b> de municípios, portanto <b>pode não ser justa.</b>")
    }
  })

  ## Reatividade da caixa de seleção de comparação,
  ## Agora como ao mudar o município selecionado, também precisamos mudar os municípios de comparação
  ## por causa da faixa que pode mudar
  observeEvent(input$sel_mun_1, {
    ## Testando se foi a primeira vez que rodou, já que o próprio observe altera o mesmo input
    if(flag_mun_regsaude_first()){
      ## Atualizando seleção de município em caso de trocar selecionar outro tipo e depois voltar pra município
      if(flag_change_type()){
        ## Município
        if(input$type == "mun"){
          # browser()
          func_selector_att_type(session, output, input$sel_uf_1, "sel_mun_1", df_dados_mun_uf_reg_saud_filter, mun_regsaude = 1, df_cap_uf_ibge)
          func_cmp_button(session, output, input$type, "outro município")
        }
        flag_change_type(F)
      }
      if(isTruthy(input$cmp_1)){
        ## Filtrando a faixa (ied)
        ied_mun_sel_f <- func_get_mun_ied(df_mun_ied, input$sel_uf_1, input$sel_mun_1)
        ## Se o filtro estiver ativado, passar 11 (filtrar por ied), senão passar 10 (não filtrar)
        filter_ied_sel <- ifelse(input$filtro_eq, 11, 10)
        ## Município
        if(input$type == "mun"){
          # browser()
          func_att_selector_type_cmp_uf_faixa(session, output, "sel_mun_2", input$sel_uf_2, df_mun_ied,
                                              mun_regsaude = 1, ied_mun_sel_f, filter_ied_sel, debug)
        }
      }
      flag_mun_regsaude_first(F)
    }
    if(input$sel_mun_1 != ""){
      # browser()
      check <- func_check_has_data(df_dados_mun_uf_reg_saud_filter, input$sel_uf_1, input$sel_mun_1, input$type, input$seletor_ef)
      if(!check){
        mod_modal_server("modal_1",
                         title = "Não há dados",
                         first_test = "O município selecionado",
                         mid_text = " <b>não possui dados disponíveis na nossa base de dados*.</b>",
                         end_text = " Por favor, selecione outro município.")

      }
    }
  })

  observeEvent(input$sel_reg_saude_1, {
    ## Testando se foi a primeira vez que rodou, já que o próprio observe altera o mesmo input
    if(flag_mun_regsaude_first()){
      ## Atualizando seleção de região de saúde em caso de trocar selecionar outro tipo e depois voltar pra região de saúde
      if(flag_change_type()){
        # Região de saúde
        if(input$type == "reg_saude"){
          func_selector_att_type(session, output, input$sel_uf_1, "sel_reg_saude_1", df_dados_mun_uf_reg_saud_filter, mun_regsaude = 2, df_cap_uf_ibge)
          # func_cmp_button(session, output, input$type, "outra região de saúde")
        }
        flag_change_type(F)
      }
      flag_mun_regsaude_first(F)
    }
    if(input$sel_reg_saude_1 != ""){
      check <- func_check_has_data(df_dados_mun_uf_reg_saud_filter, input$sel_uf_1, input$sel_reg_saude_1, input$type, input$seletor_ef)
      if(!isTruthy(check)){
        mod_modal_server("modal_1",
                         title = "Não há dados",
                         first_test = "A região de saúde selecionada",
                         mid_text = " <b>não possui dados disponíveis na nossa base de dados*.</b>",
                         end_text = " Por favor, selecione outra região de saúde.")

      }
    }
  })

  observeEvent(input$sel_mun_2, {
    if(input$sel_mun_2 != ""){
      # browser()
      check <- func_check_has_data(df_dados_mun_uf_reg_saud_filter, input$sel_uf_2, input$sel_mun_2, input$type, input$seletor_ef)
      if(!check){
        mod_modal_server("modal_1",
                         title = "Não há dados",
                         first_test = "O município selecionado",
                         mid_text = " </b>não possui dados disponíveis na nossa base de dados*.</b>",
                         end_text = " Por favor, selecione outro município.")

      }
    }
  })
  ## 4.5 Modal Comparação com faixa ----
  # observeEvent(input$cmp_1, {
  #
  # })


  # 5. Filtros reativos ----
  mod_filters_server("filters_1")
  ## 5.1 Download dados (Modal) ----
  observeEvent(input$downloadData, {
    if(input$type == "no_sel"){
      showModal(
        modalDialog(
          title = "Para fazer o download, primeiro selecione um tipo de busca",
          easyClose = TRUE,
          footer = NULL,
        )
      )
    }else{
      ## Se for UF, vai mostrar apenas dois botões, da UF e geral
      if(input$type == "uf"){
        showModal(div(class = "modal-download", modalDialog(
          title = "Selecione os dados para download",
          easyClose = TRUE,
          footer = NULL,
          # size = "s",
          div(
            class="row",
            div(
              class="col-6 d-flex justify-content-center",
              shinyWidgets::downloadBttn(
                outputId = "downloadFilteredUF",
                label = HTML(paste0("Baixar dados", "<br>", "da UF")),
                # label = HTML(paste0("Baixar dados da UF")),
                style = "material-flat",
                size = "md",
                color = "success"
              )
            ),
            div(
              class="col-6 d-flex justify-content-center",
              shinyWidgets::downloadBttn(
                outputId = "downloadAllData",
                label = HTML(paste0("Baixar todos", "<br>", "os dados")),
                # label = HTML(paste0("Baixar todos os dados")),
                style = "material-flat",
                size = "md",
                color = "success"
              )
            )
          ),
        )))
        ## Se for mun ou reg de saúde, vai mostrar trê botões, do município/da reg saúde, da UF e geral
      }else{
        reg_sel <- ifelse(input$type == "mun", " do município", " da região de saúde")
        showModal(div(class = "modal-download", modalDialog(
          title = "Selecione os dados para download",
          easyClose = TRUE,
          footer = NULL,
          # size = "s",
          div(
            class="row",
            div(
              class="col-4 d-flex justify-content-center",
              shinyWidgets::downloadBttn(
                outputId = "downloadFilteredData",
                label = HTML(paste0("Baixar dados", "<br>", reg_sel)),
                # label = HTML(paste0("Baixar dados ", reg_sel)),
                style = "material-flat",
                size = "md",
                color = "success"
              )
            ),
            div(
              class="col-4 d-flex justify-content-center",
              shinyWidgets::downloadBttn(
                outputId = "downloadFilteredUF",
                label = HTML(paste0("Baixar dados", "<br>", "da UF")),
                # label = HTML(paste0("Baixar dados da UF")),
                style = "material-flat",
                size = "md",
                color = "success"
              )
            ),
            div(
              class="col-4 d-flex justify-content-center",
              shinyWidgets::downloadBttn(
                outputId = "downloadAllData",
                label = HTML(paste0("Baixar todos", "<br>", "os dados")),
                # label = HTML(paste0("Baixar todos os dados")),
                style = "material-flat",
                size = "md",
                color = "success"
              )
            )
          ),
        )))
      }
    }
  })

  ## 5.2 Gráficos iniciais ----
  ### Título da caixa do mapa

  observeEvent(input$seletor_ef, {
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
  ## Versões reativas
  observeEvent(input$applyFilters,{
    ## Atualizando nome do período
    output$title_map <- renderUI({
      div(class="titles-graph title-map",
          span(HTML(paste0("Eficiência de <b>", title_ef_def(), "</b> ", title_map_tab_loc()))),
          br(),
          span(class = "subtitle", ifelse(!input$seletor_ef, "No período ", "No ano "), title_period())
      )
    })
    ### Título do gráfico de eficiência
    output$title_ef <- renderUI({
      div(class="titles-graph title-ef",
          span(HTML(paste0("Métricas de Eficiência "))),
          span(class="title-mun-sel", HTML(paste0(title_ef_loc()))),
          br(),
          span(class = "subtitle", ifelse(!input$seletor_ef, "No período ", "No ano "), title_period())
      )
    })
    if(initial_state()){
      # browser()
      ## 5.2.1 Gráfico de eficiência ----
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
        func_selector_att_type(session, output, input$sel_uf_1, "sel_reg_saude_1", df_dados_mun_uf_reg_saud_filter, mun_regsaude = 2, df_cap_uf_ibge)
        # func_cmp_button(session, output, input$type, "outra região de saúde")
        output$box_graf_ef <- renderUI({
          NULL
        })
      }
    }
  })

  # 6. Reiniciar gráficos (botão) ----
  observeEvent(input$resetFilters, {
    ## Resetar filtros
    input_type <- "no_sel"
    ### Função para resetar filtros
    func_reset_filters(session, output, input_type)
    ### Alterando a flag para sabermos que não houve uma troca de tipo recentemente
    flag_change_type(T)
    ### Alterando a flag para sabermos que houve uma troca de UF, portanto resetamos a flag de first de mun_regsaude
    flag_mun_regsaude_first(T)
    ## Resetar gráficos
    ### Reiniciando estado inicial
    initial_state(T)
    ef_proc_res <- input$seletor_ef
    ### Função para resetar gráficos
    func_reset_graphs(session, output, initial_state(), ef_proc_res)
  })

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
  ## Informações sobre usuários se conectando
  observe({
    cat(sprintf("[%s] Connection details:\n", format(Sys.time())))
    cat(sprintf("- IP Address: %s\n", session$request$REMOTE_ADDR))
    cat(sprintf("- User Agent: %s\n", session$request$HTTP_USER_AGENT))
  })
}
