#' selector_type_1
#'
#' @description A fct function that manipulates the first dynamic selector
#'
#' @return No return
#'
#' @noRd
func_selector_type_1 <- function(session, output, type, id_selector, uf_ordered, reg_saude_ordered,
                                 df_cap_uf_ibge){
  if(type != "no_sel"){
    teste_interno <- F
    if(teste_interno){
      # dados_munic_uf_regiao_regsaude <- read.csv("data-raw/dados_territorio/dados_territorio.csv")
      uf_ordered <- sort(unique(dados_munic_uf_regiao_regsaude$nome_uf))
      reg_saude_ordered <- sort(unique(dados_munic_uf_regiao_regsaude$nome_rs))

    }
    ## UF ----
    ## Se for selecionado UF
    if(type == "uf"){
      # dados_munic_uf_regiao_regsaude <- read.csv("data/dados_territorio/dados_territorio.csv")
      output$selector_type_1_uf_sel <- renderUI({
        selectizeInput(
          inputId = "sel_uf_1",
          label = "Selecione o estado",
          choices = uf_ordered,
          selected = "Acre",
          multiple = F,
          width = "100%"
        )
      })
      output$selector_type_1_mun_regs_sel <- renderUI({
        NULL
      })

      # output$selector_type_3 <- renderUI({
      #   NULL
      # })
    }
    ## Município ----
    ## Se for selecionado município
    ## mun, uf, regiao, reg_saude, macro_reg_saude
    if(type == "mun"){
      # dados_munic_uf_regiao_regsaude <- read.csv("data/dados_territorio/dados_territorio.csv")
      output$selector_type_1_uf_sel <- renderUI({
        selectizeInput(
          inputId = "sel_uf_1",
          label = "Selecione o estado do município",
          choices = uf_ordered,
          selected = "Acre",
          multiple = F,
          width = "100%"
        )
      })
      output$selector_type_1_mun_regs_sel <- renderUI({
        div(
          id="sel-1-selectize",
          selectizeInput(
            inputId = "sel_mun_1",
            label = "Selecione o seu município",
            choices = NULL,
            selected = NULL,
            multiple = F,
            width = "100%"
          )
        )
      })
      # output$selector_type_3 <- renderUI({
      #   selectizeInput(
      #     inputId = "sel_mun_cmp",
      #     label = "Selecione o município de comparação",
      #     choices = NULL,
      #     selected = NULL,
      #     multiple = F
      #   )
      # })
    }
    ## Região de saúde ----
    ## Se for selecionado região de saúde
    if(type == "reg_saude"){
      # dados_munic_uf_regiao_regsaude <- read.csv("data/dados_territorio/dados_territorio.csv")
      output$selector_type_1_uf_sel <- renderUI({
        selectizeInput(
          inputId = "sel_uf_1",
          label = "Selecione o estado da região de saúde",
          choices = uf_ordered,
          selected = NULL,
          multiple = F,
          width = "100%"
        )
      })
      output$selector_type_1_mun_regs_sel <- renderUI({
        div(
          id="sel-1-selectize",
          selectizeInput(
            inputId = "sel_reg_saude_1",
            label = "Selecione a Região de Saúde",
            choices = NULL,
            selected = NULL,
            multiple = F,
            width = "100%"
          )
        )
      })
      # output$selector_type_3 <- renderUI({
      #   selectizeInput(
      #     inputId = "sel_mun_cmp",
      #     label = "Selecione  a Região de Saúde de comparação",
      #     choices = NULL,
      #     selected = NULL,
      #     multiple = F
      #   )
      # })
    }

    ## Macro Região de saúde ----
    ## Se for selecionado macro região de saúde
    # if(type == "macro_reg_saude"){
    #   # dados_munic_uf_regiao_regsaude <- read.csv("data/dados_territorio/dados_territorio.csv")
    #   output$selector_type_1 <- renderUI({
    #     selectizeInput(
    #       inputId = "sel_reg_saude",
    #       label = "Selecione o estado do município",
    #       choices = dados_munic_uf_regiao_regsaude$nome_macroregiao,
    #       multiple = F
    #     )
    #   })
    # }
  }else{
    output$selector_type_1_uf_sel <- renderUI({
      NULL
    })
    output$selector_type_1_mun_regs_sel <- renderUI({
      NULL
    })
    # output$selector_type_3 <- renderUI({
    #   NULL
    # })
  }
}
