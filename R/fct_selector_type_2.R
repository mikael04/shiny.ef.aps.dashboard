#' selector_type_2
#'
#' @description A fct function that manipulates the seccond dynamic selector
#'
#' @return No return
#'
#' @noRd
func_selector_type_2 <- function(session, output, type, cmp_flag, id_selector, uf_ordered, reg_saude_ordered, df_cap_uf_ibge, sel_uf){
  if(type != "no_sel" & isTruthy(cmp_flag)){
    ## Se a flag do comparador for falsa
    if(!cmp_flag){
      output$selector_type_2_uf_sel <- renderUI({
        NULL
      })
      output$selector_type_2_mun_regs_sel <- renderUI({
        NULL
      })
    }
    ## Se a flag do comparador for verdadeira
    if(cmp_flag){
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
        output$selector_type_2_uf_sel <- renderUI({
          selectizeInput(
            inputId = "sel_uf_2",
            label = "Selecione o estado",
            choices = uf_ordered,
            selected = NULL,
            multiple = F,
            width = "100%"
          )
        })
        output$selector_type_2_mun_regs_sel <- renderUI({
          NULL
        })
      }
      ## Município ----
      ## Se for selecionado município
      ## mun, uf, regiao, reg_saude, macro_reg_saude
      if(type == "mun"){
        ## Buscando a capital para preencher o valor selecionado
        capital_sel <- func_nome_capital(df_cap_uf_ibge, "Acre")

        output$selector_type_2_uf_sel <- renderUI({
          selectizeInput(
            inputId = "sel_uf_2",
            label = "Selecione o estado",
            choices = uf_ordered,
            selected = "Acre",
            multiple = F,
            width = "100%"
          )
        })
        output$selector_type_2_mun_regs_sel <- renderUI({
          selectizeInput(
            inputId = "sel_mun_2",
            label = "Selecione o município",
            choices = NULL,
            selected = capital_sel,
            multiple = F,
            width = "100%"
          )
        })
      }
      ## Região de saúde ----
      ## Se for selecionado região de saúde
      if(type == "reg_saude"){
        # dados_munic_uf_regiao_regsaude <- read.csv("data/dados_territorio/dados_territorio.csv")
        output$selector_type_2_uf_sel <- renderUI({
          selectizeInput(
            inputId = "sel_uf_2",
            label = "Selecione o estado",
            choices = uf_ordered,
            selected = NULL,
            multiple = F,
            width = "100%"
          )
        })
        output$selector_type_2_mun_regs_sel <- renderUI({
          selectizeInput(
            inputId = "sel_reg_saude_2",
            label = "Selecione a Região de Saúde",
            choices = NULL,
            selected = NULL,
            multiple = F,
            width = "100%"
          )
        })
      }
    }
    ## Se não houver sido selecdionado um tipo de busca, ou se o comparador ainda não houver sido criado
  }else{
    output$selector_type_2_uf_sel <- renderUI({
      NULL
    })
    output$selector_type_2_mun_regs_sel <- renderUI({
      NULL
    })
  }
}
