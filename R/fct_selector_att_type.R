#' selector_att_type
#'
#' @description A fct function that manipulates the seccond dynamic selector
#'
#' @return No return
#'
#' @noRd
func_selector_att_type <- function(session, output, sel_uf, id_selector, dados_munic_uf_regiao_regsaude, mun_regsaude, df_cap_uf_ibge){
  teste_interno <- F
  if(teste_interno){
    # dados_munic_uf_regiao_regsaude <- read.csv("data-raw/dados_territorio/dados_territorio.csv")
  }
  # browser()
  ## Município ----
  if(mun_regsaude == 1){
    ## Buscando a capital
    capital_sel <- func_nome_capital(df_cap_uf_ibge, sel_uf)
    ## Filtrando dados recebidos
    # dados_filtered <- dados_munic_uf_regiao_regsaude[dados_munic_uf_regiao_regsaude$nome_uf == sel_uf,]$nome_mun
    dados_filtered <- dados_munic_uf_regiao_regsaude |>
      dplyr::filter(nome_uf == sel_uf) |>
      dplyr::pull(nome_mun)
    ## Ordenando os nomes recebidos
    dados_filtered <- sort(dados_filtered)
    ## Atualizando os municípios conforme seleção do estado (UF)
    updateSelectizeInput(
      session,
      id_selector,
      choices = dados_filtered,
      selected = capital_sel,
      options = list(
        placeholder = "Selecione ou Digite o seu município",
        plugins = list("remove_button"),
        score = I("function(search) {
                      return function(item) {
                        return item.label.toLowerCase().startsWith(search) ? 1 : 0 ;
                      };
                    }"
        )
      )
    )
  }

  ## Região de saúde ----
  if(mun_regsaude == 2){
    # browser()
    dados_filtered <- dados_munic_uf_regiao_regsaude |>
      dplyr::filter(nome_uf == sel_uf) |>
      dplyr::pull(nome_rs)
    ## Ordenando os nomes recebidos
    dados_filtered <- sort(dados_filtered)
    ## Atualizando os municípios conforme seleção do estado (UF)
    updateSelectizeInput(session,
                         id_selector,
                         choices = dados_filtered,
                         selected = NULL)
  }
}
