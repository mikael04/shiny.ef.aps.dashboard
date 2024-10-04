#' selector_type_cmp_faixa
#'
#' @description Uma função que atualizará o segundo seletor de comparação conforme UF recebida
#' e faixa (ied)
#'
#' @return No return
#'
#' @noRd
func_att_selector_type_cmp_uf_faixa <- function(session, output, id_selector, sel_uf, df_mun_ied, mun_regsaude, ied_mun_sel, filter_ied, debug) {
  teste_interno <- F
  if(teste_interno){
    # dados_munic_uf_regiao_regsaude <- read.csv("data-raw/dados_territorio/dados_territorio.csv")
  }
  # browser()
  ## Município ----
  if(mun_regsaude == 1){
    ## Filtrando dados recebidos por uf_mun_cmp e ied do mun_sel
    if(filter_ied == 11){
      # ifelse(debug, print("Filtrando IED"), NULL)
      muns_filter <- df_mun_ied |>
        dplyr::filter(nome_uf == sel_uf,
                      ied == ied_mun_sel) |>
        dplyr::pull(nome_mun)
    }
    if(filter_ied == 10){
      # ifelse(debug, print("Não filtrando IED"), NULL)
      muns_filter <- df_mun_ied |>
        dplyr::filter(nome_uf == sel_uf) |>
        dplyr::pull(nome_mun)
    }
    ## Ordenando os nomes recebidos
    muns_filter <- sort(muns_filter)
    ## Atualizando os municípios conforme seleção do estado (UF)
    updateSelectizeInput(
      session,
      id_selector,
      choices = muns_filter,
      selected = muns_filter[1],
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
    dados_filtered <- dados_munic_uf_regiao_regsaude[dados_munic_uf_regiao_regsaude$nome_uf == sel_uf,]$nome_rs
    ## Ordenando os nomes recebidos
    dados_filtered <- sort(dados_filtered)
    ## Atualizando os municípios conforme seleção do estado (UF)
    updateSelectizeInput(session,
                         id_selector,
                         choices = dados_filtered,
                         selected = NULL)
  }
}
