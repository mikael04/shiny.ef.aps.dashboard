#' create_tooltip_ef
#'
#' @description Uma função para adicionar uma coluna com tooltip e o nome do eixo
#' ao dataframe de eficiência
#'
#' @return Retorna o dataframe recebido com a nova coluna de tooltip e o nome do eixo
#'
#' @noRd
func_create_tooltip_ef <- function(ef_df_tooltip, graph_type, ef, flag_cmp, i, input_sel_period_name,
                                   nome_area, nome_area_cmp,
                                   in_out_flag, in_out_names, in_out_names_clean,
                                   cols_names, cols_jump){
  if(F){
    ef_df_tooltip <- ef_df_mun_sel
    graph_type <- 2
    ef <- F
    i <- 2
    input_sel_period_name <- input_sel_period_name
    nome_area <- ef_df_tooltip$nome_mun
    ## Se true, input, se false, output
    in_out_flag <- F
    in_out_names <- ifelse(in_out_flag, input_names, output_names)
    in_out_clean_names <- ifelse(in_out_flag, inputnames_clean, output_names_clean)
    cols_names <- cols_names
    cols_jump <- cols_jump
    nome_area_cmp <- ef_df_tooltip$cmp_nome_mun
  }
  first_col <- ifelse(in_out_flag, i-1, i-3)
  browser()
  ## ef == T se já for eficiente
  if(ef){
    ef_return <- ef_df_tooltip |>
      dplyr::mutate(
        tooltip_col = paste0(
          "Região: ", "<b>", nome_area, "</b>", "\n",
          "Período: ", "<b>", input_sel_period_name, "</b>", "\n",
          "Índice de ", ifelse(in_out_flag == T, "entrada: ",  "saída: "),
          "<b>", in_out_names_clean[first_col], "</b>", ".\n",
          "Valor atual", ifelse(graph_type == 2, ": ", " médio: "),
          "<b>", round(!!as.name(cols_names[i]), 2), "</b>", "\n",
          "A região <b>já é eficiente nesta área.</br>"),
        eixo_x = in_out_names[first_col])
  }
  ## ef == F se não for eficiente
  if(!ef){
    ## in_out_names i-3, temos 3 colunas iniciais, região geográfica, ef_bcc, quad_cod
    ef_return <- ef_df_tooltip |>
      dplyr::mutate(
        tooltip_col = paste0(
          "Região: ", "<b>", nome_area, "</b>", "\n",
          "Período: ", "<b>", input_sel_period_name, "</b>", "\n",
          "Índice de ", ifelse(in_out_flag == T, "entrada: ",  "saída: "),
          "<b>", in_out_names_clean[first_col], "</b>", ".\n",
          "Valor para alcançar eficiência: ", "<b>", round((!!as.name(cols_names[i])+!!as.name(cols_names[i+cols_jump])), 2), "</b>", "\n",
          "Valor atual", ifelse(graph_type == 2, ": ", " médio: "),
          "<b>", round(!!as.name(cols_names[i]), 2), "</b>", "\n"),
        eixo_x = in_out_names[first_col])
  }
  if(flag_cmp){
    ef_return <- ef_return |>
      dplyr::mutate(
        tooltip_col = paste0(
          tooltip_col, "\n",
          "Município de comparação: ", "<b>", nome_area_cmp, "</b>", "\n",
          "Valor atual", ifelse(graph_type == 2, ": ", " médio: "),
          "<b>", round(!!as.name(cols_names[i+cols_jump*2+1]), 2), "</b>", "\n"
        )
      )

  }
  return(ef_return)
}
