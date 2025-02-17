#' aux_graph_lollipop_input_proc_2
#'
#' @description Função para lidar com tipos de gráficos e gerar gráfico de processos do tipo pirulito para entrada 1
#'
#' @return Retornará um gráficos de entrada que servirá para os inputs no patchwork
#'
#' @noRd
func_aux_graph_lollipop_input_proc_2 <- function(
    graph_type, input_sel_period_name, ef_df,
    flag_cmp,
    ef_df_cmp, ef_proc_res){

  ## Nomes dos índices
  input_names <- c("Despesa por \nhabitante coberto", "Equipe por 3.500 \nhabitante coberto")
  input_names_clean <- gsub("\n", "", input_names)
  ## flag input para tooltip
  in_out_flag <- T

  ## Fazendo ggplot base
  graph_lollipop <- ggplot2::ggplot() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggtext::element_markdown(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    ) +
    ggplot2::xlab("") +
    ggplot2::ylab("")

  if(graph_type == 0){
    ef_df_br <- ef_df |>
      dplyr::select(c(1, 5:8))

    cols_names <- colnames(ef_df_br)
    # Gráfico de Equipe por 3.500 habitantes coberto -----
    i <- 3
    ## Quantas colunas terá que pular para ter o valor que falta para eficiência
    cols_jump <- 2
    ## Gerando tooltips
    ef_df_br <- func_create_tooltip_ef(
      ef_df_br, graph_type = 0, ef = F, flag_cmp = F, i, "3° Quad/2023",
      "Brasil", "",
      in_out_flag, input_names, input_names_clean,
      cols_names, cols_jump)


    if(flag_cmp){
      ## Selecionando colunas de output
      ef_df_cmp_ <- ef_df_cmp |>
        dplyr::select(c(2, 12:18))

      ## Nome das colunas
      cols_names_cmp <- colnames(ef_df_cmp_)

      ## Renomeando para não ter conflito
      ef_df_cmp_ <- ef_df_cmp_ |>
        dplyr::rename_with(~paste0("cmp_", cols_names_cmp), everything())

      ## Adicionando colunas ao df do mun selecionado
      ef_df_br <- cbind(ef_df_br, ef_df_cmp_)
    }

    ## Gráfico
    graph_lollipop_eq <- graph_lollipop +
      ggiraph::geom_segment_interactive(data = ef_df_br, aes(x = eixo_x, xend = eixo_x,
                                                             y = !!as.name(cols_names[i]), yend = (!!as.name(cols_names[i])+!!as.name(cols_names[i+cols_jump])), tooltip = tooltip_col),
                                        color = "grey", inherit.aes = F) +
      ggiraph::geom_point_interactive(data = ef_df_br, aes(x = eixo_x, y = !!as.name(cols_names[i]), tooltip = tooltip_col),
                                      shape = 21, colour = "black", fill = "#006400", size = 4, inherit.aes = F) +
      ggiraph::geom_point_interactive(data = ef_df_br, aes(x = eixo_x, y = (!!as.name(cols_names[i])+!!as.name(cols_names[i+cols_jump])), tooltip = tooltip_col),
                                      shape = 21, colour = "black", fill = "#014c84", size = 4, inherit.aes = F) +
      ggplot2::expand_limits(x = 0, y = 0)

    # graph_lollipop
    # ggiraph::girafe(ggobj = graph_lollipop)
  }
  if(graph_type == 2){
    # browser()
    ef_df_mun_sel <- ef_df |>
      dplyr::select(c(2, 8:11))

    if(flag_cmp){
      ## Selecionando colunas de output
      ef_df_mun_cmp <- ef_df_cmp |>
        dplyr::select(c(2, 8:11))

      ## Nome das colunas
      cols_names_cmp <- colnames(ef_df_mun_cmp)

      ## Renomeando para não ter conflito
      ef_df_mun_cmp <- ef_df_mun_cmp |>
        dplyr::rename_with(~paste0("cmp_", cols_names_cmp), everything())

      ## Adicionando colunas ao df do mun selecionado
      ef_df_mun_sel <- cbind(ef_df_mun_sel, ef_df_mun_cmp)
    }

    cols_names <- colnames(ef_df_mun_sel)

    ## Gráfico de Equipe por 3.500 habitantes coberto -----
    i <- 3
    ## Quantas colunas terá que pular para ter o valor que falta para eficiência
    cols_jump <- 2
    ## Aqui comparamos se existe um valor que a área geográfica precisa melhorar para o output
    ## Se == 0, significa que já é eficiente
    # browser()
    if(ef_df_mun_sel[[1, i+2]] == 0){
      # ef_df_mun_sel <- ef_df_mun_sel |>
      #   dplyr::mutate(tooltip_col = paste0("Município: ", "<b>", nome_mun, "</b>", "\n",
      #                                      "Valor atual: ", round(!!as.name(cols_names[i]),2), "</b>", "\n",
      #                                      "O município já é eficiente nesta área."
      #   ),
      #   eixo_x = input_names[i-1])

      ## ef == T se já for eficiente
      ef_df_mun_sel <- func_create_tooltip_ef(
        ef_df_mun_sel, graph_type, ef = T, flag_cmp, i, input_sel_period_name,
        ef_df_mun_sel$nome_mun, ef_df_mun_sel$cmp_nome_mun,
        in_out_flag, input_names, input_names_clean, cols_names, cols_jump)

      ## Gráfico
      graph_lollipop_eq <- graph_lollipop +
        ggiraph::geom_point_interactive(data = ef_df_mun_sel, aes(x = eixo_x, y = !!as.name(cols_names[i]), tooltip = tooltip_col),
                                        shape = 21, colour = "black", fill = "#026AA7", size = 4, inherit.aes = F) +
        ggplot2::expand_limits(x = 0, y = 0)
    }
    ## Se != 0, significa que não é eficiente e precisa melhorar
    if(ef_df_mun_sel[[1, i+2]] != 0){
      # ef_df_mun_sel <- ef_df_mun_sel |>
      #   dplyr::mutate(tooltip_col = paste0("Município: ", "<b>", nome_mun, "</b>", "\n",
      #                                      "índice de entrada: ", "<b>", input_names_clean[i-1], "</b>", ".\n",
      #                                      "Valor para alcançar a eficiência: ", "</b>", round((!!as.name(cols_names[i])+!!as.name(cols_names[i+2])), 2), "</b>", "\n",
      #                                      "Valor atual: ", "<b>", round(!!as.name(cols_names[i]),2), "</b>"),
      #                 eixo_x = input_names[i-1])

      ## ef == F se já for eficiente
      ef_df_mun_sel <- func_create_tooltip_ef(
        ef_df_mun_sel, graph_type, ef = F, flag_cmp, i, input_sel_period_name,
        ef_df_mun_sel$nome_mun, ef_df_mun_sel$cmp_nome_mun,
        in_out_flag, input_names, input_names_clean, cols_names, cols_jump)

      ## Gráfico
      graph_lollipop_eq <- graph_lollipop +
        ggiraph::geom_segment_interactive(data = ef_df_mun_sel, aes(x = eixo_x, xend = eixo_x,
                                                                    y = !!as.name(cols_names[i]), yend = (!!as.name(cols_names[i])+!!as.name(cols_names[i+cols_jump])), tooltip = tooltip_col),
                                          color = "grey", inherit.aes = F) +
        ggiraph::geom_point_interactive(data = ef_df_mun_sel, aes(x = eixo_x, y = !!as.name(cols_names[i]), tooltip = tooltip_col),
                                        shape = 21, colour = "black", fill = "#006400", size = 4, inherit.aes = F) +
        ggiraph::geom_point_interactive(data = ef_df_mun_sel, aes(x = eixo_x, y = (!!as.name(cols_names[i])+!!as.name(cols_names[i+cols_jump])), tooltip = tooltip_col),
                                        shape = 21, colour = "black", fill = "#014c84", size = 4, inherit.aes = F) +
        ggplot2::expand_limits(x = 0, y = 0)
    }

    if(flag_cmp){
      graph_lollipop_eq <- graph_lollipop_eq +
        ggiraph::geom_point_interactive(data = ef_df_mun_sel,
                                        aes(x = eixo_x, y = !!as.name(cols_names[i+cols_jump*2+1]), tooltip = tooltip_col),
                                        position = position_jitter(height = 0.00001),
                                        shape = 21, colour = "black", fill = "#FF8C00", size = 4, inherit.aes = F) +
        ggplot2::expand_limits(x = 0, y = 0)
      # ggiraph::girafe(ggobj = graph_lollipop_eq)
    }
  }
  return(graph_lollipop_eq)
}
