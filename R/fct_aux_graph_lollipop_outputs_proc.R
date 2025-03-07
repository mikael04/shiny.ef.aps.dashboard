#' aux_graph_lollipop_outputs_proc
#'
#' @description Função para lidar com tipos de gráficos e gerar gráfico processos do tipo de pirulito para saídas
#'
#' @return Retornando um gráfico de output para o gráfico de processos
#'
#' @noRd
func_aux_graph_lollipop_outputs_proc <- function(graph_type, input_sel_period_name, ef_df,
                                                 flag_cmp, ef_df_cmp, ef_proc_res){
  ## Nomes dos índices
  # browser()
  if(F){
    graph_type = 0
    input_sel_period_name = "3° Quad/2023"
    ef_df = ef_br_f
    flag_cmp = F
    ef_df_cmp = NULL
  }
  output_names <- c(
    "1 - Ind. \nPré-natal", "2 - Ind. \nTeste \nrápido", "3 - Ind. \nOdonto",
    "4 - Ind. \nCito", "5 - Ind. \nVacina", "6 - Ind. \nHiper", "7 - Ind. \nDiab")
  output_names_clean <- gsub("\n", "", output_names)
  ## flag output para tooltip
  in_out_flag <- F

  ## Gráfico de eficiencia do Brasil ----
  if(graph_type == 0){
    # browser()
    ## Selecionando colunas de output e último quadrimestre
    ef_df_br <- ef_df |>
      # dplyr::filter(quad_cod == quad_sel) |>
      dplyr::select(1, starts_with("ind"), starts_with("v_ind"))

    cols_names <- colnames(ef_df_br)

    graph_lollipop <- ggplot() +
      ggplot2::theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = ggtext::element_markdown(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
      ) +
      xlab("") +
      ylab("") +
      labs(
        title = "Saídas"
      )

    ## Quantas colunas precisa pular para comparar com o seu próprio "v_", isso também indica o número de colunas de output
    cols_jump <- 7
    cols = ncol(ef_df_br)-cols_jump

    # cols_names <- colnames(list_ef_inicial_br_out[[1]][1:15]) # 15 é o número de colunas de saídas removendo tooltip e eixo_x

    ## Nome das colunas
    cols_names <- colnames(ef_df_br)

    ## parâmetro que irá definir qual a primeira coluna de output
    starts_in <- 2

    # browser()
    ## Criando comparações dinamicamente
    for(i in starts_in:cols){
      ## Gerando tooltips
      ef_df_br <- func_create_tooltip_ef(
        ef_df_br, graph_type = 0, ef = F, flag_cmp = F, i, "3° Quad/2023",
        "Brasil", "",
        in_out_flag, output_names, output_names_clean,
        cols_names, cols_jump)

      graph_lollipop <- graph_lollipop +
        ggiraph::geom_segment_interactive(data = ef_df_br, aes(x = eixo_x, xend = eixo_x,
                                                               y = !!as.name(cols_names[i]), yend = (!!as.name(cols_names[i])+!!as.name(cols_names[i+cols_jump])), tooltip = tooltip_col),
                                          color = "grey", inherit.aes = F) +
        ggiraph::geom_point_interactive(data = ef_df_br, aes(x = eixo_x, y = !!as.name(cols_names[i]), tooltip = tooltip_col),
                                        shape = 21, colour = "black", fill = "#006400", size = 4, inherit.aes = F) +
        ggiraph::geom_point_interactive(data = ef_df_br, aes(x = eixo_x, y = (!!as.name(cols_names[i])+!!as.name(cols_names[i+cols_jump])), tooltip = tooltip_col),
                                        shape = 21, colour = "black", fill = "#014c84", size = 4, inherit.aes = F) +
        ggplot2::expand_limits(x = 0, y = 0)
    }
    # ggiraph::girafe(ggobj = graph_lollipop)
  }
  ## Gráfico de eficiencia do município ----
  if(graph_type == 2){
    # browser()
    ## Selecionando colunas de output
    ef_df_mun_sel <- ef_df |>
      dplyr::select(nome_area = 2, starts_with("ind"), starts_with("v_ind"))

    if(flag_cmp){
      ## Selecionando colunas de output
      ef_df_mun_cmp <- ef_df_cmp |>
        dplyr::select(nome_area = 2, starts_with("ind"))

      ## Nome das colunas
      cols_names_cmp <- colnames(ef_df_mun_cmp)

      ## Renomeando para não ter conflito
      ef_df_mun_cmp <- ef_df_mun_cmp |>
        dplyr::rename_with(~paste0("cmp_", cols_names_cmp), everything())

      ## Adicionando colunas ao df do mun selecionado
      ef_df_mun_sel <- cbind(ef_df_mun_sel, ef_df_mun_cmp)
    }

    ## Nome das colunas
    cols_names <- colnames(ef_df_mun_sel)
    # cols_names
    ## Fazendo ggplot base
    graph_lollipop <- ggplot() +
      # coord_flip()+
      # hrbrthemes::theme_ipsum() +
      ggplot2::theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = ggtext::element_markdown()
      ) +
      xlab("") +
      ylab("") +
      labs(
        title = "Saídas"
      )

    ## Colunas para pular,  para avaliar valor de referência e valor de eficiência
    cols_jump <- 7
    cols = ncol(ef_df_mun_sel)-cols_jump
    if(flag_cmp){
      cols = cols-length(cols_names_cmp)
    }

    ## parâmetro que irá definir qual a primeira coluna de output
    starts_in <- 2

    ## Criando camadas de linhas e pontos conforme colunas de outputs
    for(i in starts_in:cols){
      # i <- 2
      ## Comparando com v_ para verificar se município já é eficiente
      if(ef_df_mun_sel[[1, i+cols_jump]] == 0){
        # print("Município eficiente")
        # ef_df_mun_sel <- ef_df_mun_sel |>
        #   dplyr::mutate(tooltip_col = paste0("Município: ", "<b>", nome_mun, "</b>", "\n",
        #                                      "Valor atual: ", "<b>", round(!!as.name(cols_names[i]) ,2), "</b>", "\n",
        #                                      "O município já é eficiente em", "<b>", output_names_clean[i-1], "</b>", "."
        #   ),
        #   eixo_x = output_names[i-1])

        ## ef == T se já for eficiente
        ef_df_mun_sel <- func_create_tooltip_ef(
          ef_df_mun_sel, graph_type, ef = T, flag_cmp, i, input_sel_period_name,
          ef_df_mun_sel$nome_area, ef_df_mun_sel$cmp_nome_area,
          in_out_flag, output_names, output_names_clean, cols_names, cols_jump)

        ## Montando gráfico com única camada, pois município já é eficiente e não precisa ser comparado
        graph_lollipop <- graph_lollipop +
          ggiraph::geom_point_interactive(data = ef_df_mun_sel, aes(x = eixo_x, y = !!as.name(cols_names[i]), tooltip = tooltip_col),
                                          shape = 21, colour = "black", fill = "#026AA7", size = 4, inherit.aes = F) +
          ggplot2::expand_limits(x = 0, y = 0)
      }

      ## Município não eficiente, precisa ser comparado com valor de referência
      if(ef_df_mun_sel[[1, i+2]] != 0){
        # ef_df_mun_sel <- ef_df_mun_sel |>
        #   dplyr::mutate(tooltip_col = paste0("Município: ", "<b>", nome_mun, "</b>", "\n",
        #                                      "Índice de saída: ", "<b>", output_names_clean[i-1], "</b>", ".\n",
        #                                      "Valor para alcançar eficiência: ", "<b>", round((!!as.name(cols_names[i])+!!as.name(cols_names[i+cols_jump])), 2), "</b>", "\n",
        #                                      "Valor atual: ", "<b>", round(!!as.name(cols_names[i]), 2)), "</b>",
        #                 eixo_x = output_names[i-1])

        ## ef == F se já for eficiente
        ef_df_mun_sel <- func_create_tooltip_ef(
          ef_df_mun_sel, graph_type, ef = F, flag_cmp, i, input_sel_period_name,
          ef_df_mun_sel$nome_area, ef_df_mun_sel$cmp_nome_area,
          in_out_flag, output_names, output_names_clean, cols_names, cols_jump)

        ## Montando gráfico com três camada, pois município já não é eficiente e precisa ser comparado
        graph_lollipop <- graph_lollipop +
          ggiraph::geom_segment_interactive(data = ef_df_mun_sel,
                                            aes(x = eixo_x, xend = eixo_x,
                                                y = !!as.name(cols_names[i]), yend = (!!as.name(cols_names[i])+!!as.name(cols_names[i+cols_jump])),
                                                tooltip = tooltip_col),
                                            color = "grey", inherit.aes = F) +
          ggiraph::geom_point_interactive(data = ef_df_mun_sel,
                                          aes(x = eixo_x, y = !!as.name(cols_names[i]), tooltip = tooltip_col),
                                          shape = 21, colour = "black", fill = "#006400", size = 4, inherit.aes = F) +
          ggiraph::geom_point_interactive(data = ef_df_mun_sel,
                                          aes(x = eixo_x, y = (!!as.name(cols_names[i])+!!as.name(cols_names[i+cols_jump])), tooltip = tooltip_col),
                                          shape = 21, colour = "black", fill = "#014c84", size = 4, inherit.aes = F) +
          ggplot2::expand_limits(x = 0, y = 0)
      }

      if(flag_cmp){
        # browser()
        graph_lollipop <- graph_lollipop +
          ggiraph::geom_point_interactive(data = ef_df_mun_sel,
                                          aes(x = eixo_x, y = !!as.name(cols_names[i+cols_jump*2+1]), tooltip = tooltip_col),
                                          position = position_jitter(),
                                          shape = 21, colour = "black", fill = "#FF8C00", size = 4, inherit.aes = F) +
          ggplot2::expand_limits(x = 0, y = 0)
      }
    }
    # graph_lollipop
    # ggiraph::girafe(ggobj = graph_lollipop)
  }
  # graph_lollipop
  # ggiraph::girafe(ggobj = graph_lollipop)
  # browser()
  return(graph_lollipop)
}
