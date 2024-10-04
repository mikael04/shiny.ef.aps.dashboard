#' server_mod_mapa
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

func_server_mod_mapa <- function(ef_df, map_type, mun_sel, nome_mun_sel, title_ef, sel_period,
                                 ef_proc_res){

  teste_interno <- F
  if(teste_interno){
    map_type <- 2
  }
  ## Variável usada para eficiência, tanto para resultados quanto para processos
  var_ef <- "ef_BCC"
  if(ef_proc_res){
    title_ef <- "Processos"
  }else{
    title_ef <- "Resultados"
  }

  # browser()
  ## map_type 0 -> Mapa do Brasil com estados
  if(map_type == 0){
    # Mapa inicial dos estados ----
    # ef_df <- ef_ufs |>
    #   dplyr::filter(quad_cod == sel_period) |>
    #   dplyr::left_join(uf_sf, by = c("CO_UF" = "cod_stt"))
    ef_df <- ef_df |>
      dplyr::mutate(tooltip = paste0("<b>", nome_uf, "</b>", "\n",
                                     "Eficiência de ", "<b>",  title_ef, "</b>", ": ", !!as.name(var_ef), "\n")) |>
      sf::st_as_sf() |> sf::st_transform(crs = '+proj=longlat
    +datum=WGS84')

    # browser()
    ## Mapa do país, com separação por estados, mapa inicial
    # title = paste0("<span class='title-map'>", title, "<span>")
    ggplot_map <- ggplot(ef_df) +
      ggiraph::geom_sf_interactive(aes(fill = !!as.name(var_ef), tooltip = tooltip, data_id = tooltip)) +
      ggplot2::theme_void() +
      ggplot2::labs(
        fill = "Eficiência"
      ) +
      ggplot2::theme(plot.title = ggtext::element_markdown()) +
      ggplot2::scale_fill_gradient2(low = "#B3C7F7", high = "#054FB9") +
      ggplot2::guides(fill = guide_legend(theme = theme(
        legend.title = element_text(size = 15, face = "bold", colour = "black")
      )))
    # ggplot_map
    # browser()
    # initial_map <- ggiraph_map
    # usethis::use_data(initial_map, overwrite = T)

    # ggiraph_map <- initial_map
    # ggiraph_map
  }
  ## map_type 2 ou 3 -> Mapa do município ou da região de saúde
  if(map_type == 1 || map_type == 2 || map_type == 3){
    # browser()
    ## Tooltip
    ef_df <- ef_df |>
      dplyr::mutate(tooltip = paste0("<b>", nome_mun, "</b>", "\n",
                                     "Eficiência de ", "<b>",  title_ef, "</b>", ": ", !!as.name(var_ef), "\n")) |>
      dplyr::mutate(tooltip = stringr::str_remove_all(tooltip, "'"))
    # ef_df_mun <- ef_df |>
    #   dplyr::filter()

    ggplot_map <- ggplot(ef_df) +
      ggiraph::geom_sf_interactive(aes(fill = !!as.name(var_ef), tooltip = tooltip, data_id = tooltip)) +
      ggplot2::theme_void() +
      ggplot2::theme(plot.title = ggtext::element_markdown()) +
      ggplot2::labs(
        fill = "Eficiência"
      ) +
      ggplot2::scale_fill_gradient2(low = "#B3C7F7", high = "#054FB9") +
      ggplot2::guides(fill = guide_legend(theme = theme(
        legend.title = element_text(size = 15, face = "bold", colour = "black")
      )))

    ## Caso seja o mapa de município, adicionar a borda para destacar município selecionado
    if(map_type == 2){
      ef_df_mun_sel <- ef_df |>
        dplyr::filter(nome_mun == nome_mun_sel) |>
        dplyr::distinct(cod_ibge, .keep_all = T)

      ggplot_map <- ggplot_map + ggplot2::geom_sf(data = ef_df_mun_sel, fill = "transparent", linewidth = 1.2, color = "#008000")
    }
  }

  # ggplot_map
  ggiraph_map <- ggiraph::girafe(
    ggobj = ggplot_map,
    options = list(
      ggiraph::opts_toolbar(
        position = "topright",
        tooltips = list(
          saveaspng = "Salvar como imagem"
        ),
        hidden = c("lasso_select", "lasso_deselect")
      )
    )
  )
  ## Retornando mapa criado
  return(ggiraph_map)
}
