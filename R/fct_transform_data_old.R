#' transform_data_rda
#'
#' @description A fct function to save all data used by the application in rds format
#' in data/ folder
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
source("R/fct_order_by_ef_sel.R")
source("R/fct_create_tooltip_ef.R")
source("R/fct_server_mod_graph_lollipop_inputs_outputs.R")
source("R/fct_aux_graph_lollipop_input_proc_1.R")
source("R/fct_aux_graph_lollipop_input_proc_2.R")
source("R/fct_aux_graph_lollipop_outputs_proc.R")
source("R/fct_aux_graph_lollipop_input_res_1.R")
source("R/fct_aux_graph_lollipop_input_res_2.R")
source("R/fct_aux_graph_lollipop_outputs_res.R")
source("R/fct_aux_lollipop_patchwork.R")
source("R/fct_init_dfs_iniciais.R")
source("R/fct_init_grafs.R")
source("R/fct_server_mod_mapa.R")
source("R/fct_server_mod_tabela_ef.R")
source("R/fct_transform_data_to_download.R")
source("R/fct_update_data_PostgreSQL.R")
source("R/fct_test_cols_data.R")
source("R/fct_test_duplicate_data.R")
library(ggplot2)
library(patchwork)

func_transform_data <- function(verbose){
  if(F){
    verbose <- T
    overwrite_data <- T
    update_db <- T
  }
  if(verbose) print(paste0("Iniciando processo de transformação"))
  ## Sobrescrever dados RDS e RDA
  use_RDS <- T
  use_RDA <- T

  ## Ao invés de usar 6, usaremos o último disponível, portanto não será setado manualmente
  # sel_period <- 6L
  if(overwrite_data){
    ## Checando se dados recebidos são os mesmos já utilizados (ou se precisa uma nova transformação)
    result_test_cols_data <- func_test_cols_data(verbose)
    if(result_test_data > 0){
      stop("Parando execução, dados de eficiência não são os mesmos utilizados anteriormente,
           ajustes precisam ser feitos.")
    }else{
      if(verbose)
      cat("Dados de eficiência são os mesmos utilizados anteriormente, continuando com a transformação e atualização dos dados.")
    }
    result_test_duplicate_data <- func_test_duplicate_data(overwrite_data, verbose)

    # Lendo dados ----
    ## Geométricos ----
    ### Municípios
    mun_sf <- sf::read_sf(here::here("data-raw/dados-espaciais/mun/municipios_sf.shp"))
    ### UFs
    uf_sf <- sf::read_sf(here::here("data-raw/dados-espaciais/uf_sf.shp")) |>
      dplyr::select(cod_stt, geometry)
    ## Regiões de saúde
    reg_saude_sf <- sf::read_sf("../shiny.ef.aps/data-raw/shapefile-reg-saude/br_regionais_simplificado/BR_Regionais.shp") |>
      dplyr::rename(nome_reg_saude = nome, estado_id_reg_saude = est_id)

    ## Municípios, códigos e capitais ----
    df_mun_cod_ibge <- read.csv2("data-raw/cod_ibge_mun.csv") |>
      dplyr::select(-X)
    df_cap_uf_ibge <- read.csv("data-raw/cod_ibge_mun_capitais.csv") |>
      dplyr::select(-ID)


    ## Saúde ----
    dados_longitudinais <- data.table::fread(here::here("data-raw/dados_territorio/dados_longitudinais.csv")) |>
      dplyr::mutate(quad = dplyr::case_when(
        lubridate::month(lubridate::ymd(mes)) < 5 ~ 1,
        lubridate::month(lubridate::ymd(mes)) < 9 ~ 2,
        lubridate::month(lubridate::ymd(mes)) < 13 ~ 3,
      ))

    ## Dados municipais, estaduais e de região de saúde, usados nos filtros laterais
    dados_munic_uf_regiao_regsaude <- read.csv(here::here("data-raw/dados_territorio/dados_territorio.csv"))

    ## Buscando apenas dados de municípoios e ied
    df_mun_ied <- data.table::fread(here::here("data-raw/eficiencia_processos_corrigida_2022_2023.csv")) |>
      dplyr::select(cod_ibge = ibge, nome_mun,
                    ied = indice_de_equidade_e_dimensionamento_ied) |>
      dplyr::distinct(cod_ibge, .keep_all = T) |>
      dplyr::inner_join(dados_munic_uf_regiao_regsaude) |>
      dplyr::select(cod_ibge, nome_mun, nome_uf,
                    ied)

    ## Filtrando apenas os que possuem dados
    df_dados_mun_uf_reg_saud_filter <- dados_munic_uf_regiao_regsaude |>
      dplyr::select(cod_ibge, CO_REGSAUD, CO_UF, nome_mun, nome_rs, nome_uf) |>
      dplyr::left_join(df_mun_ied, by = c("cod_ibge", "nome_mun", "nome_uf")) |>
      dplyr::mutate(has_data_proc = ifelse(is.na(ied), F, T))


    df_mun_ied_res <- data.table::fread(here::here("data-raw/eficiencia_resultados_corrigida_2022_2023.csv")) |>
      dplyr::select(cod_ibge = ibge, nome_mun,
                    ied = indice_de_equidade_e_dimensionamento_ied) |>
      dplyr::distinct(cod_ibge, .keep_all = T) |>
      dplyr::inner_join(dados_munic_uf_regiao_regsaude) |>
      dplyr::select(cod_ibge, nome_mun, nome_uf,
                    ied) |>
      dplyr::mutate(has_data_res = ifelse(is.na(ied), F, T)) |>
      dplyr::select(cod_ibge, has_data_res)

    df_dados_mun_uf_reg_saud_filter <- df_dados_mun_uf_reg_saud_filter |>
      dplyr::left_join(df_mun_ied_res, by = c("cod_ibge")) |>
      dplyr::mutate(has_data_res = ifelse(is.na(has_data_res), F, T))


    # Manipulando dados ----
    ## Eficiência dos municípios ----
    ef_muns_proc <- data.table::fread(here::here("data-raw/eficiencia_processos_corrigida_2022_2023.csv")) |>
      dplyr::rename(ef_BCC_antigo = ef_BCC, ef_BCC = ef_BCC_corrigido) |>
      dplyr::rename(cod_ibge = ibge) |>
      ## Unindo e adicionando variáveis que serão necessárias à um único banco
      dplyr::left_join(dados_munic_uf_regiao_regsaude, by = "cod_ibge") |>
      dplyr::mutate(quad = paste0(substr(quad, 1, 4), "_", ceiling(as.integer(gsub("^.{0,5}", "", quad))/4))) |>
      dplyr::mutate(ano_quad = quad,
                    ano = as.integer(stringr::str_sub(ano_quad, 1, 4)),
                    quad = as.integer(stringr::str_sub(ano_quad, -1, -1)),
                    quad_cod = (ano-2022)*3+quad) |>
      dplyr::select(cod_ibge, nome_mun = nome_mun.x, ano, ano_quad, quad_cod, ef_BCC,
                    ## inputs
                    desp_final = desp_por_eq_2, #desp_por_hab_cob
                    ## Quanto falta nos inputs
                    v_desp_final = v_desp_final,
                    ## outputs
                    ind1_gest, ind2_teste, ind3_odonto, ind4_cito,
                    ind5_vacina, ind6_hiper, ind7_diab,
                    ## Quanto falta nos outputs
                    v_ind1_pre_natal,
                    v_ind2_tr, v_ind3_odonto_gest, v_ind4_cito, v_ind5_vac,
                    v_ind6_has, v_ind7_dm,
                    ## Outros dados geográficos
                    CO_UF, nome_uf, CO_MACSAUD, CO_REGSAUD, nome_rs,
                    ## Outros indicadores
                    ivs = indice_de_vulnerabilidade_social,
                    pop_2022 = numero_de_habitantes_segundo_o_ibge_2022,
                    faixa_porte_2022 = faixa_de_porte_populacional_segundo_o_ibge_2022,
                    ied = indice_de_equidade_e_dimensionamento_ied,
                    ## Município de referência
                    mun_ref
      )

    sel_period_proc <- max(ef_muns_proc$quad_cod)

    ef_muns_res <- data.table::fread(here::here("data-raw/eficiencia_resultados_corrigida_2022_2023.csv")) |>
      dplyr::rename(cod_ibge = ibge) |>
      ## Unindo e adicionando variáveis que serão necessárias à um único banco
      dplyr::left_join(dados_munic_uf_regiao_regsaude, by = "cod_ibge") |>
      dplyr::mutate(quad = 0) |>
      dplyr::mutate(ano_quad = paste0(ano, "_", quad),
                    quad_cod = (ano-2022)*3+quad+1) |>
      dplyr::select(
        cod_ibge, nome_mun = nome_mun.x, ano, ano_quad, quad_cod, ef_BCC,
        ## inputs
        desp_final = desp_por_hab,
        ## Quanto falta nos inputs
        v_desp_final = v_desp_por_hab,
        ## outputs
        tx_mort, tx_inter,
        ## Quanto falta nos outputs
        v_tx_mort, v_tx_inter,
        ## Outros dados geográficos
        CO_UF, nome_uf, CO_MACSAUD, CO_REGSAUD, nome_rs,
        ## Outros indicadores
        ivs = indice_de_vulnerabilidade_social,
        pop_2022 = numero_de_habitantes_segundo_o_ibge_2022,
        faixa_porte_2022 = faixa_de_porte_populacional_segundo_o_ibge_2022,
        ied = indice_de_equidade_e_dimensionamento_ied,
        ## Município de referência
        mun_ref
      )

    sel_period_res <- max(ef_muns_res$quad_cod)

    ## Dados para download, apenas limpeza de tabelas de municípios ----
    list_dfs <- func_transform_data_to_download(ef_muns_proc, ef_muns_res)
    df_muns_proc_download <- list_dfs[[1]]
    df_muns_res_download <- list_dfs[[2]]

    # length(unique(ef_muns_proc$cod_ibge))
    # length(unique(ef_muns_res$cod_ibge))

    df_mun_cod_ibge_regsaude <- dplyr::inner_join(df_mun_cod_ibge, ef_muns_proc,
                                                  dplyr::join_by("cod_ibge", "nome_mun", "nome_uf")) |>
      dplyr::select(cod_ibge, nome_mun, nome_uf, CO_REGSAUD, nome_rs) |>
      dplyr::distinct(cod_ibge, .keep_all = T)

    ### Dados de eficiência no quadrimestre, formato wide
    # ef_muns_ <- ef_muns_proc |>
    #   dplyr::filter(cod_ibge == 431690) |>
    #   dplyr::mutate(quad_cod = as.character(quad_cod))

    ef_muns_quad_proc <- ef_muns_proc |>
      dplyr::select(cod_ibge, nome_mun, quad_cod, CO_REGSAUD, nome_uf, processos = ef_BCC) |>
      tidyr::pivot_wider(names_from = quad_cod,
                         names_prefix = "processos_",
                         values_from = processos)


    ef_muns_ano_res <- ef_muns_res |>
      dplyr::select(cod_ibge, nome_mun, quad_cod, CO_REGSAUD, nome_uf, resultados = ef_BCC) |>
      tidyr::pivot_wider(names_from = quad_cod,
                         names_prefix = "resultados_",
                         values_from = resultados,
                         values_fill = sum)

    ef_muns_res |>
      dplyr::summarise(n = dplyr::n(), .by = c(cod_ibge, nome_mun, CO_REGSAUD, nome_uf, quad_cod)) |>
      dplyr::filter(n > 1)

    ## Eficiência UFs ----
    ### Criando DF de teste com estados e eficiências
    ### Processos ----
    ef_ufs_proc <- ef_muns_proc |>
      dplyr::select(CO_UF, nome_uf, ef_BCC, quad_cod,
                    ## inputs
                    desp_final,
                    ## Quanto falta nos inputs
                    v_desp_final,
                    ## outputs
                    ind1_gest, ind2_teste, ind3_odonto, ind4_cito,
                    ind5_vacina, ind6_hiper, ind7_diab,
                    ## Quanto falta no output
                    v_ind1_pre_natal,
                    v_ind2_tr, v_ind3_odonto_gest, v_ind4_cito, v_ind5_vac,
                    v_ind6_has, v_ind7_dm) |>
      dplyr::group_by(CO_UF, quad_cod) |>
      dplyr::mutate(across(where(is.numeric), ~ round(mean(.x, na.rm = T), 2))) |>
      dplyr::distinct(CO_UF, quad_cod, .keep_all = T) |>
      dplyr::ungroup()


    ef_ufs_proc_quad <- ef_ufs_proc |>
      dplyr::select(CO_UF, nome_uf, quad_cod, processos = ef_BCC) |>
      tidyr::pivot_wider(names_from = quad_cod,
                         names_prefix = "processos_",
                         values_from = processos)

    ### Resultados ----
    ef_ufs_res <- ef_muns_res |>
      dplyr::select(CO_UF, nome_uf, ef_BCC, quad_cod,
                    ## inputs
                    desp_final,
                    ## Quanto falta nos inputs
                    v_desp_final,
                    ## outputs
                    tx_mort, tx_inter,
                    ## Quanto falta nos outputs
                    v_tx_mort, v_tx_inter,
                    ## Outros dados geográficos
                    CO_UF, nome_uf, CO_MACSAUD, CO_REGSAUD) |>
      dplyr::group_by(CO_UF, quad_cod) |>
      dplyr::mutate(across(where(is.numeric), ~ round(mean(.x, na.rm = T), 2))) |>
      dplyr::distinct(CO_UF, quad_cod, .keep_all = T) |>
      dplyr::ungroup()


    ef_ufs_res_quad <- ef_ufs_res |>
      dplyr::select(CO_UF, nome_uf, quad_cod, resultados = ef_BCC) |>
      tidyr::pivot_wider(names_from = quad_cod,
                         names_prefix = "resultados_",
                         values_from = resultados)
    ## Eficiência Brasil ----
    ### Processos ----
    ## Dados do brasil
    ef_br_proc <- ef_ufs_proc |>
      dplyr::mutate(pais = "Brasil") |>
      dplyr::select(-CO_UF, -nome_uf) |>
      dplyr::relocate(pais) |>
      dplyr::group_by(quad_cod) |>
      dplyr::mutate(across(where(is.numeric), ~ round(mean(.x, na.rm = T), 2))) |>
      dplyr::distinct(quad_cod, .keep_all = T) |>
      dplyr::ungroup()


    ef_br_proc_quad <- ef_br_proc |>
      dplyr::select(pais, quad_cod, processos = ef_BCC) |>
      tidyr::pivot_wider(names_from = quad_cod,
                         names_prefix = "processos_",
                         values_from = processos)
    ### Resultados ----
    ## Dados do brasil
    ef_br_res <- ef_ufs_res |>
      dplyr::mutate(pais = "Brasil") |>
      dplyr::select(-CO_UF, -nome_uf) |>
      dplyr::relocate(pais) |>
      dplyr::group_by(quad_cod) |>
      dplyr::mutate(across(where(is.numeric), ~ round(mean(.x, na.rm = T), 2))) |>
      dplyr::distinct(quad_cod, .keep_all = T) |>
      dplyr::ungroup()


    ef_br_quad <- ef_br_res |>
      dplyr::select(pais, quad_cod, resultados = ef_BCC) |>
      tidyr::pivot_wider(names_from = quad_cod,
                         names_prefix = "resultados_",
                         values_from = resultados)

    # Gráficos iniciais ----
    ## Aqui criaremos os dataframes utilizados nos gráficos iniciais, para
    ## que não seja necessário rodar os cálculos e consequentemente
    ## ter um shiny inicialmente mais rápido
    # ef_proc_res = T -> Processos; = F -> Resultados

    ## Processos ----
    ### DFs iniciais
    list_func_init_dfs_iniciais <- func_init_dfs_iniciais(ef_ufs_proc, uf_sf, ef_ufs_proc_quad, ef_br_proc, sel_period_proc,
                                                          ef_proc_res = T)
    df_mapa_inicial_ufs_sf_proc <- list_func_init_dfs_iniciais[[1]]
    df_tabela_inicial_ufs_proc <- list_func_init_dfs_iniciais[[2]]

    ## Gráficos iniciais
    list_func_init_grafs <- func_init_grafs(df_mapa_inicial_ufs_sf_proc, df_tabela_inicial_ufs_proc,
                                            ef_br_proc, sel_period_proc,
                                            ef_proc_res = T, newcase = T)
    initial_map_p <- list_func_init_grafs[[1]]
    initial_gt_tabela_p <- list_func_init_grafs[[2]]
    initial_graf_inputs_outputs_p <- list_func_init_grafs[[3]]

    # ggiraph::girafe(ggobj = initial_graf_inputs_outputs_p)

    ## Resultados ----
    ### DFs iniciais
    list_func_init_dfs_iniciais <- func_init_dfs_iniciais(ef_ufs_res, uf_sf, ef_ufs_res_quad, ef_br_res, sel_period_res,
                                                          ef_proc_res = F)
    df_mapa_inicial_ufs_sf_res <- list_func_init_dfs_iniciais[[1]]
    df_tabela_inicial_ufs_res <- list_func_init_dfs_iniciais[[2]]

    ## Gráficos iniciais
    list_func_init_grafs <- func_init_grafs(df_mapa_inicial_ufs_sf_res, df_tabela_inicial_ufs_res,
                                            ef_br_res, sel_period_res,
                                            ef_proc_res = F, newcase = T)
    initial_map_r <- list_func_init_grafs[[1]]
    initial_gt_tabela_r <- list_func_init_grafs[[2]]
    initial_graf_inputs_outputs_r <- list_func_init_grafs[[3]]

    # ggiraph::girafe(ggobj = initial_graf_inputs_outputs_r)

    # Escrevendo dados ----
    # ### Formato RDS
    if(use_RDS){
      saveRDS(dados_longitudinais, file = "data/database_data/dados_longitudinais.rds")
      saveRDS(dados_munic_uf_regiao_regsaude, file = "data/database_data/dados_munic_uf_regiao_regsaude.rds")
      saveRDS(df_mun_cod_ibge, file = "data/database_data/df_mun_cod_ibge.rds")
      saveRDS(df_mun_cod_ibge_regsaude, file = "data/database_data/df_mun_cod_ibge_regsaude.rds")
      saveRDS(df_dados_mun_uf_reg_saud_filter, file = "data/database_data/df_dados_mun_uf_reg_saud_filter.rds")
      saveRDS(df_mun_ied, file = "data/database_data/df_mun_ied.rds")
      saveRDS(df_cap_uf_ibge, file = "data/database_data/df_cap_uf_ibge.rds")
      saveRDS(ef_muns_proc, file = "data/database_data/ef_muns_proc.rds")
      saveRDS(ef_muns_quad_proc, file = "data/database_data/ef_muns_quad_proc.rds")
      saveRDS(ef_muns_res, file = "data/database_data/ef_muns_res.rds")
      saveRDS(ef_muns_ano_res, file = "data/database_data/ef_muns_ano_res.rds")
      saveRDS(ef_ufs_proc, file = "data/database_data/ef_ufs_proc.rds")
      saveRDS(ef_ufs_proc_quad, file = "data/database_data/ef_ufs_proc_quad.rds")
      saveRDS(ef_br_proc, file = "data/database_data/ef_br_proc.rds")
      saveRDS(ef_br_proc_quad, file = "data/database_data/ef_br_proc_quad.rds")
      saveRDS(ef_ufs_res, file = "data/database_data/ef_ufs_res.rds")
      saveRDS(ef_ufs_res_quad, file = "data/database_data/ef_ufs_res_quad.rds")
      saveRDS(ef_br_res, file = "data/database_data/ef_br_res.rds")
      saveRDS(ef_br_quad, file = "data/database_data/ef_br_quad.rds")
      saveRDS(df_muns_proc_download, file = "data/database_data/df_muns_proc_download.rds")
      saveRDS(df_muns_res_download, file = "data/database_data/df_muns_res_download.rds")
      ## Após salvar dados utilizados no formato .rds, atualizar o banco de dados
      if(update_db){
        func_update_data_PostreSQL(verbose)
      }
    }

    ### Formato RDA
    if(use_RDA){
      # usethis::use_data(dados_longitudinais, overwrite = TRUE)
      # usethis::use_data(dados_munic_uf_regiao_regsaude, overwrite = TRUE)
      # usethis::use_data(df_mun_cod_ibge, overwrite = TRUE)
      # usethis::use_data(df_mun_cod_ibge_regsaude, overwrite = TRUE)
      # usethis::use_data(df_dados_mun_uf_reg_saud_filter, overwrite = TRUE)
      # usethis::use_data(df_mun_ied, overwrite = TRUE)
      # usethis::use_data(df_cap_uf_ibge, overwrite = TRUE)
      # usethis::use_data(ef_muns_proc, overwrite = TRUE)
      # usethis::use_data(ef_muns_quad_proc, overwrite = TRUE)
      # usethis::use_data(ef_muns_res, overwrite = TRUE)
      # usethis::use_data(ef_muns_ano_res, overwrite = TRUE)
      # usethis::use_data(ef_ufs_proc, overwrite = TRUE)
      # usethis::use_data(ef_ufs_proc_quad, overwrite = TRUE)
      # usethis::use_data(ef_br_proc, overwrite = TRUE)
      # usethis::use_data(ef_br_proc_quad, overwrite = TRUE)
      ## SFs utilizados ----
      if(!verbose)
        withr::local_options(list(usethis.quiet = T))
      usethis::use_data(mun_sf, overwrite = TRUE)
      usethis::use_data(uf_sf, overwrite = TRUE)
      usethis::use_data(reg_saude_sf, overwrite = TRUE)
      ## Dados iniciais para gráficos
      usethis::use_data(initial_map_p, overwrite = TRUE)
      usethis::use_data(initial_gt_tabela_p, overwrite = TRUE)
      usethis::use_data(initial_graf_inputs_outputs_p, overwrite = TRUE)
      usethis::use_data(initial_map_r, overwrite = TRUE)
      usethis::use_data(initial_gt_tabela_r, overwrite = TRUE)
      usethis::use_data(initial_graf_inputs_outputs_r, overwrite = TRUE)
    }
  }

  if(verbose){
    if(overwrite_data){
      print("Dados sobrescritos")
    }
    if(use_RDS){
      print("Dados em formato RDS salvos")
      print("dados_longitudinais, dados_munic_uf_regiao_regsaude, df_mun_cod_ibge,
          df_mun_cod_ibge_regsaude, df_dados_mun_uf_reg_saud_filter, df_mun_ied,
          df_cap_uf_ibge, ef_muns_proc, ef_muns_quad_proc, ef_muns_res, ef_muns_ano_res,
          ef_ufs_proc, ef_ufs_proc_quad, ef_br_proc, ef_br_proc_quad, ef_ufs_res,
          ef_ufs_res_quad, ef_br_res, ef_br_quad, df_muns_proc_download,
          df_muns_res_download")

      if(update_db){
        print("Banco de dados atualizado (verificar banco de dados destino)")
      }
    }
    if(use_RDA){
      print("Dados em formato RDA salvos")
      print("mun_sf, uf_sf, reg_saude_sf, initial_map_p, initial_gt_tabela_p,
          initial_graf_inputs_outputs_p, initial_map_r, initial_gt_tabela_r,
          initial_graf_inputs_outputs_r")
    }
    print(paste0("Finalizando processo de transformação"))
  }
}

# func_transform_data()


# ## Manipulando dados apenas para resolver município duplicado
#
# ## Backup
# data.table::fwrite(ef_muns_proc, "data-raw/eficiencia_processos_corrigida_2022_2023_backup.csv")
#
# ## Filtrando apenas uma das linhas pro município para cada quadrimestre
# mun_aux <- ef_muns_proc |>
#   dplyr::filter(ibge == 420207) |>
#   dplyr::arrange(desc(quad)) |>
#   dplyr::group_by(quad) |>
#   dplyr::distinct(quad, .keep_all = TRUE) |>
#   dplyr::ungroup()
#
# ## Removendo o município da base
# ef_muns_proc <- dplyr::filter(ef_muns_proc, ibge != 420207)
#
# ## Adicionando apenas uma linha por quadrimestre
# ef_muns_proc <- dplyr::bind_rows(ef_muns_proc, mun_aux)
#
# ## Salvando nova base
# data.table::fwrite(ef_muns_proc, "data-raw/eficiencia_processos_corrigida_2022_2023.csv")
