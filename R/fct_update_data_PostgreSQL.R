#' update_data_PostreSQL
#'
#' @description Uma funçõa que irá ler os dados salvos no formato rda e armazená-los em um banco de dados PostgreSQL
#'
#' @return T se funcionar, F se tiver problema.
#'
#' @noRd

library(DBI)
library(RPostgreSQL)
source("R/utils_db.R")

func_update_data_PostreSQL <- function(debug){
  # if(!shiny::isTruthy(debug)){
  #   debug <- T
  # }
  debug <- T
  ## Conectando com o banco de dados
  con <- utils_db_connect()

  ## Lendos dados dos arquivos rda
  list_files <- list.files(path = "data/database_data", pattern = ".rds", full.names = T)
  ## Removendo sf e gráficos iniciais
  list_files <- list_files[!grepl("sf", list_files)]
  list_files <- list_files[!grepl("initial", list_files)]
  # Removendo "data/" e ".rda"
  list_names <- sub("^data/", "", sub("\\.rds$", "", list_files))
  if(debug)
    print(paste0("Começando escrita no banco de dados, sobrescrevendo ", length(list_files), " tabelas"))
  ## Salvando os dados no banco de dados
  for(i in 1:length(list_files)){
    # Lendo os dados
    df <- readRDS(list_files[i])
    df_name <- list_names[i]
    # load(file = "data/dados_longitudinais.rda")
    # Escrevendo no banco
    dbWriteTable(con, df_name, df, overwrite = T)
    if(debug){
      print(paste0("Escrevendo ", list_names[i], " no banco de dados"))
      print(paste0("Nome do arquivo = ", list_files[i]))
    }
  }
  if(debug)
    print("Encerrando a conexão e finalizando função")
  ## Desconectando do banco de dados
  dbDisconnect(con)

  return(T)
}
