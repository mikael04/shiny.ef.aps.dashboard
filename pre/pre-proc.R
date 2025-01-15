## Script criado para rodar as etapas de pré-processamento dos dados.

# Carregando funções
source(here::here("R/fct_transform_data.R"))
source(here::here("R/fct_test_cols_data.R"))
source(here::here("R/fct_test_duplicate_data.R"))

## Parâmetros globais
overwrite <- T
verbose <- T
 update_db <- T

## Outro ponto importante é as configurações da base de dados alvo, por padrão,
## o banco de dados utilizado é o que está em produção, portanto deve se tomar cuidado ao rodar o script
## Alguns arquivos de documentação extra estão em man/ e podem ser consultados
## Arquivos de configuração estão em inst/golem-config.yml (ocultos no git)

## Testes nas bases de dados para verificar se estão prontas para transformação
### Testando se as bases possuem colunas utilizadas
### Essa função não corrige colunas faltantes, apenas indica qual base possui problemas e quais colunas estão faltando
result_test_cols_data <- func_test_cols_data(verbose)
## O padrão é, caso existam problemas, parar a execução
if(result_test_cols_data > 0){
  stop("Parando execução, dados de eficiência não são os mesmos utilizados anteriormente,
           ajustes precisam ser feitos.")
}else{
  if(verbose)
    cat("Dados de eficiência são os mesmos utilizados anteriormente, continuando com a transformação e atualização dos dados.")
}

### Testando se as bases possuem duplicatas
### Essa função verifica e corrige duplicatas, caso existam, e sobrescreve a base de dados
result_test_duplicate_data <- func_test_duplicate_data(overwrite, verbose)
## O padrão é, caso existam problemas, parar a execução
if(result_test_duplicate_data > 0){
  stop("Parando execução, dados de eficiência possuem duplicatas não ajustadas.")
}else{
  if(verbose)
    cat("Dados de eficiência não possuem duplicatas, continuando com a transformação e atualização dos dados.")
}

## Após conclusão dos testes, rodar o processo de transformação dos dados
### Esse processo transformará os dados da pasta data-raw/ nos dados utilizados pela aplicação
func_transform_data(verbose, overwrite, update_db)

## Após essas transformações, é possível rodar a aplicação

