\name{rodando_aplicacao}
\alias{rodando_aplicacao}
\title{Rodando a Aplicação}
\description{
  Instruções para iniciar a aplicação, incluindo frontend React e backend Shiny.
}
\details{
  Este guia descreve os passos para iniciar a aplicação, assumindo que o servidor já está configurado com as aplicações React e Shiny.
}

\section{Rodando a aplicação}{

Para rodar a aplicação, primeiro teremos que conectar no servidor onde ela está.

Após a conexão, usualmente trabalho dentro do \code{tmux}, porque facilita o gerenciamento das aplicações que estarão rodando lá (react + shiny + nginx).

\subsection{Iniciando o Tmux}{
Para abrir o \code{tmux} já definindo um nome para a sessão o comando é:
\preformatted{
tmux new -s nome_da_sessao
}
}

\subsection{Iniciando o servidor React}{
Para a primeira aba, eu costumo utilizar o servidor React, que está rodando na porta 3000.
Para isso, eu entro na pasta do projeto e rodo o comando para iniciar a aplicação:
\preformatted{
cd /var/www/html/react-shiny-ef-aps/
npm start
}

Neste momento a aplicação principal deve estar rodando.
}

\subsection{Iniciando a dashboard Shiny}{
Para a dashboard, eu costumo utilizar a segunda aba do \code{tmux} e inicio a aplicação Shiny, que está rodando na porta 4000 (configuração definida no \code{.Renviron} e \code{inst/golem-config.yml}).
Para isso, eu entro na pasta do projeto e rodo o comando para iniciar a aplicação:
\preformatted{
cd /srv/shiny-server/shiny.ef.aps.dashboard/
Rscript app.R
}

Pronto, a aplicação deve estar rodando agora.
}
}
