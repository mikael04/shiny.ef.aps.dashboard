Para rodar a aplicação, primeiro teremos que conectar no servidor onde ela está.

Após a conexão, usualmente trabalho dentro do tmux, porque facilita o gerenciamento das aplicações que estarão rodando lá (react + shiny + nginx).

Para abrir o tmux já definindo um nome para a sessão o comando é:

```bash
tmux new -s nome_da_sessao
```
Para a primeira aba, eu costumo utilizar o servidor React, que está rodando na porta 3000. Para isso, eu entro na pasta do projeto e rodo o comando para iniciar a aplicação:

```bash
cd /var/www/html/react-shiny-ef-aps/
npm start
```

Neste momento a aplicação principal deve estar rodando.

Para a dashboard, eu costumo utilizar a segunda aba do tmux e inicio a aplicação Shiny, que está rodando na porta 4000 (configuração definida no .Renviron e inst/golem-config.yml). Para isso, eu entro na pasta do projeto e rodo o comando para iniciar a aplicação:

```bash
cd /srv/shiny-server/shiny.ef.aps.dashboard/
Rscript app.R
```

Pronto, a aplicação deve estar rodando agora.
