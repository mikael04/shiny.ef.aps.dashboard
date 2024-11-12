## Ubuntu 24.04

Para instalação do postgresql, rodar o seguinte comando:

```bash
sudo apt-get install -y     postgresql postgresql-contrib
```

## Alterando configurações para aceitar conexões externas

Para aceitar conexões externas, é necessário alterar o arquivo de configuração do postgresql. Faremos isso acessando o arquivo de configuração do postgresql:

```bash
sudo nano /etc/postgresql/16/main/postgresql.conf
```

Onde 16 é a versão que foi instalada. Então buscaremos pela linha contendo `listen_addresses` e alteraremos para `'*'`. Salve o arquivo e saia do editor.

## Criação de usuário e banco de dados

Vamos seguir com os passos de criação de usuário e banco de dados. Primeiramente, vamos criar o usuário.

### Criar usuário

```bash
sudo -u postgres psql
postgres=# ALTER USER postgres with encrypted password 'minhasenha';
"
```

Alterar o arquivo pg_hba para autenticar o usuário usando scram-sha-256.

```bash
sudo nano /etc/postgresql/16/main/pg_hba.conf
```

E então reiniciar o serviço

```bash
sudo systemctl restart postgresql.service
```


### Criando o banco de dados

Agora iremos criar o banco, no nosso caso, usando um usuário chamado `postgres` e um banco chamado `meubanco`.

```bash
sudo -u postgres createdb meubanco;
```

Para testar a conexão e se o banco de dados foi corretamente criado:

```bash
psql -h IP -p 5432 -d NovoUsuario -U NovoUsuario
```

### Instalando pgAdmin (opcional)

Para testar via interface gráfica e facilitar a administração do banco de dados, podemos instalar o pgAdmin. Para isso, siga os passos abaixo:

```bash
#
# Setup the repository
#

# Install the public key for the repository (if not done previously):
curl -fsS https://www.pgadmin.org/static/packages_pgadmin_org.pub | sudo gpg --dearmor -o /usr/share/keyrings/packages-pgadmin-org.gpg

# Create the repository configuration file:
sudo sh -c 'echo "deb [signed-by=/usr/share/keyrings/packages-pgadmin-org.gpg] https://ftp.postgresql.org/pub/pgadmin/pgadmin4/apt/$(lsb_release -cs) pgadmin4 main" > /etc/apt/sources.list.d/pgadmin4.list && apt update'

#
# Install pgAdmin
#

# Install for both desktop and web modes:
sudo apt install pgadmin4

# Install for desktop mode only:
sudo apt install pgadmin4-desktop

# Install for web mode only:
sudo apt install pgadmin4-web

# Configure the webserver, if you installed pgadmin4-web:
sudo /usr/pgadmin4/bin/setup-web.sh
```

