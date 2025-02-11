\name{instalando_postgresql}
\alias{instalando_postgresql}
\title{Instalação do PostgreSQL no Ubuntu 24.04}
\description{
  Guia passo a passo para instalar e configurar o PostgreSQL no Ubuntu 24.04.
}
\details{
  Este documento fornece instruções detalhadas para instalar o PostgreSQL no Ubuntu 24.04,
  configurar conexões externas, criar usuários e bancos de dados, e opcionalmente instalar o pgAdmin.
}

\section{Ubuntu 24.04}{
  Para instalação do postgresql, rodar o seguinte comando:
  \preformatted{
    sudo apt-get install -y     postgresql postgresql-contrib
  }
}

\section{Alterando configurações para aceitar conexões externas}{
  Para aceitar conexões externas, é necessário alterar o arquivo de configuração do postgresql.
  Faremos isso acessando o arquivo de configuração do postgresql:
  \preformatted{
    sudo nano /etc/postgresql/16/main/postgresql.conf
  }

  Onde 16 é a versão que foi instalada. Então buscaremos pela linha contendo \code{listen_addresses} e alteraremos para \code{'*'}.
  Salve o arquivo e saia do editor.
}

\section{Criação de usuário e banco de dados}{
  Vamos seguir com os passos de criação de usuário e banco de dados. Primeiramente, vamos criar o usuário.

  \subsection{Criar usuário}{
    \preformatted{
      sudo -u postgres psql
      postgres=\# ALTER USER postgres with encrypted password 'minhasenha';
      "
    }

    Alterar o arquivo pg_hba para autenticar o usuário usando scram-sha-256.
    \preformatted{
      sudo nano /etc/postgresql/16/main/pg_hba.conf
    }

    E então reiniciar o serviço
    \preformatted{
      sudo systemctl restart postgresql.service
    }
  }

  \subsection{Criando o banco de dados}{
    Agora iremos criar o banco, no nosso caso, usando um usuário chamado \code{postgres} e um banco chamado \code{meubanco}.
    \preformatted{
      sudo -u postgres createdb meubanco;
    }

    Para testar a conexão e se o banco de dados foi corretamente criado:
    \preformatted{
      psql -h IP -p 5432 -d NovoUsuario -U NovoUsuario
    }
  }
}

\section{Instalando pgAdmin (opcional)}{
  Para testar via interface gráfica e facilitar a administração do banco de dados, podemos instalar o pgAdmin.
  Para isso, siga os passos abaixo:

  \subsection{Setup the repository}{
    \preformatted{#
# Setup the repository
#

# Install the public key for the repository (if not done previously):
curl -fsS https://www.pgadmin.org/static/packages_pgadmin_org.pub | sudo gpg --dearmor -o /usr/share/keyrings/packages-pgadmin-org.gpg

# Create the repository configuration file:
sudo sh -c 'echo "deb [signed-by=/usr/share/keyrings/packages-pgadmin-org.gpg] https://ftp.postgresql.org/pub/pgadmin/pgadmin4/apt/$(lsb_release -cs) pgadmin4 main" > /etc/apt/sources.list.d/pgadmin4.list && apt update'
}
  }

  \subsection{Install pgAdmin}{
    \preformatted{#
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
}
  }
}
