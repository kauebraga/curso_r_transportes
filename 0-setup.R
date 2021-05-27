
# install git
# https://git-scm.com/download/win


# 1) instalar pacotes ----------------------------------------------------------------------------



install.packages(c("dplyr",       # manipulacao de dados
                   "data.table",  # abrir e salvar dados (por enquanto)
                   "ggplot2",     # graficos e mapas
                   "mapview",     # visualizacao de dados espaciais
                   "sf",           # operacoes com dados espaciais
                   "readr",
                   "zip"
                   ))




# 2) unzip data ----------------------------------------------------------------------------------

# criar pasta de figuras
dir.create("figuras")

# criar pastas de dados
dir.create("data-raw")
dir.create("data")

# deszipar dados brutos
zip::unzip("data-raw.zip",
           exdir = "data-raw")
