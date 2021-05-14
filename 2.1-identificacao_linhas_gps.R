library(dplyr)      # manipulacao de dados
library(data.table) # abrir e salvar dados (por enquanto)
library(ggplot2)    # graficos e mapas
library(mapview)    # visualizacao de dados espaciais
library(sf)         # operacoes com dados espaciais


# 1.1) Abrir arquivo de GPS
gps <- fread("gps_rio_amostra.csv")


# 1.2) Filtrar uma linha para o horario de 6 da manha
gps_linha <- gps %>%
  # extrair a hora do registro de GPS
  mutate(hora = format(datahora, "%H")) %>%
  # filtrar somente os registros de 06h
  filter(hora == "06") %>%
  # filtrar somente al inha 864
  filter(linha == "864")