# Esse script faz um monitoramento espacial da frota de GPS, onde eh possivel identificar interativamente
# onde estao os pontos de GPS de determinada linha e em seguida comparar com o shape da linha,
# identificando visualmente possiveis distorcoes entre a linha identificada no GPS e a linha que os veiculos
# estao realmente rodando

# Conteudo:
# 1) Abrir arquivos de GPS e linhas da etapa anterior
# 2) Filtrar linha teste para visualizacao
# 3) Transformar os dados de GPS para formato espacial
# 4) Criar buffer em torno da linha para facilitar visualizacao
# 5) Visualizar
# 6) Salvar bases de dados




# 0) Carregar pacotes -------------------------------------------------------------------------

library(dplyr) # manipulacao de dados
library(data.table) # abrir e salvar dados (por enquanto)
library(ggplot2) # graficos e mapas
library(mapview) # visualizacao de dados espaciais
library(sf) # operacoes com dados espaciais
library(readr) # abrir e salvar dados em rds






# 1) Abrir arquivos -----------------------------

# 1.1) Abrir arquivo de GPS
gps <- fread("data/gps_rio_amostra.csv")
# selecionar coluna de interesse
gps <- gps %>% select(datahora, ordem, linha, lon = longitude, lat = latitude)

# 1.2) Abrir arquivo com as linhas
linhas_shape <- st_read("data-raw/2020-ago-30/2020-ago-30.shp")
head(linhas_shape)

# 1.3) Selecionar colunas necessarias
linhas_shape_select <- linhas_shape %>%
  # selecionar a coluna 'ref' e renomea-la para 'linha'
  select(linha = ref, name)



# 2) Filtrar linha teste para visualizacao ----------------------------------------------------

# definir linha a ser filtrada
linha_teste <- 309

# 2.1) Filtrar a linha desejada no GPS
# idealmente, quando for filtrar alguma base de dados, eh importante dar um novo nome a base
# nesse caso, como estamos filtrando uma linha, um nome pra base como 'gps_linha' eh interessante
# como pode haver alguma insuficiendia de memoria quando vc cria varias bases de dados diferentes,
# estamos salvando com o mesmo nome de 'gps'
gps <- gps %>%
  # extrair a hora do registro de GPS
  mutate(hora = format(datahora, "%H")) %>%
  # filtrar somente al inha 864
  filter(linha == linha_teste)

# 2.2) Filtrar a linha desejada no shape
linhas_shape_select <- linhas_shape_select %>%
  filter(linha == linha_teste)




# 3) Transformar os dados de GPS para formato espacial sf --------------------------------
# isos vai permitir fazer a visualizacao espacial desses dados

# primeiro, trocar a , por . nas coordenadas
gps <- gps %>%
  # fazer a substituicao e salvar numa nova coluna
  mutate(lon = sub(pattern = ",", replacement = ".", x = lon),
         lat = sub(pattern = ",", replacement = ".", x = lat))

# agora fazer a transformacao para sf
gps <- st_as_sf(gps, coords = c("lon", "lat"), crs = 4326, remove = FALSE)




# 4) Criar buffer em torno da linha para facilitar visualizacao -----------------------
# o argumento em distancia esta em graus, onde o 1 grau representa 111139 metros
linhas_shape_buffer <- st_buffer(linhas_shape_select, dist = 0.001)




# 5) Visualizar -----------------------------------------------------

# 5.1) Filtrar somente os registros de GPS de 06h e 07h para facilitar a visualizacao
gps_pico <- gps %>%
  filter(hora %in% c("06", "07"))

# visualizacao interativa
mapview(gps_pico) + mapview(linhas_shape_buffer)

# mapa estatico
ggplot() +
  # para criar mapas, eh necessario usar o geom_sf
  # 1) criar a cadamada com a linha, e adicionar transparencia com o argumento 'alpha'
  geom_sf(data = linhas_shape_buffer, alpha = 0.2) +
  # 2) criar a camada com os pontos
  geom_sf(data = gps_pico) +
  # mudar tema
  theme_bw()







# 6) Salvar bases de dados -----------------------------------------------------------------------
# as bases de dados serao salvas em formato RDS, que eh um formato proprio do R que mantem todas
# as propriedades do arquivo, eh mais leve, e eh mais rapido de abrir e salvar
# como ponto negativo, esse arquivo so pode ser lido no R

# salvar arquivo de GPS filtrado
write_rds(gps, "data/gps_rio_amostra_linha.rds")

# salvar arquivo de linhas filtrado
write_rds(linhas_shape_buffer, "data/linhas_rio_amostra.rds")



