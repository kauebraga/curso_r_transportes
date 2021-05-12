library(dplyr) # manipulacao de dados
library(data.table) # abrir e salvar dados (por enquanto)
library(ggplot2) # graficos e mapas
library(mapview) # visualizacao de dados espaciais
library(sf) # operacoes com dados espaciais



# 1) Verificação de itinerários praticados por linha via GPS a partir de shp do que es --------
# Mapa com sobreposição dos pontos de GPS da linha com o shape da linha;

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

# 1.3) Abrir arquivo com as linhas
linhas_shape <- st_read("data-raw/2020-ago-30/2020-ago-30.shp")
head(linhas_shape)

# 1.4) Selecionar colunas necessarias
linhas_shape_select <- linhas_shape %>%
  # selecionar a coluna 'ref' e renomea-la para 'linha'
  select(linha = ref, name)

# 1.5) Selecionar linha de interesse
linhas_shape_filter <- linhas_shape_select %>%
  filter(linha == "864")

# 1.6) Criar buffer em torno da linha para facilitar visualizacao
linhas_shape_filter_buffer <- st_buffer(linhas_shape_filter, dist = 0.0005)

# 1.6) Transformar os dados de GPS para formato espacial sf
# isos vai permitir fazer a visualizacao espacial desses dados

# primeiro, trocar a , por . nas coordenadas
gps_linha_coords <- gps_linha %>%
  # fazer a substituicao e salvar numa nova coluna
  mutate(lon = sub(pattern = ",", replacement = ".", x = longitude),
         lat = sub(pattern = ",", replacement = ".", x = latitude))

# agora fazer a transformacao para sf
gps_linha_sf <- st_as_sf(gps_linha_coords, coords = c("lon", "lat"), crs = 4326)

# visualizar
mapview(gps_linha_sf) + mapview(linhas_shape_filter_buffer)


# 2) Embarque no BRT por estação e vans por hora (+ mapas e gráficos) -------------------------
# Mapa com as estações com mais embarque (left_join)


