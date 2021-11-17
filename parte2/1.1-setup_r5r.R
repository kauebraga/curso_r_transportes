# Esse script faz o setup das pastas e arquivos para a utilizacao do r5r para calculo de matriz
# de tempo de viagem
options(java.parameters = '-Xmx10G')
library(osmextract)
library(sf)
library(aopdata) # pontos de origem-destino
library(dplyr)
library(r5r)
library(gtfstools)
library(mapview)

# 1) Download da malha viaria do OSM ----------------------------



# 2) Verificacao do arquivo de GTFS -----------------------------------------------------------

# abrir gtfs
gtfs1 <- gtfstools::read_gtfs("data-raw/gtfs_brt_08-10-2021.zip")
gtfs2 <- gtfstools::read_gtfs("data-raw/licitacao_brt_v3.zip")


# verificar rotas
gtfs1_rotas <- gtfstools::get_trip_geometry(gtfs1, file = "shapes")
mapview(gtfs1_rotas)

gtfs2_rotas <- gtfstools::get_trip_geometry(gtfs2, file = "shapes")
mapview(gtfs2_rotas)

# verificar viagens
gtfs1_velocidades <- gtfstools::get_trip_speed(gtfs1)
summary(gtfs1_velocidades$speed)

gtfs2_velocidades <- gtfstools::get_trip_speed(gtfs2)
summary(gtfs2_velocidades$speed)

# OK!

# 3) Geracao dos pontos origem-destino --------------------------------------------------------

# fazer download do aopdata
pontos_rio <- aopdata::read_grid("rio")

# calcular centroide de cada grade
pontos_rio_centroide <- st_centroid(pontos_rio)

# extrair as coordenadas dos centroids
coords_centroids <- st_coordinates(pontos_rio_centroide)

# trazer essas coordendas para o df principal
pontos_rio_centroide <- pontos_rio_centroide %>%
  mutate(lon = coords_centroids[,1],
         lat = coords_centroids[,2]) %>%
  # deletar a geometria do sf: nao vamos precisar dela!
  st_set_geometry(NULL)

# selecionar e renomear colunas para o r5
pontos_rio_centroide <- pontos_rio_centroide %>%
  select(id = id_hex, lon, lat)

# salvar no formato do r5r
data.table::fwrite(pontos_rio_centroide, "r5r/points/points_rio_todo.csv")



# 4) Criar network -------------------------------------------------------------------------------

# definir o caminho da pasta
path1 <- "r5r/network/rio_atual/"
path2 <- "r5r/network/rio_2023/"
r5r::setup_r5(data_path = path1, use_elevation = TRUE, overwrite = TRUE)
r5r::setup_r5(data_path = path2, use_elevation = TRUE, overwrite = TRUE)
