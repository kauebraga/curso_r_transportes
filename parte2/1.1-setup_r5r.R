# Esse script faz o setup das pastas e arquivos para a utilizacao do r5r para calculo de matriz
# de tempo de viagem
library(sf)
library(aopdata) # pontos de origem-destino
library(dplyr)
library(gtfstools)
library(mapview)



# 1) Criar pastas com os arquivos ----------------------------
# criar 2 pastas, uma representando cada gtfs
dir.create("r5/rio_atual")
dir.create("r5/rio_filtrado")


# 2) mover arquivos para as pastas -------------------------------------------

# As duas pasta vao ter os mesmos arquivos de malha viara e topografia
file.copy(from = "data-raw/rio_2020.osm.pbf"   , to = "r5/rio_atual")
file.copy(from = "data-raw/topografia3_rio.tif", to = "r5/rio_atual")

file.copy(from = "data-raw/rio_2020.osm.pbf"   , to = "r5/rio_filtrado")
file.copy(from = "data-raw/topografia3_rio.tif", to = "r5/rio_filtrado")

# copiar arquivos de gtfs
file.copy(from = "data/gtfs/gtfs_rio_atual.zip",    to = "r5/rio_atual")
file.copy(from = "data/gtfs/gtfs_rio_filtrado.zip", to = "r5/rio_filtrado")





# 3) Geracao dos pontos origem-destino --------------------------------------------------------
# vamo usar a malha hexagonoal utilizada no projeto acesso a oportunidades

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
write.csv(pontos_rio_centroide, "r5/points_rio_todo.csv", 
          row.names = FALSE)



# 4) Criar network -------------------------------------------------------------------------------

# definir o caminho da pasta
path1 <- "r5/rio_atual"
path2 <- "r5/rio_filtrado"
r5r::setup_r5(data_path = path1, use_elevation = TRUE, overwrite = TRUE)
r5r::setup_r5(data_path = path2, use_elevation = TRUE, overwrite = TRUE)
