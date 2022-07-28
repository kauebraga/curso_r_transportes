library(dplyr)
library(gtfstools)
library(mapview)

# Esse script vai criar um novo GTFS sem algunas linhas definidas
# antes, sera feita uma exploracao basica no arquivo de GTFS


# 1) abrir e juntar gtfsgtfs --------------------------------------------------------------

gtfs1 <- read_gtfs("data-raw/gtfs/gtfs_pcrj_2022-07-26_filt_ed.zip")
gtfs2 <- read_gtfs("data-raw/gtfs/brt_2022-07.zip")

# o gtfs1 esta

# juntar gtfs
gtfs_atual <- merge_gtfs(gtfs1, gtfs2,
                         files = c("agency", "calendar", "calendar_dates", "frequencies", "routes", "shapes",
                                   "stop_times", "stops", "trips"),
                         prefix = c("bus", "brt"))
# criar diretorio
dir.create("data/gtfs", recursive = TRUE)

# salvar novo gtfs atual
write_gtfs(gtfs_atual, "data/gtfs/gtfs_rio_atual.zip")





# 2) explorar gtfs -----------------------------------------------------------
gtfs_atual <- read_gtfs("data/gtfs/gtfs_rio_atual.zip")

# exibir lista de linhas (routes) presentes no GTFS
linhas <- gtfs_atual$routes

# ver essas linhas espacialmente
# cada linha eh composta por 1 ou mais shapes, que representam o tracado.
# geralmente 1 linha contem 2 shapes - 1 de ida / 1 de volta
# converter os shapes p/ formato espacial
linhas_sf <- convert_shapes_to_sf(gtfs_atual)
# visualizar
mapview(linhas_sf)

# talvez eu queria visualizar o sistema por tipo de rota (onibus, metro etc)
# para isso, preciso trazer a classificaco de cada linha para o arquivo de shapes
# a relacao entre route e shape esta presente no arquivo de trips
# primeiro, precisamos extrair essas colunas do arquivo de trips
route_shape <- gtfs_atual$trips %>%
  select(route_id, shape_id) %>%
  # essas informacoes podem ser duplicadas, entao vamos tirar as repetidas
  distinct(route_id, shape_id)

# trazer essa informacao para o arquivo de routes, q eh onde esta a coluna com a
# identificacao do tipo de rota (route_type)
route_shape_type <- linhas %>%
  left_join(route_shape, by = "route_id") %>%
  # selecionar somente as colunas q eu quero
  select(shape_id, route_type)

# trazer entao a classificaco para o meu arquivo espacial com os shapes
linhas_sf <- left_join(linhas_sf, route_shape_type, by = "shape_id")
# agora eu posso visualizar o sistema por tipo de rota
mapview(linhas_sf, zcol = "route_type")



# 3) fazer modificacoes para criar novo gtfs ---------------------------------

# abrir o gtfs atual 
gtfs_atual <- read_gtfs("data/gtfs/gtfs_rio_atual.zip")

# definir quais linhas vao ser removidas
linhas_removidas <- c("010", "014", "104", "201", "311",     "349", "388",     "435", "448", 
                      "603", "626", "651", "652", "SVA 665", "669", "SVB 685", "741",
                      "743", "778", "822", "831", "845",     "849", "851",     "865", "870",
                      "871", "892", "893", "922", "925",     "951", "LECD 39", "LECD 40", 
                      "LECD 42", "LECD 43", "LECD 44", "LECD 49", "LECD 50")

# visualizar as linhas que sao removidas ----------------
# primeiro, extrair essas linhas do arquivo de rotas
linhas_filtro <- gtfs_atual$routes %>%
  # as linhas que foram informadas representam o nome da linha, que esta na coluna
  # route_short_name
  filter(route_short_name %in% linhas_removidas)

# identificar quais sao os shapes relacionados a essa linha
# primeiro, extrair a relacao route_Id - shape_id
route_shape <- gtfs_atual$trips %>%
  select(route_id, shape_id) %>%
  # essas informacoes podem ser duplicadas, entao vamos tirar as repetidas
  distinct(route_id, shape_id)

# trazer essa relacao para o arquivo de shapes
shapes <- gtfs_atual$shapes %>%
  left_join(route_shape, by = "shape_id")

# fazer entao o filtro somente com as linhas que eu quero
shapes_filtro <- shapes %>% 
  filter(route_id %in% linhas_filtro$route_id)

# filtrar entao somente os shapes que queremos visualizar
linhas_sf_filtro <- linhas_sf %>%
  filter(shape_id %in% shapes_filtro$shape_id)

mapview(linhas_sf_filtro)


# confirmado que essa sao as linhas que desejo, remover do gtfs p/ criar o novo
# remover linhas
gtfs_filtrado <- gtfstools::filter_by_route_id(gtfs_atual, 
                                               route_id = linhas_filtro$route_id, 
                                               keep = FALSE)

# 3) salvar -------------------------
write_gtfs(gtfs_filtrado, "data/gtfs/gtfs_rio_filtrado.zip")
