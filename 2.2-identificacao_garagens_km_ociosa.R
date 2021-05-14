library(dplyr) # manipulacao de dados
library(data.table) # abrir e salvar dados (por enquanto)
library(ggplot2) # graficos e mapas
library(mapview) # visualizacao de dados espaciais
library(sf) # operacoes com dados espaciais

options(scipen=999)

# Abrir dados de GPS do BRT
# Selecionar uma linha
# Trazer o shape dessa linha
# Mapear com a linha e os pontos de GPS com mapview::mapview
# Verificar quais pontos de GPS estão dentro/fora dessa linha como sf::st_join
# Calcular o tempo e a km que a linha fica fora de linha (garagem)


# identificar linha amostral
# linha_amostra <- 864
# linha_amostra <- 350
# linha_amostra <- 638
# linha_amostra <- 2336
linha_amostra <- 309 # problematica

# 1.1) Abrir arquivo de GPS
gps <- fread("gps_rio_amostra.csv")
# selecionar coluna de interesse
gps <- gps %>% select(datahora, ordem, linha, lon = longitude, lat = latitude)


# 1.2) Filtrar a linha 
gps_linha <- gps %>%
  # extrair a hora do registro de GPS
  mutate(hora = format(datahora, "%H")) %>%
  # filtrar somente os registros de 04h, 05h e 06h
  # filter(hora %in% c("05", "06", "07", "08")) %>%
  # filtrar somente al inha 864
  filter(linha == linha_amostra)

# 1.3) Abrir arquivo com as linhas
linhas_shape <- st_read("data-raw/2020-ago-30/2020-ago-30.shp")
head(linhas_shape)

# 1.4) Selecionar colunas necessarias
linhas_shape_select <- linhas_shape %>%
  # selecionar a coluna 'ref' e renomea-la para 'linha'
  select(linha = ref, name)

# 1.5) Selecionar linha de interesse
linhas_shape_filter <- linhas_shape_select %>%
  filter(linha == linha_amostra)

# 1.6) Criar buffer em torno da linha para facilitar visualizacao
# o argumento em distancia esta em graus, onde o 1 grau representa 111139 metros
linhas_shape_filter_buffer <- st_buffer(linhas_shape_filter, dist = 0.001)
mapview(linhas_shape_filter_buffer)

# como temos o shape da ida e da volta, eh interessante junta-los em uma mesma observacao
# isso eh feito atraves da combinacao 'group_by' e 'summarise', so que agora eh uma operacao espacial
# que vai agrupar espacialmente as observacoes
linhas_shape_filter_buffer <- linhas_shape_filter_buffer %>%
  group_by(linha) %>%
  summarise(do_union = TRUE)

# 1.6) Transformar os dados de GPS para formato espacial sf
# isos vai permitir fazer a visualizacao espacial desses dados

# primeiro, trocar a , por . nas coordenadas
gps_linha_coords <- gps_linha %>%
  # fazer a substituicao e salvar numa nova coluna
  mutate(lon = sub(pattern = ",", replacement = ".", x = lon),
         lat = sub(pattern = ",", replacement = ".", x = lat))

# agora fazer a transformacao para sf
gps_linha_sf <- st_as_sf(gps_linha_coords, coords = c("lon", "lat"), crs = 4326, remove = FALSE)


# 1.7) Visualizar

# visualizacao interativa
mapview(gps_linha_sf) + mapview(linhas_shape_filter_buffer)

# 1.8) Verificar quais pontos de GPS estão dentro/fora dessa linha com sf::st_join
gps_join_linha <- st_join(gps_linha_sf, linhas_shape_filter_buffer)

# filtrar somente os pontos de GPS que estao FORA da linha
# para fazer isso, eh preciso filtrar os pontos de GPS que nao tiveram equivalente na juncao espacial
# esses pontos sao identificados quando um ponto de GPS tem um valor NA em alguma coluna que veio do shape das linhas
# uma coluna que veio do shape das linhas foi a coluna 'name', e utilizaremos ela para fazer esse filtro

gps_join_linha_fora <- gps_join_linha %>%
  # para filtrar os pontos de GPS que nao estao inseridos na linha, pegamos os NA da coluna 'name' 
  filter(is.na(linha.y))

# qual a porcentagem dos pontos que estao dentro da linha?
# identificar quando um ponto esta dentro/fora

gps_join_linha %>%
  st_set_geometry(NULL) %>%
  count(linha.y)
  

# visualizar esses pontos
mapview(gps_join_linha_fora) + mapview(linhas_shape_filter_buffer)





# 2) Verificar se esses pontos podem fazer parte de outra(s) linha(s) -------------------------

# para verificar se esses pontos que estao fora da linha 864 podem fazer parte de outras linhas,
# vamos pegar o shape de todas as linhas do sistema e ver se esses pontos se encaixam de alguma forma
# em alguma outra linha

# 2.1) Fazer novamente o buffer, mas em relacao a todas as outras linhas
linhas_shape_buffer <- st_buffer(linhas_shape_select, dist = 0.001)

# fazer novamenta a juncao da ida com a volta
linhas_shape_buffer <- linhas_shape_buffer %>%
  group_by(linha) %>%
  summarise(do_union = TRUE)

# 2.2) Fazer entao a juncao espacial das duas bases

# primeiro, selecionar somente as colunas necessarias
gps_join_linha_fora <- gps_join_linha_fora %>% select(datahora, ordem, hora, lon, lat)

# fazer buffer de 50 metros e deletar pontos que estejam na mesma localizacao
# gps_join_linha_fora1 <-  st_buffer(gps_join_linha_fora, dist = 0.0005) # 50 metros
# mapview(gps_join_linha_fora1)

gps_join_linha_fora1 <- arrange(gps_join_linha_fora, ordem, datahora)

# calcular intervalo
gps_join_linha_fora1 <- gps_join_linha_fora1 %>% mutate(momento := as.ITime(format(datahora, "%H:%M:%S")))
gps_join_linha_fora1 <- gps_join_linha_fora1 %>% group_by(ordem) %>% mutate(headway := -(as.integer(momento - dplyr::lead(momento)))) %>% ungroup()


get.dist <- function(lon, lat) geosphere::distHaversine(tail(cbind(lon,lat),-1), head(cbind(lon,lat),-1))

# identificar distancia para o ponto anterior
gps_join_linha_fora1 <- gps_join_linha_fora1 %>%
  group_by(ordem) %>%
  mutate(dist = c(0, get.dist(as.numeric(lon), as.numeric(lat))))

gps_join_linha_fora1 <- gps_join_linha_fora1 %>% 
  # group_by(ordem) %>%
  mutate(oi = ifelse(dist > 50, c("a", "b", "c"), "1"))


gps_join_linha_fora1 <- gps_join_linha_fora1 %>% mutate(seq = rleid(oi))

# calcular o tamanho de cada grupo
gps_join_linha_fora1 <- gps_join_linha_fora1 %>%
  add_count(seq)

# para os grupos com mais de 20 pontos, pegar so o primeiro e o ultimo
gps_join_linha_fora2 <- gps_join_linha_fora1 %>%
  group_by(ordem) %>%
  filter(n >= 20) %>%
  slice(1, n()) %>%
  ungroup()

gps_join_linha_fora1_new <- gps_join_linha_fora1 %>%
  group_by(ordem) %>%
  filter(n < 20) %>%
  ungroup() %>%
  rbind(gps_join_linha_fora2) %>%
  arrange(ordem, datahora)

# calcular a quantidade dep ontos de gps por veiculo
gps_join_linha_fora1_new <- gps_join_linha_fora1_new %>%
  add_count(ordem, name = "pontos_por_veiculo")

# juncao espacial: a base com os pontos fora da linha fica no lado esquerdo, enquanto a base com as linhas do lado direito
gps_join_linhas <- st_join(gps_join_linha_fora1_new, linhas_shape_buffer)

a <- gps_join_linhas %>%
  st_set_geometry(NULL) %>%
  group_by(linha, ordem) %>%
  summarise(pontos_n = n(), pontos_por_veiculo =  first(pontos_por_veiculo)) %>%
  mutate(perc_n = pontos_n/pontos_por_veiculo)

# como interpretar esse resultado????
b <- a %>%
  group_by(ordem) %>%
  filter(perc_n > 0.8) %>%
  arrange(ordem)

# verificar

mapview(linhas_shape_buffer %>% filter(linha %in% c(330, 442, 443, 444))) +
  # mapview(gps_linha_sf %>% filter(ordem == "C41211") %>% View())
  mapview(gps_join_linha_fora1_new %>% filter(ordem == "C41116"))
  # mapview(gps_linha_sf %>% filter(ordem == "C41392") %>% slice(1:500))

mapview(linhas_shape_buffer %>% filter(linha %in% c(181))) +
  # mapview(gps_linha_sf %>% filter(ordem == "C41211") %>% View())
  mapview(gps_join_linha_fora1_new %>% filter(ordem == "C41381"))
  # mapview(gps_linha_sf %>% filter(ordem == "C41392") %>% slice(1:500))

mapview(linhas_shape_buffer %>% filter(linha %in% c(315, 361))) +
  # mapview(gps_linha_sf %>% filter(ordem == "C41211") %>% View())
  mapview(gps_join_linha_fora1_new %>% filter(ordem == "C41089"))
  # mapview(gps_linha_sf %>% filter(ordem == "C41392") %>% slice(1:500))



aaai <- gps_join_linhas %>% filter(ordem == "C41381")

count(aaai, linha, sort = TRUE)        
