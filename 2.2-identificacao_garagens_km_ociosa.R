
# 0) Carregar pacotes -------------------------------------------------------------------------

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

# 1) Abrir arquivo de GPS e filtrar linha -------------------------------
gps <- fread("gps_rio_amostra.csv")
# selecionar coluna de interesse
gps <- gps %>% select(datahora, ordem, linha, lon = longitude, lat = latitude)







# 2) Filtrar a linha dos dados de GPS -----------------------------------
gps_linha <- gps %>%
  # extrair a hora do registro de GPS
  mutate(hora = format(datahora, "%H")) %>%
  # filtrar somente os registros de 04h, 05h e 06h
  # filter(hora %in% c("05", "06", "07", "08")) %>%
  # filtrar somente al inha 864
  filter(linha == linha_amostra)







# 3) Abrir arquivo com as linhas ---------------------------------------
linhas_shape <- st_read("data-raw/2020-ago-30/2020-ago-30.shp")
head(linhas_shape)

# 3.1) Selecionar colunas necessarias
linhas_shape_select <- linhas_shape %>%
  # selecionar a coluna 'ref' e renomea-la para 'linha'
  select(linha = ref, name)







# 4) Selecionar linha de interesse -------------------------------
linhas_shape_filter <- linhas_shape_select %>%
  filter(linha == linha_amostra)





# 5) Criar buffer em torno da linha para facilitar visualizacao -------------------------
# o argumento em distancia esta em graus, onde o 1 grau representa 111139 metros
linhas_shape_filter_buffer <- st_buffer(linhas_shape_filter, dist = 0.001)

# como temos o shape da ida e da volta, eh interessante junta-los em uma mesma observacao
# isso eh feito atraves da combinacao 'group_by' e 'summarise', so que agora eh uma operacao espacial
# que vai agrupar espacialmente as observacoes
linhas_shape_filter_buffer <- linhas_shape_filter_buffer %>%
  group_by(linha) %>%
  summarise(do_union = TRUE)

mapview(linhas_shape_filter_buffer)





# 6) Transformar os dados de GPS para formato espacial sf ----------------------------------

# primeiro, trocar a , por . nas coordenadas
gps_linha_coords <- gps_linha %>%
  # fazer a substituicao e salvar numa nova coluna
  mutate(lon = sub(pattern = ",", replacement = ".", x = lon),
         lat = sub(pattern = ",", replacement = ".", x = lat))

# agora fazer a transformacao para sf
gps_linha_sf <- st_as_sf(gps_linha_coords, coords = c("lon", "lat"), crs = 4326, remove = FALSE)









# 7) Visualizar -------------------------------------

# visualizacao interativa
mapview(gps_linha_sf) + mapview(linhas_shape_filter_buffer)










# 8) Verificar quais pontos de GPS estão dentro/fora dessa linha com sf::st_join --------------

# filtrar somente os pontos de GPS que estao FORA da linha
# para fazer isso, eh preciso filtrar os pontos de GPS que nao tiveram equivalente na juncao espacial
# esses pontos sao identificados quando um ponto de GPS tem um valor NA em alguma coluna que veio do shape das linhas

# para identificar a linha que esta no GPS e a que vai vir do shape das linhas, eh preciso diferenciar
# o nome das colunas de linha das duas bases:
gps_linha_sf <- gps_linha_sf %>% rename(linha_gps = linha)
linhas_shape_filter_buffer <- linhas_shape_filter_buffer %>% rename(linha_shape = linha)

# realizar a intersecao espacial
gps_join_linha <- st_join(gps_linha_sf, linhas_shape_filter_buffer)

# a coluna 'linha_shape' veio dos shapes das linhas, entao todos as observacoes em que ela esteja como NA
# serao observacoes do GPS que estao FORA da linha
gps_join_linha_fora <- gps_join_linha %>%
  # para filtrar os pontos de GPS que nao estao inseridos na linha, pegamos os NA da coluna 'name' 
  filter(is.na(linha_shape))

# qual a porcentagem dos pontos que estao dentro da linha?
# identificar quando um ponto esta dentro/fora
gps_join_linha %>%
  # deletar a parte espacial do dataframe que nao vamos precisar aqui (fica bem + rapido)
  st_set_geometry(NULL) %>%
  count(linha_shape) %>%
  mutate(perc = n / sum(n))
  
# visualizar esses pontos
mapview(gps_join_linha_fora) + mapview(linhas_shape_filter_buffer)










# 9) Fazer limpeza nos dados de GPS -----------------------------------------------------------

#' Foram observados diversos pontos mortos de GPS na linha, o que estava prejudicando as analises
#' Situacoes como centanas de registros de GPS de uma linha sendo identificados em uma mesma localidade
#' Ainda nao eh possivel saber se essas concentracoes de pontos sao uma garagem, final de linha, ou
#' qualquer outra coisa
#' Para contornar isso, essa etapa identifica quando essas concentracoes acontecem e diminui a quantidade
#' de pontos das concentracoes, de forma a garantir que os pontos de GPS que estao na base sejam de
#' quando o veiculo esteja realmente em movimento


# 9.1) Ordenar os dados de GPS por carro e linha
gps_join_linha_fora1 <- arrange(gps_join_linha_fora, ordem, datahora)

# # Extrair o momento preciso
# gps_join_linha_fora1 <- gps_join_linha_fora1 %>% mutate(momento := as.ITime(format(datahora, "%H:%M:%S")))
# gps_join_linha_fora1 <- gps_join_linha_fora1 %>% group_by(ordem) %>% mutate(headway := -(as.integer(momento - dplyr::lead(momento)))) %>% ungroup()

# Calcular a distancia entre um ponto e o seu ponto anterior
# Isso vai ajudar a identificar quando um veiculo se manteve imovel/se moveu muito pouco

# estabelecer funcao para calcular dist entre um ponto e seu anterior
get.dist <- function(lon, lat) geosphere::distHaversine(tail(cbind(lon,lat),-1), head(cbind(lon,lat),-1))

# Calcular essa distancia, agrupando por veiculo
gps_join_linha_fora1 <- gps_join_linha_fora1 %>%
  group_by(ordem) %>%
  mutate(dist = c(0, get.dist(as.numeric(lon), as.numeric(lat))))

#' Aqui, vamos estabelecer a velocidade de 50 metros como uma distancia limite entre um ponto e seu anterior
#' para identificar esses pontos como uma 'aglomeracao' onde o veiculo estava parado/pouco se mexendo
#' se a distancia for maior que 50m, sera atribuido um conjunto de letras que sao diferentes
#' se a distancia for menor que 50m, sera atribuido o numeral 1
#' isso eh um recurso de programacao para ajudar no agrupamento desses pontos
gps_join_linha_fora1 <- gps_join_linha_fora1 %>% 
  mutate(oi = ifelse(dist > 50, c("a", "b", "c"), "1"))

# agrupar esses pontos
#' a funcao 'rleid' cria numeros que vao percorrendo o data.frame e se mantem iguais quando a coluna 
#' de referencia for igual
#' por ex, quando a coluna de referencia estiver com os valores "a b c", a funcao vai retornar "1 2 3"
#' porem, quando a coluna de referencia estiver com os valores "1 1 1", a funcao vai retornar "4 4 4",
#' respeitando a sequencia que foi estabelecida
gps_join_linha_fora1 <- gps_join_linha_fora1 %>% mutate(seq = rleid(oi))


# Com as concentracoes ja identificadas, eh necessario identificar quando essas concentracoes
# devem ser diminuidas
# O criterio estabelecido aqui estabelece que uma concentracao com mais de 15 pontos de GPS deve ser
# reduzida para 2 pontos, que vao representar o primeiro e o ultimo ponto da concentracao

# Primeiramente, eh calculado o tamanho da concentracao
# A funcao 'add_count' adiciona uma nova coluna com a quantidade de cada 'seq'
gps_join_linha_fora1 <- gps_join_linha_fora1 %>%
  add_count(seq)

# Fazer entao o filtro para concentracoes que tenham mais de 15 pontos - essas vao ser reduzidas
gps_join_linha_fora2 <- gps_join_linha_fora1 %>%
  # fazer o filtro de 15 pontos
  # agruparar por veiculo e sequencia
  filter(n >= 10)  %>%
  group_by(ordem, seq) %>%
  # a funcao 'slice' serve para extrair as observacoes por posicao - nesse caso, vamos tirar a 
  # primeira (1) e a ultima (n())
  slice(1, n()) %>%
  ungroup()

# Os pontos concentrados que foram reduzidos precisam ser colados a base original
# Para isso, precisamos primeiro manter somente os nao-concentrados da base original
gps_join_linha_fora1_new <- gps_join_linha_fora1 %>%
  # filtrar somente os nao-concetrados, ou seja, as concentracoes com menos de 10 pontos
  filter(n < 10) %>%
  ungroup()

# Em seguida, juntar os nao-concentrados com os concentrados corridigos 
gps_join_linha_fora1_new <- gps_join_linha_fora1_new %>%
  # a funcao rbind faz essa juncao
  rbind(gps_join_linha_fora2) %>%
  # ordenar novamente por veiculo e hora
  arrange(ordem, datahora)














# 2) Verificar se esses pontos podem fazer parte de outra(s) linha(s) -------------------------

# para verificar se esses pontos que estao fora da linha  podem fazer parte de outras linhas,
# vamos pegar o shape de todas as linhas do sistema e ver se esses pontos se encaixam de alguma forma
# em alguma outra linha

# 2.1) Fazer novamente o buffer, mas em relacao a todas as outras linhas (isso pode demorar)
linhas_shape_buffer <- st_buffer(linhas_shape_select, dist = 0.001)

# fazer novamenta a juncao da ida com a volta  (isso pode demorar)
linhas_shape_buffer <- linhas_shape_buffer %>%
  group_by(linha) %>%
  summarise(do_union = TRUE)

# 2.2) Fazer entao a juncao espacial das duas bases

# primeiro, selecionar somente as colunas necessarias
gps_join_linha_fora1_new <- gps_join_linha_fora1_new %>% select(datahora, ordem, hora, lon, lat)


# calcular a quantidade de pontos de gps por veiculo
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
