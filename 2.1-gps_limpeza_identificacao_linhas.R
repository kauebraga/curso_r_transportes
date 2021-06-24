# Esse script primeiramente faz uma limpeza dos dados de GPS, excluindo altas concentracoes indevidas
# Por fim, propoe e aplica um metodo que pega pontos de GPS que estao fora da linha identificada na base
# e identifica possiveis linhas corretas desses pontos

# Conteudo:
# 1) Abrir arquivos de GPS e linhas da etapa anterior
# 2) Juntar shapes de ida e volta da linha
# 3) Verificar quais pontos de GPS estão dentro/fora dessa linha
# 4) Fazer limpeza nos dados de GPS
# 5) Verificar se esses pontos podem fazer parte de outra(s) linha(s)





# 0) Carregar pacotes -------------------------------------------------------------------------

library(dplyr) # manipulacao de dados
library(data.table) # abrir e salvar dados (por enquanto)
library(ggplot2) # graficos e mapas
library(mapview) # visualizacao de dados espaciais
library(sf) # operacoes com dados espaciais
library(readr) # abrir e salvar dados
options(scipen=999)


# 1) Abrir arquivos de GPS e linhas da etapa anteriorA -------------------------------

# 1.1) Abrir dados de GPS
gps <- read_rds("data/gps_rio_amostra_linha.rds")

# selecionar somente horarios do pico manha
gps <- gps %>% filter(hora %in% c("05", "06", "07", "08"))

# 1.2) Abrir shape da linha
linhas_shape_buffer <- read_rds("data/linhas_rio_amostra.rds")



# 2) Juntar shapes de ida e volta da linha ----------------------------------------------------
# como temos o shape da ida e da volta, eh interessante junta-los em uma mesma observacao
# isso eh feito atraves da combinacao 'group_by' e 'summarise', so que agora eh uma operacao espacial
# que vai agrupar espacialmente as observacoes
linhas_shape_buffer <- linhas_shape_buffer %>%
  group_by(linha) %>%
  summarise(do_union = TRUE)









# 3) Verificar quais pontos de GPS estão dentro/fora dessa linha com sf::st_join --------------

#' Filtrar somente os pontos de GPS que estao FORA da linha
#' Para fazer isso, eh preciso filtrar os pontos de GPS que nao tiveram equivalente na juncao espacial
#' Esses pontos sao identificados quando um ponto de GPS tem um valor NA em alguma coluna que veio do shape das linhas

# 3.1) Para identificar a linha que esta no GPS e a que vai vir do shape das linhas, eh preciso diferenciar
# o nome das colunas de linha das duas bases:
gps <- gps %>% rename(linha_gps = linha)
linhas_shape_buffer <- linhas_shape_buffer %>% rename(linha_shape = linha)

# Ordenar os dados de GPS por carro e linha
gps <- arrange(gps, ordem, datahora)

# 3.2) Realizar a intersecao espacial
gps_join_linha <- st_join(gps, linhas_shape_buffer)

# a coluna 'linha_shape' veio dos shapes das linhas, entao todas as observacoes em que ela esteja como NA
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
mapview(gps_join_linha_fora) + mapview(linhas_shape_buffer)










# 4) Fazer limpeza nos dados de GPS -----------------------------------------------------------

#' Foram observados diversos pontos mortos de GPS na linha, o que estava prejudicando as analises
#' Situacoes como centanas de registros de GPS de uma linha sendo identificados em uma mesma localidade
#' Ainda nao eh possivel saber se essas concentracoes de pontos sao uma garagem, final de linha, ou
#' qualquer outra coisa
#' Para contornar isso, essa etapa identifica quando essas concentracoes acontecem e diminui a quantidade
#' de pontos das concentracoes, de forma a garantir que os pontos de GPS que estao na base sejam de
#' quando o veiculo esteja realmente em movimento


# 4.1) Ordenar os dados de GPS por carro e linha
gps_join_linha_fora1 <- arrange(gps_join_linha_fora, ordem, datahora)

# # Extrair o momento preciso
# gps_join_linha_fora1 <- gps_join_linha_fora1 %>% mutate(momento := as.ITime(format(datahora, "%H:%M:%S")))
# gps_join_linha_fora1 <- gps_join_linha_fora1 %>% group_by(ordem) %>% mutate(headway := -(as.integer(momento - dplyr::lead(momento)))) %>% ungroup()

# Calcular a distancia entre um ponto e o seu ponto anterior
# Isso vai ajudar a identificar quando um veiculo se manteve imovel/se moveu muito pouco

# 4.2) Estabelecer funcao para calcular dist entre um ponto e seu anterior
get.dist <- function(lon, lat) geosphere::distHaversine(tail(cbind(lon,lat),-1), head(cbind(lon,lat),-1))

# 4.3) Calcular essa distancia, agrupando por veiculo
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

# Primeiramente, eh calculado o tamanho da concentracao (o tamanho de pontos dentro da concentracao)
# A funcao 'add_count' adiciona uma nova coluna com a quantidade de cada 'seq'
gps_join_linha_fora1 <- gps_join_linha_fora1 %>%
  add_count(seq)

# Fazer entao o filtro para concentracoes que tenham mais de 15 pontos - essas vao ser reduzidas
gps_join_linha_fora2 <- gps_join_linha_fora1 %>%
  # fazer o filtro de 15 pontos
  filter(n >= 15)  %>%
  # agruparar por veiculo e sequencia
  group_by(ordem, seq) %>%
  # a funcao 'slice' serve para extrair as observacoes por posicao - nesse caso, vamos tirar a 
  # primeira (1) e a ultima (n())
  slice(1, n()) %>%
  ungroup()

# Os pontos concentrados que foram reduzidos precisam ser colados a base original
# Para isso, precisamos primeiro manter somente os nao-concentrados da base original
gps_join_linha_fora1_new <- gps_join_linha_fora1 %>%
  # filtrar somente os nao-concetrados, ou seja, as concentracoes com menos de 15 pontos
  filter(n < 15) %>%
  ungroup()

# Em seguida, juntar os nao-concentrados com os concentrados corridigos 
gps_join_linha_fora1_new <- gps_join_linha_fora1_new %>%
  # a funcao rbind faz essa juncao
  rbind(gps_join_linha_fora2) %>%
  # ordenar novamente por veiculo e hora
  arrange(ordem, datahora)







# 5) Verificar se esses pontos podem fazer parte de outra(s) linha(s) -------------------------

# para verificar se esses pontos que estao fora da linha  podem fazer parte de outras linhas,
# vamos pegar o shape de todas as linhas do sistema e ver se esses pontos se encaixam de alguma forma
# em alguma outra linha
# isso sera feito atraves ca juncao espacial st_join

# 5.1) Fazer novamente o buffer, mas em relacao a todas as outras linhas (isso pode demorar)
linhas <- st_read("data-raw/2020-ago-30/2020-ago-30.shp")
# selecionar a coluna 'ref' e renomea-la para 'linha'
linhas <- linhas %>% select(linha = ref, name, operator)

# fazer novamente a juncao da ida com a volta  (isso pode demorar)
linhas <- linhas %>%
  group_by(linha) %>%
  summarise(operator = first(operator), 
            do_union = TRUE)


# fazer entao o buffer
linhas_shape_buffer <- st_transform(linhas, crs = 31983)
linhas_shape_buffer <- st_buffer(linhas_shape_buffer, dist = 100)
linhas_shape_buffer <- st_transform(linhas_shape_buffer, crs = 4326)



# 5.2) Fazer entao a juncao espacial das duas bases

# primeiro, selecionar somente as colunas necessarias
gps_join_linha_fora1_new <- gps_join_linha_fora1_new %>% select(datahora, linha_gps, ordem, hora, lon, lat)

# calcular a quantidade de pontos de gps
# isso vai basicamente criar uma coluna com a quantidade de pontos de GPS - para servir para comparacao mais a frente
gps_join_linha_fora1_new <- gps_join_linha_fora1_new %>%
  add_count(linha_gps, name = "pontos_totais")

# juncao espacial: a base com os pontos fora da linha fica no lado esquerdo, enquanto a base com as linhas do lado direito
# essa operacao vai dizer em qual(is) linha(s) estao cada ponto de GPS
# como varias linhas podem tem intersecoes, um mesmo ponto de GPS pode se multiplicar em varias observaoes - uma por linha
gps_join_linhas <- st_join(gps_join_linha_fora1_new, linhas_shape_buffer)


# calcular a proporcao dos pontos que esta dentro de todas as outras linhas do sistema
# essa porcentagem pode ser liga como "a porcentagem de pontos que nao sao da linha registrada no GPS que estao dentro dessa linha"
# dessa forma, por ex, se um linha tiver uma porcentagem de 80%, significa que 80% dos pontos com informacao errada do GPS
# estao dentro dessa linha

a <- gps_join_linhas %>%
  st_set_geometry(NULL) %>%
  group_by(linha, operator) %>%
  summarise(pontos_n = n(), 
            pontos_totais =  first(pontos_totais)) %>%
  mutate(perc_n = pontos_n/pontos_totais)

# selecionar somente as linhas que sejam do mesmo operador
# primeiro, extrair a lihna do GPS
linha_gps <- gps$linha_gps
# a partir dessa linha, extrair o operador
operador <- linhas_shape_buffer %>% filter(linha %in% linha_gps)

# filtrar entao somente o operador
a_operador <- a %>%
  filter(operator == operador$operator)



# verificar
mapview(filter(linhas_shape_buffer, linha %in% c(315))) +
  mapview(gps_join_linha_fora1_new)

mapview(filter(linhas_shape_buffer, linha %in% c(361))) +
  mapview(gps_join_linha_fora1_new)
