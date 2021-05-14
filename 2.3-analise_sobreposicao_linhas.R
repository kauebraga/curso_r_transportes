library(dplyr) # manipulacao de dados
library(data.table) # abrir e salvar dados (por enquanto)
library(ggplot2) # graficos e mapas
library(mapview) # visualizacao de dados espaciais
library(sf) # operacoes com dados espaciais


# Selecionar linhas que podem ter alguma sobreposição
# Fazer buffer em relação a uma das linhas;
# Identificar sobreposição através do st_join


# 1) Abrir arquivo com as linhas ---------------------------

linhas_shape <- st_read("data-raw/2020-ago-30/2020-ago-30.shp")
head(linhas_shape)

# 1.1) Selecionar colunas necessarias
linhas_shape_select <- linhas_shape %>%
  # selecionar a coluna 'ref' e renomea-la para 'linha'
  select(linha = ref, name)


# 2) Criar buffer em torno das linhas para ajudar a intersecao espacial ---------------

# o argumento em distancia esta em graus, onde o 1 grau representa 111139 metros
linhas_shape_buffer <- st_buffer(linhas_shape_select, dist = 0.001)


# como temos o shape da ida e da volta, eh interessante junta-los em uma mesma observacao
# isso eh feito atraves da combinacao 'group_by' e 'summarise', so que agora eh uma operacao espacial
# que vai agrupar espacialmente as observacoes

# isso vai ser feito tanto para 
linhas_shape_select <- linhas_shape_select %>%
  group_by(linha) %>%
  summarise(do_union = TRUE)

linhas_shape_buffer <- linhas_shape_buffer %>%
  group_by(linha) %>%
  summarise(do_union = TRUE) %>%
  # renomear coluna da linha para ficar mais claro
  rename(linha_intersecao = linha)




# pegar amostra
mapview(linhas_shape_select)

linhas_shape_select_filter <- linhas_shape_select %>%
  filter(linha %in% c(315)) %>%
  mutate(linha_tamanho = st_length(.))

# linhas_shape_buffer_filter <- linhas_shape_buffer %>%
  # filter(linha %in% c(361))

mapview(linhas_shape_buffer_filter) + linhas_shape_select_filter

a <- st_intersection(linhas_shape_select_filter, linhas_shape_buffer) %>%
  mutate(intersecao_tamanho = st_length(.)) %>%
  # calcular a porcentagem da linha que faz intersecao com a outra linha
  mutate(intersecao_perc = as.numeric(intersecao_tamanho) / as.numeric(linha_tamanho))

mapview(a) + linhas_shape_buffer_filter
