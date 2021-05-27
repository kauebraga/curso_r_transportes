# Esse script faz uma analise de sobreposicao de linhas, identificando linha por linhas quais sao
# suas possiveis sobreposicoes com outras linhas

# Conteudo:
# 1) Abrir arquivo com as linhas
# 2) Criar buffer em torno das linhas para realizar a intersecao espacial
# 3) Realizar a intersecao espacial para identificar sobreposicoes de linhas






# 0) Carregar pacotes -------------------------------------------------------------------------

library(dplyr) # manipulacao de dados
library(data.table) # abrir e salvar dados (por enquanto)
library(ggplot2) # graficos e mapas
library(mapview) # visualizacao de dados espaciais
library(sf) # operacoes com dados espaciais







# 1) Abrir arquivo com as linhas ------------------------------

linhas_shape <- st_read("data-raw/2020-ago-30/2020-ago-30.shp")
head(linhas_shape)

# 1.1) Selecionar colunas necessarias
linhas_shape_select <- linhas_shape %>%
  # selecionar a coluna 'ref' e renomea-la para 'linha'
  select(linha = ref, name)






# 2) Criar buffer em torno das linhas para realizar a intersecao espacial ---------------

# o argumento em distancia esta em graus, onde o 1 grau representa 111139 metros
linhas_shape_buffer <- st_buffer(linhas_shape_select, dist = 0.001)


#' como temos o shape da ida e da volta, eh interessante junta-los em uma mesma observacao
#' isso eh feito atraves da combinacao 'group_by' e 'summarise', so que agora eh uma operacao espacial
#' que vai agrupar espacialmente as observacoes

# isso vai ser feito tanto para as linhas como para as linhas bufferizadas
linhas_shape_select <- linhas_shape_select %>%
  group_by(linha) %>%
  summarise(do_union = TRUE)

linhas_shape_buffer <- linhas_shape_buffer %>%
  group_by(linha) %>%
  summarise(do_union = TRUE) %>%
  # renomear coluna da linha para ficar mais claro
  rename(linha_intersecao = linha)







# 3) Realizar a intersecao espacial para identificar sobreposicoes de linhas ------------------

#' a sobreposicao ira sobrepor duas camadas da linhas:
#' 1 - camada com a LINHA que queremos identificar sobreposicao
#' 2- camada com as LINHAS BUFFERIZADAS que sao utilizadas para checar a sobreposicao
#' a funcao de intersecao espacial nesse caso tb funciona como um 'left_join' espacial

# 3.1) Para demonstrar a funcionalidade, primeiro vamos selecionar uma linha para buscar possiveis sobreposicoes
linha_teste <- 315

# 3.2) Filtrar a linha teste na camada 1 (o filtro nao sera necessario na camada 2)
linhas_shape_select_filter <- linhas_shape_select %>%
  filter(linha %in% linha_teste) %>%
  # calcular o comprimento da linha para futura comparacao
  mutate(linha_comprimento = st_length(.))

# verificar atraves da visualizacao (melhor nao rodar caso computador seja lento)
mapview(linhas_shape_buffer_filter) + linhas_shape_select_filter


# 3.3) Realizar a operacao de sobreposicao com a funcao `st_intersection` 
#' essa funcao realiza quebras na geometria principal (linha) onde acontece alguma intersecao
#' com as linhas bufferizadas.. assim eh possivel avaliar o tamanho da intersecao
#' a geometria resultante vai ser a geometria da intersecao entre os dois shapes, com a identificacao
#' da linha fez intersecao com a nossa linha principal 

# aplicar funcao
linha_sobreposicao <- st_intersection(linhas_shape_select_filter, linhas_shape_buffer) %>%
  # calcular entao o tamanho dessa intersecao
  mutate(intersecao_comprimento = st_length(.)) %>%
  # calcular a porcentagem da linha que faz intersecao com a outra linha
  mutate(intersecao_perc = as.numeric(intersecao_comprimento) / as.numeric(linha_comprimento)) %>%
  # ordenar os dados pela intersecao_perc 
  # (colocar a coluna entre desc() significa que a ordenacao sera do maior para o menor)
  arrange(desc(intersecao_perc))

# verificar resultado
head(linha_sobreposicao)

# visualizar resultado
mapview(a) + linhas_shape_buffer_filter
