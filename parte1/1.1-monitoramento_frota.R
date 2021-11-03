# Esse script faz um monitoramento da frota de GPS, primeiramente identificando a frota operante
# de algumas linhas por dia e depois fazendo uma comparacao dessa frota com a frota que esta prevista

# Conteudo:
# Identificando a frota operante:
# 1) Abrir arquivo de GPS
# 2) Selecionar e renomear colunas necessarias
# 3) Extrair o dia e a hora
# 4) Calcular a quantidade de pontos de GPS por dia, veiculo e linha
# 5) Calcular a quantidade de veiculos (frota) por linha
# 6) Plot
# 7) Salvar bases de dados
# 
# Comparar com a frota que era pra estar rodando
# 8) Abrir dados
# 9) Trazer a coluna da quantidade de veiculos da base de GPS p/ a base de frotas
# 10) Plot




# 0) Carregar pacotes -------------------------------------------------------------------------

library(dplyr)
library(data.table)
library(ggplot2)






# 1) Abrir arquivo de GPS ------------------------------------------
gps <- fread("data-raw/gps_rio_amostra.csv")




# 2) Selecionar e renomear colunas necessarias ----------------------------------------
# verificar as colunas
colnames(gps)

gps_colunas <- gps %>% 
  select(datahora, veiculo_id = ordem, linha, lon = longitude, lat = latitude)





# 3) Extrair o dia e a hora ---------------------------------------
# a coluna de datahora contem o dia e a hora no formato 2019-07-29 00:00:01
# para facilitar as operacoes, vamos extrair o dia e a o momento do registro e criar 1 coluna para cada
gps_colunas <- gps_colunas %>%
  # criar as colunas de dia e hora
  mutate(dia = format(datahora, "%d/%m/%y"),
         momento = format(datahora, "%H:%M:%S"))





# 4) Calcular a quantidade de pontos de GPS por dia, veiculo e linha -----------------------------
gps_veiculos_linhas_pontos <- gps_colunas %>%
  # agrupar por dia, veiculo e linha
  group_by(dia, veiculo_id, linha) %>%
  # calcular a quantidade de pontos por veiculo e linha
  summarise(pontos_n = n()) %>%
  ungroup()



# 5) Calcular a quantidade de veiculos (frota) por linha -----------------------
# ao final, cada obsercao 
gps_veiculos_linhas <-  gps_veiculos_linhas_pontos %>%
  # agrupar por dia e linha
  group_by(dia, linha) %>%
  # calcular a quantidade de veiculos por linha
  summarise(veiculos_n = n()) %>%
  ungroup()




# 6) Plot --------------------------------

# iniciar o ggplot
ggplot() +
  # criar o grafico de barras
  geom_col(data = gps_veiculos_linhas, aes(x = as.factor(linha), y = veiculos_n)) +
  # ajustar texto
  labs(x = "Linha", y = "Veiculos", title = "Quantidade de veiculos, por dia")

# salvar o plot
ggsave("figuras/plot1.png")





# 7) Salvar os dados ----------------------------
fwrite(gps_veiculos_linhas, "data/rio_veiculos_linhas.csv")






# bonus: calcular quantidade de veiculos por linha e hora
gps_colunas_hora <- gps_colunas %>%
  # calcular a hora de cada registro
  mutate(hora = format(datahora, "%H")) %>%
  # agrupar por dia, veiculo e linha
  group_by(dia, hora, veiculo_id, linha) %>%
  # calcular a quantidade de pontos por veiculo e linha
  summarise(pontos_n = n()) %>%
  ungroup() %>%
  # agrupar e contar agora por dia, hora e linha p/ calcular veciulos por hora
  count(dia, hora, linha)



# Reiniciar secao do R



# 8) Abrir dados  --------------------------

# 8.1) Abrir dados com a quantidade de veiculos por linha
veiculos_linhas <- fread("data/rio_veiculos_linhas.csv")

# transformar a coluna de linha para character
veiculos_linhas <- veiculos_linhas %>%
  # reescrever a coluna com o mesmo nome
  mutate(linha = as.character(linha))

# 8.2) Abrir dados com a frota determinada 
frota_onibus <- fread("data-raw/tabela_frota_rio.csv")

# Filtrar a frota somente das linhas qque estamos analisando
frota_onibus_filt <- frota_onibus %>%
  # fazer a selecao
  # o codigo %in% faz a selecao de varios itens em um vetor
  filter(linha_codigo %in% veiculos_linhas$linha)



# 9) Trazer a coluna da quantidade de veiculos da base de GPS p/ a base de frotas ---------------
# a funcao lef_join vai manter todas as observaoes do lado esquerdo da juncao
frota_onibus_gps <- left_join(
  # base dados do lado esquerdo
  x = frota_onibus_filt,
  # base de dados do lado direito
  y = veiculos_linhas,
  # escolher a coluna que vai servir para relacionar as bases de dados
  # como as colunas tem nome diferente, isso precisa estar explicito na seguinte forma
  by = c("linha_codigo" = "linha")
  
)




# 10) Plot -----------------------

# primeiro, calcular a diferenca abs entre a frota prevista e operada
frota_onibus_gps <- frota_onibus_gps %>%
  mutate(frota_dif = veiculos_n - frota_determinada)

# iniciar ggplot
ggplot() +
  # grafico de barras com a quantidade de frota prevista e operada
  geom_col(data = frota_onibus_gps, aes(x = linha_codigo, y = frota_dif, fill = frota_dif > 0))+
  # ajustar texto
  labs(x = "Linha", y = "Veiculos", title = "Diferenca absoluta entre veiculos operados e previstos, por dia")



# salvar o plot
ggsave("figuras/plot2.png")




