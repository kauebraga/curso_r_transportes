# Esse script faz o calculo de indicadores de acessibilidade cumulativa para a matriz de tempo
# de viagem calculada na etapa anterior

library(data.table) # install.packages('data.table')
library(dplyr)
library(sf)


# 0) Fazer download do arquivo com as oportunidades -------------------------------------------
hex_rio <- aopdata::read_landuse(city = "rio", year = 2019, geometry = TRUE)
# salvar
saveRDS(hex_rio, "data/hex_rio.rds")


# 1) Abrir arquivos ---------------------------------------------------------------------------

ttmatrix <- fread("data/ttmatrix_rio.csv")
hex_rio <- readr::read_rds("data/hex_rio.rds") %>% 
  # deletar a geometria para agilizar o processamento - nao vamos precisar dela agora
  st_set_geometry(NULL)
hex_rio <- hex_rio %>%
  filter(year == 2019)

# mapview(hex_rio, zcol = "P001")

# variaveis a serem selecionadas
help("read_landuse", "aopdata")
# sociodemographic	P001	Total number of residents	
# sociodemographic	R001	Average household income per capita	R$ (Brazilian Reais), values in 2010
# sociodemographic	R003	Income decile group	1 (poorest), 2, 3, 4, 5, 6, 7, 8, 9, 10 (richest)

# E001	Total number of public schools	
# E002	Number of public schools - early childhood	
# E003	Number of public schools - elementary schools	
# E004	Number of public schools - high schools	
# S001	Total number of healthcare facilities	
# S002	Number of healthcare facilities - low complexity	
# S003	Number of healthcare facilities - medium complexity	
# S004	Number of healthcare facilities - high complexity	


# Filtrar apenas colunas com info demograficas na origem
hex_orig <- hex_rio %>% dplyr::select(id_hex, 
                                      # variaveis de populacao - cor
                                      pop_total = P001, 
                                      # variaveis de renda
                                      renda_capita = R001, decil = R003)

# Filtrar apenas colunas com info de uso do solo no destino
hex_dest <- hex_rio %>% dplyr::select(id_hex, 
                                      # variaveis de emprego
                                      empregos_total = T001,
                                      # variaveis de saude, 
                                      saude_total = S001, saude_baixa = S002, saude_media = S003, saude_alta = S004,
                                      # variaveis de educacao
                                      edu_total = E001, edu_infantil = E002, edu_fundamental = E003, edu_medio = E004)


# Join dados de origem na matrix de tempo de viagem
ttmatrix <- left_join(ttmatrix, hex_orig,
                      by = c("origin" = "id_hex"))

# Merge dados de destino na matrix de tempo de viagem
ttmatrix <-  left_join(ttmatrix, hex_dest,
                       by = c("destination" = "id_hex"))



# calcular acessibilidade ---------------------------------------------------------------------

acess_atual <- ttmatrix %>%
  # excluir os tempos de viagem menor que 60 minutos
  filter(ttime_atual <= 60) %>%
  # agrupar pela origem
  mutate(un = 1) %>%
  group_by(origin) %>%
  # calcular a soma das oportunidades nos destinos
  summarise(
    # acess_empregos
    acess_empregos_total = sum(empregos_total, na.rm = TRUE),
    acess_saude_total = sum(saude_total, na.rm = TRUE),
    acess_edu_total = sum(edu_total, na.rm = TRUE),
    acess_saude_baixa = sum(saude_baixa, na.rm = TRUE),
    acess_saude_alta = sum(saude_alta, na.rm = TRUE),
    acess_un = sum(un, na.rm = TRUE)
    
    ) %>%
  ungroup() %>%
  # identificar tipo
  mutate(tipo = "atual")

acess_filtrado <- ttmatrix %>%
  # excluir os tempos de viagem menor que 60 minutos
  filter(ttime_filtrado <= 60) %>%
  mutate(un = 1) %>%
  # agrupar pela origem
  group_by(origin) %>%
  # calcular a soma das oportunidades nos destinos
  summarise(
    acess_empregos_total = sum(empregos_total, na.rm = TRUE),
    acess_saude_total = sum(saude_total, na.rm = TRUE),
    acess_edu_total = sum(edu_total, na.rm = TRUE),
    acess_saude_baixa = sum(saude_baixa, na.rm = TRUE),
    acess_saude_alta = sum(saude_alta, na.rm = TRUE),
    acess_un = sum(un, na.rm = TRUE)) %>%
  ungroup() %>%
  # identificar tipo
  mutate(tipo = "filtrado")


# juntar arquivos
acess <- rbind(acess_atual, acess_filtrado)

# trazer geometria de volta
hex_rio <- readr::read_rds("data/hex_rio.rds") 
hex_rio <- hex_rio %>%
  filter(year == 2019)

acess <- left_join(
  acess,
  hex_rio %>% select(id_hex),
  by = c("origin" = "id_hex")
) %>% st_sf(crs = 4326)

# salvar o arquivo
saveRDS(acess, "data/acess_rio.rds")
