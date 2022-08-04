# Esse script faz o calculo de uma matriz de tempo de viagem a partir de pontos OD e de um
# arquivo de GTFS

# carregar bibliotecas
options(java.parameters = '-Xmx12G') # vai depender da memoria da maquina de cada um
library(dplyr)
library(r5r)
library(data.table)


# 1) Abrir pontos ------------------

pontos_rio <- data.table::fread("r5/points_rio_todo.csv")


# 2) Definir parametros de roteamento do r5r -------------------------------------------------------

# checar valor de data no gtfs
gtfs <- gtfstools::read_gtfs("r5/rio_atual/gtfs_rio_atual.zip")
calendar <- gtfs$calendar
View(calendar)

# tem que ser: 
# - um dia dentro do intervalo start_date e end_date 
# - um dia de semana
date <- "18-07-2022"
# selecionar tambem a hora de partida
time <- "06:00:00"

# setar argumentos ----------
# definir os modos de transporte do roteamento
?travel_time_matrix
mode <- c("WALK", "TRANSIT")
# definir o tempo maximo de caminhada da origem ate a parada de onibus / da parada de onibus pro destino
max_walk_time <- 30  # minutes
# definir o tempo maximo da viagem
max_trip_duration <- 120 # minutes
# compor o dia e a hora da viagem
departure <- paste0(date, " ", time)
departure_datetime <- as.POSIXct(departure, format = "%d-%m-%Y %H:%M:%S")

# outros parametros
# esse parametro vai definir a janela de calculo da matriz, e eh definido em minutos
# por exemplo: como definimos a hora de partida como 06h, um time_window de 120
# calcularia uma matriz por minuto por 120 minutos - de 6h as 8h
# isso eh importante pq o tempo de viagem pode variar bastante a cada minuto q vc parte de "casa",
# entao calcular uma mtariz por minuto suaviza isso

# para fins de teste, vamos deixar como um (bem masi rapido)
time_window <- 1
# esse argumento eh importante para gtfst tipo frequencies, e deixa o processametno bem amis rapido
draws_per_minute <- 1



# 2) Rodar ttmatrix ---------------------------------------------------------------------------

# 1 - ttmatrix - GTFS atual 

# criar network to r5r (so necessario 1 vez)
a <- Sys.time()
r5r_core <- setup_r5(data_path = "r5/rio_atual", verbose = FALSE)
a1 <- Sys.time()
a1 - a
# olhar arquivo network_settings.json na pasta


# 3.1) calculate a travel time matrix
a <- Sys.time()
ttm1 <- travel_time_matrix(r5r_core = r5r_core,
                           origins = pontos_rio,
                           destinations = pontos_rio,
                           mode = mode,
                           departure_datetime = departure_datetime,
                           max_walk_time = max_walk_time,
                           max_trip_duration = max_trip_duration,
                           time_window = time_window,
                           draws_per_minute = draws_per_minute)
a1 <- Sys.time()
a1 - a
# checar a quantidade de pontos que o r5r retornou
unique(ttm1$from_id) %>% length()
# checar a quantidade de pontos alcancados por cada origem
pontos_por_origem <- ttm1 %>%
  count(from_id)

saveRDS(ttm1, "data/ttmatrix_atual.rds")
rm(ttm1)
gc()

# 2 - ttmatrix - GTFS modificado

# criar network to r5r (so necessario 1 vez)
r5r_core <- setup_r5(data_path = "r5/rio_filtrado", verbose = TRUE)

ttm2 <- travel_time_matrix(r5r_core = r5r_core,
                           origins = pontos_rio,
                           destinations = pontos_rio,
                           mode = mode,
                           departure_datetime = departure_datetime,
                           max_walk_time = 30,
                           max_trip_duration = max_trip_duration,
                           time_window = 1,
                           draws_per_minute = 1)

# juntar matrizes
ttm <- full_join(ttm1, ttm2,
                 by = c("from_id", "to_id"),
                 suffix = c("_atual", "_filtrado"))
ttm %>% filter(is.na(travel_time_p50_atual)) %>% nrow()
ttm %>% filter(is.na(travel_time_p50_filtrado)) %>% nrow()

# renomear variaveis
ttm <- ttm %>% 
  mutate(city = "rio", mode = "tp") %>%
  dplyr::select(city, mode, origin = from_id, destination = to_id, 
                ttime_atual = travel_time_p50_atual, ttime_filtrado = travel_time_p50_filtrado)




# 3) Salvar -----------------------------------------------------------------------------------
fwrite(ttm, "data/ttmatrix_rio.csv")

