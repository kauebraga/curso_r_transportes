# Esse script faz o calculo de uma matriz de tempo de viagem a partir de pontos OD e de um
# arquivo de GTFS

# carregar bibliotecas
options(java.parameters = '-Xmx15G') # vai depender da memoria da maquina de cada um
library(dplyr)
library(r5r)
library(data.table)



# 1) Abrir pontos ------------------

pontos_rio <- fread("r5r/points/points_rio_todo.csv")


# 2) Definir parametros de roteamento do r5r -------------------------------------------------------


mode <- c("WALK", "TRANSIT")
max_walk_dist <- 1000   # meters
max_trip_duration <- 180 # minutes
departure <- "16-11-2021 06:00:00"
departure_datetime <- as.POSIXct(departure, format = "%d-%m-%Y %H:%M:%S")





# 2) Rodar ttmatrix ---------------------------------------------------------------------------

# 1 - ttmatrix - GTFS atual 

r5r_core <- setup_r5(data_path = "r5r/network/rio_atual", verbose = FALSE)

# 3.1) calculate a travel time matrix
ttm1 <- travel_time_matrix(r5r_core = r5r_core,
                           origins = pontos_rio,
                           destinations = pontos_rio,
                           mode = mode,
                           departure_datetime = departure_datetime,
                           time_window = 120,
                           max_walk_dist = max_walk_dist,
                           max_trip_duration = max_trip_duration)



# 2 - ttmatrix - GTFS 2023

r5r_core <- setup_r5(data_path = "r5r/network/rio_2023", verbose = TRUE)

ttm2 <- travel_time_matrix(r5r_core = r5r_core,
                           origins = pontos_rio,
                           destinations = pontos_rio,
                           mode = mode,
                           departure_datetime = departure_datetime,
                           time_window = 120,
                           max_walk_dist = max_walk_dist,
                           max_trip_duration = max_trip_duration)

# juntar matrizes
ttm <- full_join(ttm1, ttm2,
                 by = c("fromId", "toId"),
                 suffix = c("_atual", "_2023"))


# renomear variaveis
ttm <- ttm %>% 
  mutate(city = "rio", mode = "tp") %>%
  dplyr::select(city, mode, origin = fromId, destination = toId, 
                ttime_atual = travel_time_atual, ttime_2023 = travel_time_2023)




# 3) Salvar -----------------------------------------------------------------------------------
fwrite(ttm, "data/ttmatrix_rio.csv")

