# Esse script faz o calculo de uma matriz de tempo de viagem a partir de pontos OD e de um
# arquivo de GTFS

# carregar bibliotecas
options(java.parameters = '-Xmx6G') # vai depender da memoria da maquina de cada um
library(dplyr)
library(r5r)
library(data.table)
library(readr) # install.packages('readr)


# 1) Abrir pontos ------------------

pontos_rio <- readr::read_csv("r5/points_rio_todo.csv")


# 2) Definir parametros de roteamento do r5r -------------------------------------------------------
# checar valor de data no gtfs
gtfs <- gtfstools::read_gtfs("r5/rio_atual/gtfs_rio_atual.zip")
calendar <- gtfs$calendar
View(calendar)
date <- "18-07-2022"
time <- "06:00:00"

# setar argumentos
mode <- c("WALK", "TRANSIT")
max_walk_time <- 30  # minutes
max_trip_duration <- 180 # minutes
departure <- paste0(date, " ", time)
departure_datetime <- as.POSIXct(departure, format = "%d-%m-%Y %H:%M:%S")

# outros parametros
time_window <- 1
draws_per_minute <- 1



# 2) Rodar ttmatrix ---------------------------------------------------------------------------

# 1 - ttmatrix - GTFS atual 

# criar network to r5r (so necessario 1 vez)
r5r_core <- setup_r5(data_path = "r5/rio_atual", verbose = FALSE)


# 3.1) calculate a travel time matrix
ttm1 <- travel_time_matrix(r5r_core = r5r_core,
                           origins = pontos_rio,
                           destinations = pontos_rio,
                           mode = mode,
                           departure_datetime = departure_datetime,
                           max_walk_time = 60,
                           max_trip_duration = max_trip_duration,
                           time_window = 1,
                           draws_per_minute = 1)

# checar a quantidade de pontos que o r5r retornou
unique(ttm1$from_id) %>% length()


# 2 - ttmatrix - GTFS modificado

# criar network to r5r (so necessario 1 vez)
r5r_core <- setup_r5(data_path = "r5/rio_filtrado", verbose = TRUE)

ttm2 <- travel_time_matrix(r5r_core = r5r_core,
                           origins = pontos_rio,
                           destinations = pontos_rio,
                           mode = mode,
                           departure_datetime = departure_datetime,
                           max_walk_time = 60,
                           max_trip_duration = max_trip_duration,
                           time_window = 1,
                           draws_per_minute = 1)

# juntar matrizes
ttm <- full_join(ttm1, ttm2,
                 by = c("fromId", "toId"),
                 suffix = c("_atual", "_filtrado"))


# renomear variaveis
ttm <- ttm %>% 
  mutate(city = "rio", mode = "tp") %>%
  dplyr::select(city, mode, origin = from_id, destination = to_id, 
                ttime_atual = travel_time_atual, ttime_filtrado = travel_time_filtrado)




# 3) Salvar -----------------------------------------------------------------------------------
fwrite(ttm, "data/ttmatrix_rio.csv")

