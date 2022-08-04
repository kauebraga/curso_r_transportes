options(scipen = 999999)
library(tidyr) # install.packages("tidyr")
library(ggplot2) # install.packages("ggplot2")
library(dplyr)
library(sf)
library(mapview)


# abrir city limits
# city_shape <- geobr::read_municipality()


# abrir access
acess <- readRDS("acess_rio.rds")



# plotar o acesso a a cada uma das atividades --------------------------
ggplot() +
  geom_sf(data = acess %>% filter(tipo == "atual"), aes(fill = acess_empregos_total), color = NA)+
  scale_fill_viridis_c(option = "inferno")+
  theme_void()
ggplot() +
  geom_sf(data = acess %>% filter(tipo == "atual"), aes(fill = acess_edu_total), color = NA)+
  scale_fill_viridis_c(option = "inferno")+
  theme_void()
ggplot() +
  geom_sf(data = acess %>% filter(tipo == "atual"), aes(fill = acess_saude_total), color = NA)+
  scale_fill_viridis_c(option = "inferno")+
  theme_void()
  
  

# comparar os valores de acessibilidade --------------------
access_comp <- acess %>%
  gather("ind", "valor", acess_empregos_total:acess_edu_total) %>%
  spread(tipo, valor) %>%
  # calculate abs diffs
  mutate(dif_abs = atual - filtrado,
         dif_rel = (atual - filtrado)/filtrado) %>%
  # dif_rel = round((depois-antes)/antes, 2)) %>%
  st_sf(crs = 4326)



# plotar a diferenca absoluta
ggplot()+
  geom_sf(data = access_comp %>% filter(ind == "acess_empregos_total"), aes(fill = dif_abs), color = NA)+
  # geom_sf(data= city_shape_map, fill = NA)+
  scale_fill_distiller(
    palette = "Spectral", direction = 1
    # palette = "PuBu", direction = 1
                       # limits = c(-1,1)*limits_ind$dif_rel_tc
                       # breaks = c(-30000, 0, 30000),
                       # labels = c("-30 mil", 0, "30 mil")
                       # , label = label_percent(accuracy = 1)
  )+
  theme_void()

# verificar alguns desses valores estranhgos
a <- access_comp %>% filter(ind == "acess_empregos_total") %>%
  filter(dif_abs < - 10000 | dif_abs > 100000)

mapview(a, zcol = "dif_abs")



ggplot()+
  geom_sf(data = access_comp %>% filter(ind == "acess_edu_total"), aes(fill = dif_abs), color = NA)+
  # geom_sf(data= city_shape_map, fill = NA)+
  scale_fill_distiller(palette = "PuBu", direction = 1
                       # limits = c(-1,1)*limits_ind$dif_rel_tc
                       # breaks = c(-30000, 0, 30000),
                       # labels = c("-30 mil", 0, "30 mil")
                       # , label = label_percent(accuracy = 1)
  )+
  theme_void()

ggplot()+
  geom_sf(data = access_comp %>% filter(ind == "acess_saude_total"), aes(fill = dif_abs), color = NA)+
  # geom_sf(data= city_shape_map, fill = NA)+
  scale_fill_distiller(
    palette = "PuBu", direction = 1
    palette = "Spectral", direction = 1
                       # limits = c(-1,1)*limits_ind$dif_rel_tc
                       # breaks = c(-30000, 0, 30000),
                       # labels = c("-30 mil", 0, "30 mil")
                       # , label = label_percent(accuracy = 1)
  )+
  theme_void()






