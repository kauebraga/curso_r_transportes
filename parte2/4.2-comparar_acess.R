


# abrir city limits
city_shape <- geobr::read_municipality(city_code)


# abrir access
access <- read_rds(sprintf("../../data/avaliacao_intervencoes/%s/output_access/acess_%s_%s.rds", sigla_muni, sigla_muni, modo_acesso))


# compare each indicator
access_comp <- setDT(access)
access_comp <- access_comp[!is.na(quintil)]
access_comp <- access_comp %>% dplyr::select(city, origin, pop_total, quintil, decil, tipo, geometry, CMATT30:CMAEM120) %>%
  gather("ind", "valor", CMATT30:CMAEM120) %>%
  spread(tipo, valor) %>%
  # calculate abs diffs
  mutate(dif_abs = depois - antes,
         dif_rel = log(depois/antes)) %>%
  # dif_rel = round((depois-antes)/antes, 2)) %>%
  st_sf(crs = 4326)




go <- access_comp %>%
  filter(quintil != 0) %>%
  filter(ind == variaveiss)


ggplot()+
  # geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
  # coord_equal() +
  # scale_fill_identity()+
  # # nova escala
  # new_scale_fill() +
  geom_sf(data = go_map, aes(fill = {{var}}), color = NA)+
  geom_sf(data = linhas_shape_map, size = 0.5, alpha = 0.5)+
  # geom_sf(data = linhas_shape, aes(color = route_long_name1), size = 0.5, alpha = 0.7)+
  geom_sf(data= city_shape_map, fill = NA)+
  theme_mapa()




# map with access antes
map1 <- create_map_acess(antes) +
  scale_fill_viridis_c(labels = labelss, option = "inferno"
  )+
  labs(title = "Acessibilidade TP ***antes***",
       # subtitle = variaveiss,
       fill = "")


# map with acess depois
map2 <- create_map_acess(depois)+
  scale_fill_viridis_c(labels = labelss, option = "inferno")+
  labs(title = "Acessibilidade TP ***depois***",
       # subtitle = variaveiss,
       fill = "")

map3 <- create_map_acess(dif_abs)+
  scale_fill_distiller(palette = "RdBu", direction = 1, labels = labelss,
                       limits = c(-1,1)*limits_ind$dif_abs
                       # breaks = c(-30000, 0, 30000),
                       # labels = c("-30 mil", 0, "30 mil")
  )+
  labs(title = "Diferença ***absoluta***",
       # subtitle = variaveiss,
       fill = "")




# boxplot(go$dif_abs)
# boxplot(go$dif_rel)
# mapview(go, zcol = "dif_abs")

map4 <- create_map_acess(dif_rel)+
  scale_fill_distiller(palette = "PuBu", direction = 1
                       # limits = c(-1,1)*limits_ind$dif_rel_tc
                       # breaks = c(-30000, 0, 30000),
                       # labels = c("-30 mil", 0, "30 mil")
                       , label = label_percent(accuracy = 1)
  )+
  labs(title = "Diferença ***relativa***",
       # subtitle = variaveiss,
       fill = "")

# boxplot(go_map$dif_abs)


# arrange plot
plot_conditions <- 
  # map2 + map3 + map4 &
  # (map1 | map2) / (map3 |map4) +
  (map1 | map2) +
  # plot_layout(nrow = 2) & 
  # plot_layout(nrow = 2, heights = c(3, 1)) & 
  plot_layout(heights = c(1, 1)) &
  theme_mapa() +
  theme(plot.title = element_markdown(size = 9),
        plot.subtitle = element_text(size = 6),
        legend.text = element_text(size = 7),
        legend.key.width = unit(0.8, "cm"))

# out
filename <- sprintf("figures/%s/access_conditions/map1_conditions_%s_%s_%s", sigla_muni, sigla_muni, modo_acesso, variaveiss)
ggsave(plot = plot_conditions, filename = paste0(filename, ".png"),
       height = 10, width = 16,
       # height = 14, width = 16,
       units = "cm", device = "png", dpi = 300)

plot_comparison <- 
  # map2 + map3 + map4 &
  # (map1 | map2) / (map3 |map4) +
  (map3 | map4) +
  # plot_layout(nrow = 2) & 
  # plot_layout(nrow = 2, heights = c(3, 1)) & 
  plot_layout(heights = c(1, 1)) &
  theme_mapa() +
  theme(plot.title = element_markdown(size = 9),
        plot.subtitle = element_text(size = 6),
        legend.text = element_text(size = 7),
        legend.key.width = unit(0.8, "cm"))

# out
filename <- sprintf("figures/%s/access_comparison/map2_comparisonn_%s_%s_%s", sigla_muni, sigla_muni, modo_acesso, variaveiss)
ggsave(plot = plot_comparison, filename = paste0(filename, ".png"),
       height = 10, width = 16,
       # height = 14, width = 16,
       units = "cm", device = "png", dpi = 300)




# plot access inequalities --------------------------------------------------------------

# 1) access inequalities boxplot 1
go_long <- go %>%
  st_set_geometry(NULL) %>%
  pivot_longer(cols = dif_abs:dif_rel,
               names_to = "tipo_indicador",
               values_to = "valor_indicador"
  )
# 
# # change label
# go_long <- go_long %>%
#   mutate(valor_indicador1 = ifelse(tipo_indicador == "dif_rel", 
#                                    paste0(valor_indicador * 100, " %"), valor_indicador))

# calcular palma ratio
acess_palma <- go_long %>%
  dplyr::select(city, decil, pop_total, tipo_indicador, valor_indicador) %>%
  # pegar so decis 4 e 9
  filter(decil %in% c(1, 2, 3, 4, 10)) %>%
  # definir ricos e pobres
  mutate(classe = ifelse(decil %in% c(1, 2, 3, 4), "pobre", "rico")) %>%
  group_by(city, classe, tipo_indicador) %>%
  summarise(acess_media = weighted.mean(valor_indicador, pop_total)) %>%
  ungroup() %>%
  spread(classe, acess_media) %>%
  # calcular palma ratio
  group_by(city, tipo_indicador) %>%
  mutate(palma_ratio = rico/pobre) %>%
  ungroup()

# definir valor pra truncar eixo y
valor_trunc_abs <- ifelse(variaveiss == "CMATT60", 20000, 10)
valor_trunc_rel <- ifelse(variaveiss == "CMATT60", 0.1, 0.1)


boxplot_inequalities11 <- ggplot()+
  geom_boxplot(data = go, 
               aes(x = as.factor(decil), y = dif_abs, weight = pop_total, color = as.factor(decil)), 
               outlier.size = 1.5, outlier.alpha = 0.5, outlier.colour=rgb(.5,.5,.5, alpha=0.05),
               show.legend = FALSE) +
  # coord_flip()+
  # facet_wrap(~tipo_indicador, scale = "free_y", nrow = 2)+
  # scale_y_continuous(label = percent)+
  # scale_y_continuous(labels = function(x) ifelse(x <= 2, paste0(x * 10, "%"), x))+
  scale_colour_brewer(palette = "RdBu", labels=c('D1 Pobres', paste0('D', c(2:9)), 'D10 ricos'), name='Decil de renda') +
  theme_ipsum_rc(grid = "X", base_family = 'Helvetica')+
  guides(color=guide_legend(nrow=1)) +
  labs(y = "Ganho absoluto de acess.", x = "",
       subtitle = sprintf("**Razão de Palma**: %s", round(subset(acess_palma, tipo_indicador == "dif_abs")$palma_ratio, 2)))+
  coord_cartesian(ylim = c(NA, valor_trunc_abs)) +
  theme( 
    panel.grid.minor = element_blank()
    ,strip.text = element_markdown(size = 7, face ="bold")
    ,legend.text = element_text(size = 5)
    , legend.position = "bottom"
    , axis.text.x = element_blank()
    , axis.text.y = element_text(size = 6),
    axis.title.x = element_text(size = 7),
    axis.title.y = element_text(size = 7, face="bold"),
    legend.title = element_text(size = 7)
    
  )

boxplot_inequalities12 <- ggplot()+
  geom_boxplot(data = go, 
               aes(x = as.factor(decil), y = dif_rel, weight = pop_total, color = as.factor(decil)), 
               outlier.size = 1.5, outlier.alpha = 0.5, outlier.colour=rgb(.5,.5,.5, alpha=0.05)) +
  # coord_flip()+
  # facet_wrap(~tipo_indicador, scale = "free_y", nrow = 2)+
  # scale_y_continuous(label = percent)+
  scale_y_continuous(labels = percent)+
  scale_colour_brewer(palette = "RdBu", labels=c('D1 Pobres', paste0('D', c(2:9)), 'D10 ricos'), name='Decil de renda') +
  theme_ipsum_rc(grid = "X", base_family = 'Helvetica')+
  guides(color=guide_legend(nrow=1)) +
  labs(y = "Ganho relativo de acess.", x = "",
       subtitle = sprintf("**Razão de palma**: %s", round(subset(acess_palma, tipo_indicador == "dif_rel")$palma_ratio, 2)))+
  coord_cartesian(ylim = c(NA, valor_trunc_rel)) +
  theme( 
    panel.grid.minor = element_blank()
    ,strip.text = element_markdown(size = 5, face ="bold")
    ,legend.text = element_text(size = 5)
    , legend.position = "bottom"
    , axis.text.x = element_blank()
    , axis.text.y = element_text(size = 6),
    axis.title.x = element_text(size = 7),
    axis.title.y = element_text(size = 7, face="bold"),
    legend.title = element_text(size = 7)
    
  )

# juntar

boxplot_inequalities1 <- 
  # map2 + map3 + map4 &
  # (map1 | map2) / (map3 |map4) +
  (boxplot_inequalities11 / boxplot_inequalities12) +
  # plot_layout(nrow = 2) & 
  # plot_layout(nrow = 2, heights = c(3, 1)) & 
  plot_layout(heights = c(1, 1)) &
  theme(plot.title = element_markdown(size = 7),
        plot.subtitle = element_markdown(size = 8),
        legend.text = element_text(size = 6),
        plot.margin=unit(c(0,0,0,0),"mm"))


ggsave(plot = boxplot_inequalities1, 
       filename = sprintf("figures/%s/access_inequalities/fig1_ineq1_%s_%s_%s.png", 
                          sigla_muni, sigla_muni, modo_acesso, variaveiss), 
       height = 10, width = 16, units = "cm", device = "png")



