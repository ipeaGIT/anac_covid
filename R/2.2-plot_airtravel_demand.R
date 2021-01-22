source("../../git_kaue/acesso_oport/R/fun/setup.R")
library(scales)



# 1 - PLOT ALL PASSANGERS ---------------------------------------------------------------------


# open data
data_cols <- read_rds("../../data/anac_covid/combinada_cols.rds")

# make sure we only have 2019 and 2020
data_cols <- data_cols[year %in% c(2017, 2018, 2019, 2020)]

# sum number of passengers by origin and destination by day
t <- data_cols[ds_cotran %in% c("DESEMBARQUE", "", "CONEXÃO DOMÉSTICO"),
               .(total_pass = sum(nr_passag_pagos, nr_passag_gratis, na.rm = TRUE)),
               by = .(origin, destination, date, year, month, day, day_week) ]

# classify whether international flights are inbound or outbound
t[, international := fifelse(nchar(origin)==2 & nchar(destination)>2, 'outbound',
                             fifelse(nchar(origin)>2 & nchar(destination)==2, 'inbound', 'national')) ]

# sum total of passenger by day and flight type
t <- t[, .(total_pass=sum(total_pass)), by=.(date, year, international)]
t[, xx := paste0("2020-", format(date, "%m-%d"))]
t[, xx := lubridate::as_date(xx, format="%Y-%m-%d") ]


# drop pairs with no passengers and reorder columns
t <- subset(t, total_pass > 0 )

# refactor international flights
t$international <- factor(t$international, levels=c('inbound','outbound','national'),
                          labels=c('International inbound','International outbound','National'))

# export
write_rds(t, "./outputs/impact_input_passengers.rds", compress = 'gz')




# make sure that there are no flights after june
t <- t[between(xx, lubridate::ymd("2020-01-01"), lubridate::as_date("2020-12-31"))]


# identify date of first death
m_1st_br <- lubridate::as_date('2020-03-12')
m2019 <- grid::grobTree(grid::textGrob('2019', x=unit(0.9, "npc"), y=unit(0.85,"npc"), gp = grid::gpar(fontsize = 7)))


library(scales)

plot1_a <- ggplot() + 
  geom_smooth(data= subset(t,year==2019), aes(x=xx, y=total_pass), color='gray50', size=1, fill='gray70') +
  geom_point( data= subset(t,year==2020), aes(x=xx, y=total_pass), alpha=.4, size=1) +
  # geom_smooth(data= subset(t,year==2020), aes(x=xx, y=total_pass, color=international, fill=international)) +
  geom_vline(xintercept = m_1st_br, color='red', linetype="dashed") +
  facet_wrap(~international, ncol=3,  scales= "fixed") +
  
  scale_y_log10(name="Number of Passengers (log, thousands)", 
                labels = unit_format(unit = "", scale = 1e-3, accuracy = 0.1),
                limit=c(100,NA),oob=squish) +
  # scale_y_continuous(name="Number of Passengers", labels = comma, limit=c(10,NA),oob=squish) +
  guides(color=guide_legend(title="Flight"), fill=guide_legend(title="Flight")) +
  
  # annotate("text", label = "1st confirmed case", x =m_1st_br-25, y = 1500, size = 2.5, colour = "red") +
  annotation_custom(m2019) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b")+
  
  theme_minimal() +
  theme(legend.position = "bottom",
        # legend.position = c(0.8, 0.2),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        axis.title.y = element_text(size = 9))




# ggplot() + 
#   geom_smooth(data= subset(t,year==2020), aes(x=xx, y=total_pass, color=international)) +
#   geom_point( data= subset(t,year==2020), aes(x=xx, y=total_pass, color=international), alpha=.4, size=1) +
#   geom_vline(xintercept = m_1st_br, color='red', linetype="dashed")+
#   scale_y_log10(name="Number of Passengers (log)", labels = comma, limit=c(100,NA),oob=squish)


ggsave('./figures/fig1-daily_passengers_log.png', dpi=300, width = 16, units = 'cm')
ggsave('./figures/fig1-daily_passengers_log.pdf', dpi=300, width = 16, units = 'cm')


# percent drop
t_percent_drop <- t %>%
  filter(year %in% c(2019, 2020)) %>%
  mutate(month = month(date)) %>%
  group_by(year, month, international) %>%
  summarise(sum_pass = sum(total_pass)) %>%
  ungroup() %>%
  # only april
  filter(month == 4) %>%
  mutate(ano_mes = paste0("que_", year, "_", month)) %>%
  select(-year, -month) %>%
  tidyr::spread(ano_mes, sum_pass) %>%
  mutate(drop = que_2020_4/que_2019_4) %>%
  mutate(drop_perc = (que_2020_4 - que_2019_4)/que_2019_4)

# percent drop
t_percent_drop <- t %>%
  filter(year %in% c(2019, 2020)) %>%
  mutate(month = month(date)) %>%
  group_by(year, month, international) %>%
  summarise(sum_pass = sum(total_pass)) %>%
  ungroup() %>%
  # only april
  filter(month == 7) %>%
  mutate(ano_mes = paste0("que_", year, "_", month)) %>%
  select(-year, -month) %>%
  tidyr::spread(ano_mes, sum_pass) %>%
  mutate(drop = que_2020_7/que_2019_7) %>%
  mutate(drop_perc = (que_2020_7 - que_2019_7)/que_2019_7)





# 2 - PLOT PASSENGERS FROM TOP COUNTRIES ------------------------------------------------------



# identify top countries

topcovid <- c('ITÁLIA', 'ESTADOS UNIDOS DA AMÉRICA', 
              'PORTUGAL', 'ALEMANHA', 
              'FRANÇA', 'ESPANHA', 'CHINA'
              # 'EMIRADOS ÁRABES UNIDOS'
)

america_sul <- c('ARGENTINA',
                 'URUGUAI',
                 'PARAGUAI',
                 'EQUADOR',
                 'COLÔMBIA',
                 'VENEZUELA',
                 "PERU",
                 'BOLÍVIA',
                 'CHILE'
                 )

europa <- c('POLÔNIA', 'GRÉCIA', 'IRLANDA', 'TURQUIA', 'LUXEMBURGO',
            'REINO UNIDO', 
            'SUÍÇA',
            'HOLANDA',
            'ALBÂNIA',
            'ANDORRA',
            'ÁUSTRIA',
            'BÉLGICA',
            'BIELORRÚSSIA',
            'BÓSNIA',
            'BULGÁRIA',
            'CROÁCIA',
            'DINAMARCA',
            'ESLOVÁQUIA',
            'ESLOVÊNIA',
            'ESTÔNIA',
            'FINLÂNDIA',
            'HUNGRIA',
            'ISLÂNDIA',
            'LETÔNIA',
            'LIECHTENSTEIN',
            'LITUÂNIA',
            'MALTA',
            'MOLDÁVIA',
            'MÔNACO',
            'MONTENEGRO',
            'NORUEGA',
            'ROMÊNIA',
            'RÚSSIA',
            'SANMARINO',
            'SÉRVIA',
            'SUÉCIA',
            'UCRÂNIA')


asia <- c("COREIA DO SUL")


# sum number of passangers
# flight has to go or come from brazil
odmatrix_country <- data_cols[nm_pais_origem == 'BRASIL' | nm_pais_destino == "BRASIL"]
odmatrix_country <- odmatrix_country[ds_cotran %in% c("DESEMBARQUE", "", "CONEXÃO DOMÉSTICO"),
                              .(total_pass = sum(nr_passag_pagos, nr_passag_gratis, na.rm = TRUE)),
                              by = .(origin, destination, date, year, month, day, day_week) ]


# classify whether international flights are inbout or outbound
odmatrix_country[, international := fifelse(nchar(origin)==2 & nchar(destination)>2, 'outbound',
                                            fifelse(nchar(origin)>2 & nchar(destination)==2, 'inbound', 'national')) ]

# drop pairs with no passengers and reorder columns
odmatrix_country <- subset(odmatrix_country, total_pass >0 )
odmatrix_country <- odmatrix_country[order(international, origin, date, month, day)]

# filter only flights from the selected countries
t2 <- data.table::copy(odmatrix_country)
t2 <- t2[ origin %in% c(topcovid, america_sul, europa)]

# refactor origin
t2$origin <- factor(t2$origin, 
                    levels= c('ITÁLIA', 'ESTADOS UNIDOS DA AMÉRICA', 
                              'PORTUGAL', 'ALEMANHA', 
                              'FRANÇA', 'ESPANHA', 'CHINA',
                              america_sul,
                              europa
                    ),
                    labels=c('Italy', 'USA', 'Portugal', 'Germany', 
                             'France', 'Spain', 'China',
                             rep("South America", length(america_sul)),
                             rep("Rest of Europe", length(europa))))


# sum passengers by origin and date
t2 <- t2[, .(total_pass=sum(total_pass)), by=.(origin, date, year, international)]
t2[, xx := paste0("2020-", format(date, "%m-%d"))]
t2[, xx := lubridate::as_date(xx, format="%Y-%m-%d") ]


# calculate percent drop
t_percent_drop_countries <- t2 %>%
  filter(year %in% c(2019, 2020)) %>%
  mutate(month = month(date)) %>%
  group_by(year, month, international, origin) %>%
  summarise(sum_pass = sum(total_pass)) %>%
  ungroup() %>%
  # only april
  filter(month %in% c(4, 7)) %>%
  mutate(ano_mes = paste0("que_", year, "_", month)) %>%
  select(-year, -month) %>%
  tidyr::spread(ano_mes, sum_pass) %>%
  mutate(drop_4 = que_2020_4/que_2019_4) %>%
  mutate(drop_7 = que_2020_7/que_2019_7) %>%
  mutate(drop_perc_4 = (que_2020_4 - que_2019_4)/que_2019_4) %>%
  mutate(drop_perc_7 = (que_2020_7 - que_2019_7)/que_2019_7) %>%
  mutate(drop_perc_rel = drop_perc_7 - drop_perc_4)

# a1 <- t2 %>% group_by(origin) %>% summarise(sum = sum(total_pass))
# a2 <- t2 %>% group_by(origin, year) %>% summarise(sum = sum(total_pass))

# t2$international <- factor(t2$international, levels=c('inbound','outbound','national'),
#                           labels=c('International inbound','International outbound','National'))


# identify date of the first confirmed case
m_1st_br <- lubridate::as_date('2020-03-12')
m2019 <- grid::grobTree(grid::textGrob('2019', x=unit(0.87, "npc"), y=unit(.95,"npc"), gp = grid::gpar(fontsize = 6)))



plot1_b <- ggplot() + 
  geom_smooth(data= subset(t2,year==2019), aes(x=xx, y=total_pass), color='gray50', size=1, fill='gray70') +
  geom_point( data= subset(t2,year==2020), aes(x=xx, y=total_pass), alpha=.3, size=1) +
  geom_smooth(data= subset(t2,year==2020), aes(x=xx, y=total_pass)) +
  geom_vline(xintercept = m_1st_br, color='red', linetype="dashed") +
  facet_wrap(~origin, ncol=3,  scales= "free_y") +
  
  scale_y_log10(name="Number of Passengers (log)", labels = comma, limit=c(10,NA),oob=squish) +
  # scale_y_continuous(name="Number of Passengers", labels = comma, limit=c(10,NA),oob=squish) +
  guides(color=guide_legend(title="Flight"), fill=guide_legend(title="Flight")) +
  
  # annotate("text", label = "1st confirmed case", x =m_1st_br-7, y = 15, size = 2.5, colour = "red") +
  annotation_custom(m2019) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b", date_minor_breaks = "1 month")+
  
  theme_minimal() +
  theme( #legend.position = c(0.8, 0.2),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    axis.title.y = element_text(size = 9))


# join plots
library(patchwork)
plot1 <- plot1_a + theme(plot.margin = unit(c(0, 0, 30, 0), "pt"))+ 
  plot1_b + plot_layout(nrow = 2, heights = c(1, 2)) + plot_annotation(tag_levels = "A")


ggsave('./figures/fig1-daily_passengers_log.pdf', dpi=300, width = 16, units = 'cm')
ggsave('./figures/fig1-daily_passengers_log.png', dpi=300, width = 16, height=15, units = 'cm')

# save data
fwrite(t2, 'data/output/daily_passangers_topcountries_202004.csv')







# 3 - MAP BEFORE AND AFTER --------------------------------------------------------------------


# open data from the brazilian od
odmatrix_passdist_filter_agreg <- fread("../../data/anac_covid/air_odmatrix_filter_agreg.csv")

# brazil map
brazil_sf <- geobr::read_country()
states_sf <- geobr::read_state()

# dark theme
ggplot() + 
  geom_sf(data= sf::st_transform(states_sf, crs = 4326), fill="gray0", color = "grey10") +
  geom_sf(data= sf::st_transform(brazil_sf, crs = 4326), fill=NA, color ="grey50") +
  # geom_point(data = top_20_od, aes(x = lon_from, y = lat_from)) +
  geom_curve(data = odmatrix_passdist_filter_agreg, 
             aes(x = lon_from, y = lat_from, xend = lon_to, yend = lat_to, 
                 color = log10(total_pass), lwd = total_pass), alpha = 0.3,
             curvature = -0.1, arrow = arrow(length = unit(0.01, "npc"))) +
  # scale_alpha_continuous(range = c(0.1, 0.8))+
  # scale_color_gradient(low = "blue", high = "white")+
  # scale_colour_viridis_c(option = "plasma", name="Passenger x Km", direction = 1)+
  # scale_color_distiller(palette = 'Reds', direction = 1)+
  
  scale_color_distiller(palette ='Blues', breaks = scales::extended_breaks(n = 6))+
  scale_size_continuous(range = c(0.1, 3), breaks = scales::extended_breaks(n = 6))+
  coord_sf()+
  facet_wrap(~period)+
  theme_void()+
  labs(size = "Average Daily Passenger Flow (log10)")+
  theme(panel.background = element_rect(fill = "black", colour = "black"),
        legend.position = "bottom",
        legend.box = 'vertical',
        legend.key.width = unit(3, "lines"),
        legend.spacing.x = unit(1, "lines"),
        strip.text.x = element_text(size = 12, vjust = 1),
        legend.margin = margin(0, 0, 0, 0, unit = "cm")
        # legend.key.size = unit(2, "cm"),
        # legend.key.width = unit(1, "cm")
        # legend.background = element_rect(colour = "black", fill = "grey85", size = 0.5, linetype = 'dashed')
  )+
  guides(size = guide_legend(title.position = "top", title.hjust = 0.5, keywidth = 2, keyheight = 0.1,
                             nrow = 1, order = 1, label.position = 'bottom', label = FALSE),
         colour = guide_colourbar(title.position = "top", title.hjust = 0.5, keywidth = 2, 
                                  order = 2, title = NULL))


ggsave('./figures/3-dark_map.png', dpi=300, width = 16, units = 'cm')
ggsave('./figures/3-dark_map.pdf', width = 16, height = 12, units = 'cm')


# aa <- subset(odmatrix_passdist_filter_agreg, total_pass >500) 

## light theme
fi2_map <- ggplot() + 
  geom_sf(data= sf::st_transform(states_sf, crs = 4326), fill="gray90", color = "white") +
  geom_sf(data= sf::st_transform(brazil_sf, crs = 4326), fill=NA, color ="grey50") +
  # geom_point(data = top_20_od, aes(x = lon_from, y = lat_from)) +
  geom_curve(data = odmatrix_passdist_filter_agreg,
             aes(x = lon_from, y = lat_from, xend = lon_to, yend = lat_to,
                 color = log10(total_pass), lwd = total_pass) , alpha = 0.3,
             curvature = -0.1, arrow = arrow(length = unit(0.01, "npc")), show.legend = TRUE) +
  scale_size_continuous(range = c(0.1, 3), breaks = scales::extended_breaks(n = 6))+
  facet_wrap(~period)+
  # scale_alpha_continuous(range = c(0.1, 0.8))+
  # scale_color_gradient(low = "blue", high = "white")+
  # scale_colour_viridis_c(option = "plasma", name="Passenger x Km", direction = 1)+
  # scale_color_distiller(palette = 'Reds', direction = 1)+
  coord_sf()+
  theme_void() +
  labs(size = "Average Daily Passenger Flow (log10)")+
  scale_fill_continuous(guide = "colorbar", breaks = scales::extended_breaks(n = 6)) + 
  # scale_color_distiller(palette ='Blues', breaks = scales::extended_breaks(n = 6))+
  theme(legend.position="bottom",
        strip.text.x = element_text(size = 12, vjust = 1),
        legend.box = 'vertical',
        legend.key.height = unit(0.7, "lines"),
        legend.key.width = unit(3, "lines"),
        legend.spacing.x = unit(1, "lines"),
        legend.margin = margin(0, 0, 0, 0, unit = "cm"))+
  guides(size = guide_legend(title.position = "top", title.hjust = 0.5, keywidth = 2, keyheight = 0.1,
                             nrow = 1, order = 1, label.position = 'bottom', label = FALSE),
         colour = guide_colourbar(title.position = "top", title.hjust = 0.5, keywidth = 2, keyheight = 0.005,
                                  order = 2, title = NULL))



ggsave('./figures/3-light_map.png', dpi=300, width = 16, units = 'cm')
ggsave('./figures/3-light_map.pdf', dpi=300, width = 16, height = 12, units = 'cm')






# 4 - PLOT BEFORE AND AFTER AVERAGE PASSENGERS ------------------------------------------------



# open data
data_brazil_dists_first <- fread("../../data/anac_covid/flights_passengers_complete.csv")


# caculate indicators by date
odmatrix_passdist_filter_agreg_dia <- data_brazil_dists_first[between(date, as.Date("2020-02-01"), as.Date("2020-04-30")),
                                                              .(total_passdist = sum(pass_dist, na.rm = TRUE),
                                                                total_pass = sum(nr_passag_pagos, nr_passag_gratis, na.rm=T),
                                                                total_dist = sum(distance, na.rm = TRUE),
                                                                total_passdist_weighted = weighted.mean(distance, w = nr_passag_pagos+nr_passag_gratis, na.rm=T),
                                                                lon_from = lon_from[1],
                                                                lat_from = lat_from[1],
                                                                lon_to = lon_to[1],
                                                                lat_to = lat_to[1]
                                                              ),
                                                              by = .( 
                                                                date, year, month, day, day_week) ]


# CHART mean_km_by_passengers

fig2_graph_b <- ggplot()+
  geom_line(data = odmatrix_passdist_filter_agreg_dia,
            aes(x = date, y = total_passdist_weighted), color='#008080', size = .8, show.legend = FALSE) +
  # geom_vline(data = bb, color='gray60', size=1.8, alpha=.4, aes(xintercept = as.Date(as.Date("2020-03-15"))))+
  scale_y_log10(labels = comma  ) +
  scale_x_date(date_labels = "%d %b", breaks = c(as.Date("2020-01-01"),
                                                 as.Date("2020-02-01"),
                                                 as.Date("2020-03-01"),
                                                 as.Date("2020-03-15"),
                                                 as.Date("2020-04-01"),
                                                 as.Date("2020-04-30")))+
  labs(y='Daily average of Kilometers\n per passenger (log)', x='') +
  hrbrthemes::theme_ipsum(plot_margin = margin(1, 1, 1, 1),
                          axis_text_size = 7)+
  
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.ticks.y = element_line(colour = "black"))

ggsave('./figures/avg_kilometers_per_passenger.pdf', dpi=300, width = 16, units = 'cm')
ggsave('./figures/avg_kilometers_per_passenger.png', dpi=300, width = 16, units = 'cm')


# calculate FOLD CHANGE
odmatrix_passdist_filter <- fread("../../data/anac_covid/air_odmatrix_filter.csv")
a <- odmatrix_passdist_filter[, .(sum_pass = sum(total_pass, na.rm = TRUE),
                                  sum_flights = sum(n_flights, na.rm = TRUE)
), 
by = .(name_muni_uf_from, name_muni_uf_to, period, dist_pair)]


a <- a %>% 
  # categorize
  mutate(dist_cat = cut(dist_pair, breaks = c(0, 1000, 2000, Inf), 
                        labels = c("0 - 1000", "1000 - 2000", "2000+"),
                        dig.lab = 5)) %>%
  # delete same origin adn destination
  filter(!(name_muni_uf_from == name_muni_uf_to))

a2 <- a %>%
  mutate(period = case_when(period == "01Fev-15Mar" ~ "antes",
                            period == "16Mar-30Apri" ~ "depois")) %>%
  tidyr::pivot_wider(names_from = period, 
                     values_from = c(sum_pass, sum_flights), values_fill = 0) %>%
  group_by(dist_cat) %>%
  summarise(sum_pass_antes = sum(sum_pass_antes, na.rm = TRUE),
            sum_pass_depois = sum(sum_pass_depois, na.rm = TRUE),
            sum_flights_antes = sum(sum_flights_antes, na.rm = TRUE),
            sum_flights_depois = sum(sum_flights_depois, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(fold_change_pass1 = sum_pass_depois/sum_pass_antes) %>%
  mutate(fold_change_flights1 = sum_flights_depois/sum_flights_antes) %>%
  mutate(fold_change_pass2 = sum_pass_antes/sum_pass_depois) %>%
  mutate(fold_change_flights2 = sum_flights_antes/sum_flights_depois) %>%
  tidyr::pivot_longer(sum_pass_antes:fold_change_flights2, names_to = "tipo", values_to = "value") %>%
  filter(stringr::str_detect(tipo, "fold"))


plot3_a <- a2 %>%
  filter(stringr::str_detect(tipo, "1")) %>%
  mutate(tipo = factor(tipo, levels = c("fold_change_pass1", "fold_change_flights1"),
                       labels = c("Passengers", "Flights"))) %>%
  ggplot()+
  geom_col(aes(dist_cat, y = 1/value, fill = tipo), position = "dodge")+
  scale_fill_brewer(palette = "Dark2")+
  labs(fill = "Type Fold Change",
       x = "Flight distance (km)",
       y = "Fold Change (Before/After)")+
  hrbrthemes::theme_ipsum(grid = "Y", plot_margin = margin(1, 1, 1, 1),
                          axis_text_size = 7)+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7))


# assembly plots
plot3 <- plot3_a + plot3_b + plot_layout(ncol = 2) + plot_annotation(tag_levels = 'A')

ggsave('./figures/fig3-average_distance.png', dpi=300, width = 16, height = 10, units = 'cm')
