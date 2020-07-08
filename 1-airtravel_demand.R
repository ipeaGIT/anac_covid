library(dplyr)
library(data.table)
library(ggplot2)
library(scales)
library(magrittr)
library(gghighlight)
library(viridis)

options(scipen=999)

# # TESTE!!!!!!!!! ------------------------------------------------------------------------------
# 
# combinada <- fread("combinada2020-01.txt") %>% select(id_combinada, 
#                                                  nr_voo, sg_empresa_icao,
#                                                  hr_partida_real, dt_partida_real, sg_icao_origem,
#                                                  nm_municipio_origem, sg_uf_origem, nm_pais_origem, 
#                                                  hr_chegada_real, dt_chegada_real,
#                                                  sg_icao_destino, nm_municipio_destino, sg_uf_destino, nm_pais_destino,
#                                                  nr_escala_destino,
#                                                  ds_cotran, nr_passag_pagos, nr_passag_gratis)
# basica <- fread("basica2020-01.txt") %>% select(id_basica, 
#                                                 nr_voo, sg_empresa_icao,
#                                                 hr_partida_real, dt_partida_real, sg_icao_origem,
#                                                 nm_municipio_origem, sg_uf_origem, nm_pais_origem, 
#                                                 hr_chegada_real, dt_chegada_real,
#                                                 sg_icao_destino, nm_municipio_destino, sg_uf_destino, nm_pais_destino,
#                                                 nr_escala_destino,
#                                                 nr_passag_pagos, nr_passag_gratis)
# 
# setdiff(data1, data)
# 
# combinada <- combinada %>% mutate(id = paste0(sg_empresa_icao, "-", nr_voo))
# basica <- basica %>% mutate(id = paste0(sg_empresa_icao, "-", nr_voo))
# 
# # sem diferenças de viagens
# setdiff(combinada$id, basica$id) # 0
# setdiff(basica$id, combinada$id) # 0
# 
# # quantidade de obs por id
# combinada1 <- combinada %>% count(id)
# basica1 <- basica %>% count(id)
# 
# ui <- left_join(combinada1, basica1, by = "id", suffix = c(".combinada", ".basica")) %>% mutate(dif = n.combinada - n.basica)
# 
# # pegar exemplo
# combinada_ex <- combinada %>% filter(id == "TAM-3773") %>% arrange(dt_partida_real, hr_partida_real)
# basica_ex <- basica %>% filter(id == "TAM-3773") %>% arrange(dt_partida_real, hr_partida_real)
# 
# combinada_ex <- combinada %>% filter(id == "TAM-8011") %>% arrange(dt_partida_real, hr_partida_real)
# basica_ex <- basica %>% filter(id == "TAM-8011") %>% arrange(dt_partida_real, hr_partida_real)
# 



# usar combinada! ----------------------------------------------

files <- list.files(path = 'data-raw/', pattern = '-01|-02|-03|-04', full.names = T)
combinada <- lapply(files, fread) %>% rbindlist()

data_cols <- combinada %>% select(id_combinada, 
                                  nr_voo, sg_empresa_icao,
                                  hr_partida_real, dt_partida_real, sg_icao_origem,
                                  nm_municipio_origem, sg_uf_origem, nm_pais_origem, 
                                  hr_chegada_real, dt_chegada_real,
                                  sg_icao_destino, nm_municipio_destino, sg_uf_destino, nm_pais_destino,
                                  nr_escala_destino,
                                  ds_cotran, nr_passag_pagos, nr_passag_gratis)

# set locale to english to convert weeknames
Sys.setlocale("LC_ALL","English")

gc(reset = T)
# 
# 
# # matriz
# data_group <- data_cols %>%
#   # somente destinos dentro do brasil
#   filter(sg_uf_destino != "") %>%
#   # identificar se uma viagem eh internacional ou nao
#   mutate(viagem_origem = ifelse(sg_uf_origem == "", "int", "nac")) %>%
#   # se a origem for internacional, identificar o pais
#   mutate(sg_uf_origem = ifelse(sg_uf_origem == "", nm_pais_origem, sg_uf_origem)) %>%
#   # somente desembarques
#   filter(ds_cotran == "DESEMBARQUE") %>%
#   # matriz
#   group_by(dt_chegada_real, sg_uf_origem, sg_uf_destino) %>%
#   summarise(total_pass = sum(nr_passag_pagos, nr_passag_gratis, na.rm = TRUE),
#             data = first(dt_chegada_real),
#             international = first(viagem_origem)
#             ) %>%
#   # calcular dia do mes, da semana, mes
#   mutate(data = lubridate::as_date(data)) %>%
#   mutate(month = lubridate::month(data),
#          day = lubridate::day(data),
#          day_week = lubridate::wday(data, label = TRUE)) %>%
#   # organize columns
#   select(origin = sg_uf_origem, dest = sg_uf_destino, international, 
#          month, day, day_week, total_pass) %>%
#   # tirar zeros
#   filter(total_pass != 0)
# 
# head(data_group)
# 
# # output
# readr::write_csv(data_group, "data/output/air-travel_od_2020.csv")




######################################### dt ------------------------------
# rename columns
setnames(data_cols, 'sg_uf_origem', 'origin')
setnames(data_cols, 'sg_uf_destino', 'destination')

# trazer paises para coluna de estados
data_cols[, origin := fifelse(origin=="", nm_pais_origem, origin)]
data_cols[, destination := fifelse(destination=="", nm_pais_destino, destination)]

# criar coluna de datas
data_cols[, date := lubridate::as_date(dt_chegada_real)]
data_cols[, year := lubridate::year(date)]
data_cols[, month := lubridate::month(date)]
data_cols[, day := lubridate::day(date)]
data_cols[, day_week := lubridate::wday(date, label = TRUE)]
          

# sum number of passangers
odmatrix_uf <- data_cols[ds_cotran %in% c("DESEMBARQUE", ""),
                    .(total_pass = sum(nr_passag_pagos, nr_passag_gratis, na.rm = TRUE)),
                       by = .(origin, destination, date, year, month, day, day_week) ]


# classify whether international flights are inbout or outbound
odmatrix_uf[, international := fifelse(nchar(origin)==2 & nchar(destination)>2, 'outbound',
                             fifelse(nchar(origin)>2 & nchar(destination)==2, 'inbound', 'national')) ]
  
# drop pairs with no passengers and reorder columns
odmatrix_uf <- subset(odmatrix_uf, total_pass >0 )
odmatrix_uf <- odmatrix_uf[order(international, origin, date, month, day)]

head(odmatrix_uf)
table(odmatrix_uf$year)


fwrite(odmatrix_uf, "data/output/air-travel_od_202004.csv")






############# plot all passengers  ------------------------------

# sum number of passangers
t <- data_cols[ds_cotran %in% c("DESEMBARQUE", "", "CONEXÃO DOMÉSTICO"),
                         .(total_pass = sum(nr_passag_pagos, nr_passag_gratis, na.rm = TRUE)),
                         by = .(origin, destination, date, year, month, day, day_week) ]

# classify whether international flights are inbout or outbound
t[, international := fifelse(nchar(origin)==2 & nchar(destination)>2, 'outbound',
                                       fifelse(nchar(origin)>2 & nchar(destination)==2, 'inbound', 'national')) ]

t <- t[, .(total_pass=sum(total_pass)), by=.(date, year, international)]
t[, xx := format(date, "%m-%d")]
t[, xx := lubridate::as_date(xx, format="%m-%d") ]

# drop pairs with no passengers and reorder columns
t <- subset(t, total_pass >0 )


t$international <- factor(t$international, levels=c('inbound','outbound','national'),
               labels=c('International inbound','International outbound','National'))

# export
t_export <- t %>% filter(year == 2020) %>% select(-xx)
fwrite(t_export, "data/output/output_publish/air_totalpass_fig3.csv")



m_1st_br <- lubridate::as_date('2020-02-26')
m2019 <- grid::grobTree(grid::textGrob('2019', x=unit(0.9, "npc"), y=unit(0.85,"npc"), gp = grid::gpar(fontsize = 7)))



ggplot() + 
  geom_smooth(data= subset(t,year==2019), aes(x=xx, y=total_pass), color='gray50', size=1, fill='gray70') +
  geom_point( data= subset(t,year==2020), aes(x=xx, y=total_pass, color=international), alpha=.4, size=1) +
  # geom_smooth(data= subset(t,year==2020), aes(x=xx, y=total_pass, color=international, fill=international)) +
  geom_vline(xintercept = m_1st_br, color='red', linetype="dashed") +
  facet_wrap(~international, ncol=2,  scales= "free_y") +
  
  scale_y_log10(name="Number of Passengers (log)", labels = comma, limit=c(100,NA),oob=squish) +
 # scale_y_continuous(name="Number of Passengers", labels = comma, limit=c(10,NA),oob=squish) +
  guides(color=guide_legend(title="Flight"), fill=guide_legend(title="Flight")) +
  
  annotate("text", label = "1st confirmed case", x =m_1st_br-25, y = 1500, size = 2.5, colour = "red") +
  annotation_custom(m2019) +

  theme_minimal() +
  theme(legend.position = c(0.8, 0.2),
        axis.title.x = element_blank())




ggplot() + 
  geom_smooth(data= subset(t,year==2020), aes(x=xx, y=total_pass, color=international)) +
  geom_point( data= subset(t,year==2020), aes(x=xx, y=total_pass, color=international), alpha=.4, size=1) +
  geom_vline(xintercept = m_1st_br, color='red', linetype="dashed")+
  scale_y_log10(name="Number of Passengers (log)", labels = comma, limit=c(100,NA),oob=squish)


ggsave('./figures/daily_passengers_log.png', dpi=300, width = 16, units = 'cm')
ggsave('./figures/daily_passengers_log.pdf', dpi=300, width = 16, units = 'cm')






############# plot passengers from top countries  ------------------------------

# identify top countries
# top_countries <- subset(odmatrix, international=='inbound' & year==2020)
# # top_countries <- subset(top_countries, !(month==2 & day>25) )
# head(top_countries)
# top20 <- top_countries[, .(total_pass=sum( total_pass )), by=origin][order(-total_pass)]
# top20 <- top20[1:20,]$origin

topcovid <- c('ITÁLIA', 'ESTADOS UNIDOS DA AMÉRICA', 
              'REINO UNIDO', 'HOLANDA', 'PORTUGAL', 'ALEMANHA', 
              'SUÍÇA', 'FRANÇA', 'ESPANHA', 'CHINA',
              'EMIRADOS ÁRABES UNIDOS')

europa <- c('POLÔNIA', 'GRÉCIA', 'IRLANDA', 'BÉLGICA', 'TURQUIA', 'LUXEMBURGO',
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
              'LUXEMBURGO',
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


# x <- data_cols[ nm_pais_origem=='EMIRADOS ÁRABES UNIDOS' & nm_pais_destino=='BRASIL']
# table(x$ds_cotran)

# x[ds_cotran==""]


# sum number of passangers
odmatrix_country <- data_cols[ds_cotran %in% c("DESEMBARQUE", "", "CONEXÃO DOMÉSTICO"),
                         .(total_pass = sum(nr_passag_pagos, nr_passag_gratis, na.rm = TRUE)),
                         by = .(origin, destination, date, year, month, day, day_week) ]


# classify whether international flights are inbout or outbound
odmatrix_country[, international := fifelse(nchar(origin)==2 & nchar(destination)>2, 'outbound',
                                       fifelse(nchar(origin)>2 & nchar(destination)==2, 'inbound', 'national')) ]

# drop pairs with no passengers and reorder columns
odmatrix_country <- subset(odmatrix_country, total_pass >0 )
odmatrix_country <- odmatrix_country[order(international, origin, date, month, day)]

t2 <- odmatrix_country[ origin %in% c(topcovid, europa), .(total_pass=sum(total_pass)), by=.(origin, date, year, international)]
t2[, xx := format(date, "%m-%d")]
t2[, xx := lubridate::as_date(xx, format="%m-%d") ]



t2$origin <- factor(t2$origin, 
                    levels= c('ITÁLIA', 'ESTADOS UNIDOS DA AMÉRICA', 
                              'REINO UNIDO', 'HOLANDA', 'PORTUGAL', 'ALEMANHA', 
                              'SUÍÇA', 'FRANÇA', 'ESPANHA', 'CHINA',
                              'EMIRADOS ÁRABES UNIDOS',
                              europa),
                    labels=c('Italy', 'USA', 'UK', 'Netherlands', 'Portugal', 'Germany', 
                            'Switzerland', 'France', 'Spain', 'China','UAE',
                            rep("Rest of Europe", length(europa)))) # UAE = United Arab Emirates

# a1 <- t2 %>% group_by(origin) %>% summarise(sum = sum(total_pass))
# a2 <- t2 %>% group_by(origin, year) %>% summarise(sum = sum(total_pass))

# t2$international <- factor(t2$international, levels=c('inbound','outbound','national'),
#                           labels=c('International inbound','International outbound','National'))


m_1st_br <- lubridate::as_date('2020-02-26')
m2019 <- grid::grobTree(grid::textGrob('2019', x=unit(0.87, "npc"), y=unit(.95,"npc"), gp = grid::gpar(fontsize = 6)))



ggplot() + 
  geom_smooth(data= subset(t2,year==2019), aes(x=xx, y=total_pass), color='gray50', size=1, fill='gray70') +
  geom_point( data= subset(t2,year==2020), aes(x=xx, y=total_pass, color=origin), alpha=.3, size=1) +
  geom_smooth(data= subset(t2,year==2020), aes(x=xx, y=total_pass, color=origin, fill=origin)) +
  geom_vline(xintercept = m_1st_br, color='red', linetype="dashed") +
  facet_wrap(~origin, ncol=4,  scales= "free_y") +
  
  scale_y_log10(name="Number of Passengers (log)", labels = comma, limit=c(10,NA),oob=squish) +
  # scale_y_continuous(name="Number of Passengers", labels = comma, limit=c(10,NA),oob=squish) +
  guides(color=guide_legend(title="Flight"), fill=guide_legend(title="Flight")) +
  
  annotate("text", label = "1st confirmed case", x =m_1st_br-7, y = 15, size = 2.5, colour = "red") +
  annotation_custom(m2019) +
  
  theme_minimal() +
  theme( #legend.position = c(0.8, 0.2),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 7))




ggsave('./figures/daily_passengers_topcountries_log.pdf', dpi=300, width = 16, units = 'cm')
ggsave('./figures/daily_passengers_topcountries_log.png', dpi=300, width = 16, height=14, units = 'cm')

# save data
fwrite(t2, 'data/output/daily_passangers_topcountries_202004.csv')








# MATRIX 28 X 28 ------------------------------------------------------------------------------

od <- fread("data/output/air-travel_od_2020_04.csv")

# only national and inbound flights
# od_inbound <- od[international %in% c("inbound", "national")]
# od_inbound <- od_inbound[nchar(destination) == 2]

# confirm that everything is correct

# if internatil inbound, the origin is 'international'
od[, origin1 := ifelse(nchar(origin) > 2, "INTERNATIONAL", origin)]
od[, destination1 := ifelse(nchar(destination) > 2, "INTERNATIONAL", destination)]

# divide flights between 01/02 to 15/03 and 16/03 to 30/04
od[, date := as.Date(date)]

# filter flights after 01-02
od_inbound_filter <- od[date >= as.Date("2020-02-01")]
od_inbound_filter <- od_inbound_filter[date <= as.Date("2020-04-30")]

od_inbound_filter[, period := ifelse(between(date, as.Date("2020-02-01"), as.Date("2020-03-15")),
                                     "before", "after")]

# calculate mean of passenger before and after date for each uf and international
od_inbound_filter_agreg <- od_inbound_filter[, .(pass_mean = sum(total_pass, na.rm = TRUE)),
                                             by = .(origin1, destination1, period)]

# transform to matrix - before
od_inbound_filter_agreg_wide_before <- od_inbound_filter_agreg %>% filter(period == 'before') %>%
  tidyr::spread(destination1, pass_mean)

od_inbound_filter_agreg_mtx_before <-  as.matrix(od_inbound_filter_agreg_wide_before)

rownames(od_inbound_filter_agreg_mtx_before) <- od_inbound_filter_agreg_mtx_before[,1]
od_inbound_filter_agreg_mtx_before <- od_inbound_filter_agreg_mtx_before[,c(-1, -2)]

View(od_inbound_filter_agreg_mtx_before)

# save it
write.table(od_inbound_filter_agreg_mtx_before, 
            "data/output/airtavel-od_Fev1-Mar15_mtx.csv")

# transform to matrix - after
od_inbound_filter_agreg_wide_after <- od_inbound_filter_agreg %>% filter(period == 'after') %>%
  tidyr::spread(destination1, pass_mean)

od_inbound_filter_agreg_mtx_after <-  as.matrix(od_inbound_filter_agreg_wide_after)

rownames(od_inbound_filter_agreg_mtx_after) <- od_inbound_filter_agreg_mtx_after[,1]
od_inbound_filter_agreg_mtx_after <- od_inbound_filter_agreg_mtx_after[,c(-1, -2)]

View(od_inbound_filter_agreg_mtx_after)

# save it
write.table(od_inbound_filter_agreg_mtx_after, 
            "data/output/airtavel-od_Mar16-Abr30_mtx.csv")





ggplot()+
  geom_point(data = od_inbound_filter, aes(x = date, y = total_pass, color = period))+
  scale_x_date(date_labels = "%d/%b", breaks = c(as.Date("2020-02-01"),
                                                      as.Date("2020-03-15"),
                                                      as.Date("2020-04-30")))+
  facet_wrap(~international, ncol = 1, scales = "free")+
  theme_bw()








# TRAFFIC FLOW BETWEEN MUNICIPALITIES FIRST CASE ----------------------------------------------

files <- list.files(path = 'data-raw/', pattern = '-01|-02|-03|-04', full.names = T)
combinada <- lapply(files, fread) %>% rbindlist()

data_cols <- combinada %>% select(id_combinada, 
                                  nr_voo, sg_empresa_icao,
                                  hr_partida_real, dt_partida_real, sg_icao_origem,
                                  nm_municipio_origem, sg_uf_origem, nm_pais_origem, 
                                  hr_chegada_real, dt_chegada_real,
                                  sg_icao_destino, nm_municipio_destino, sg_uf_destino, nm_pais_destino,
                                  nr_escala_destino,
                                  ds_cotran, nr_passag_pagos, nr_passag_gratis)

# set locale to english to convert weeknames
Sys.setlocale("LC_ALL","English")

# only brazilian flights
data_brazil <- data_cols[nm_pais_origem == "BRASIL" & nm_pais_destino == "BRASIL"]

# rename columns
setnames(data_brazil, 'nm_municipio_origem', 'origin')
setnames(data_brazil, 'nm_municipio_destino', 'destination')

# format cities names
data_brazil <- data_brazil %>% 
  mutate_at(c("origin", "destination"), stringi::stri_trans_general, id = "Latin-ASCII") %>%
  mutate_at(c("origin", "destination"), iconv, to="UTF-8") %>%
  mutate_at(c("origin", "destination"), tolower)

  
# criar coluna de datas
setDT(data_brazil)[, date := lubridate::as_date(dt_chegada_real)]
data_brazil[, year := lubridate::year(date)]
data_brazil[, month := lubridate::month(date)]
data_brazil[, day := lubridate::day(date)]
data_brazil[, day_week := lubridate::wday(date, label = TRUE)]

# open dists - only first cases
dists <- fread("data/first_case_dist-df.csv") %>%
  # create combination of muni and UF
  mutate(name_muni_uf_from = paste0(name_muni_from, "-", tolower(abbrev_state_from))) %>%
  mutate(name_muni_uf_to = paste0(name_muni_to, "-", tolower(abbrev_state_to)))

# open dists - all cities
dists <- fread("data/airports_dist-df.csv")

# bring distance between origin and destination - only first cases
data_brazil_dists <- data_brazil %>%
  # create combination of muni and UF
  mutate(name_muni_uf_from = paste0(origin, "-", tolower(sg_uf_origem))) %>%
  mutate(name_muni_uf_to = paste0(destination, "-", tolower(sg_uf_destino))) %>%
  left_join(select(dists, name_muni_uf_from, name_muni_uf_to, lon_from, lat_from, lon_to, lat_to, distance), 
            by = c("name_muni_uf_from", "name_muni_uf_to"))

# filter only municipalities with first casa
data_brazil_dists_first <- data_brazil_dists %>%
  filter(name_muni_uf_from %in% dists$name_muni_uf_from, 
         name_muni_uf_to %in% dists$name_muni_uf_to)
  

# verify NAs
data_brazil_dists_first %>%
  select(name_muni_uf_from, name_muni_uf_to, distance) %>%
  filter(is.na(distance)) %>%
  View() # 0 - OK!

# filter only desembarques e "'
data_brazil_dists_first <- setDT(data_brazil_dists_first)[ds_cotran %in% c("DESEMBARQUE", "")]

# calculate number of passangers x distance of each flight
data_brazil_dists_first[, pass_dist := sum(nr_passag_pagos, nr_passag_gratis) * distance,
                        by = id_combinada]




# sum number of pass_dist by day OD pair
odmatrix_passdist <- data_brazil_dists_first[,
                                             .(total_passdist = sum(pass_dist, na.rm = TRUE),
                                               total_pass = sum(nr_passag_pagos, nr_passag_gratis, na.rm=T),
                                               total_dist = sum(distance, na.rm = TRUE),
                                               dist_pair = distance[1],
                                               n_flights = .N,
                                               lon_from = lon_from[1],
                                               lat_from = lat_from[1],
                                               lon_to = lon_to[1],
                                               lat_to = lat_to[1]
                                               ),
                                             by = .(name_muni_uf_from, name_muni_uf_to, 
                                                    date, year, month, day, day_week) ]


# filter flights after 01-02
odmatrix_passdist_filter <- odmatrix_passdist[date >= as.Date("2020-02-01")]
odmatrix_passdist_filter <- odmatrix_passdist_filter[date <= as.Date("2020-04-30")]

odmatrix_passdist_filter[, period := ifelse(between(date, as.Date("2020-02-01"), as.Date("2020-03-15")),
                                     "before", "after")]

# refactor
odmatrix_passdist_filter$period <- factor(odmatrix_passdist_filter$period,
                                                levels = c("before", "after"),
                                                labels = c("01Fev-15Mar", "16Mar-30Apri"))



# export data
fwrite(odmatrix_passdist_filter, "data/output/output_publish/air_odmatrix_filter.csv")


# calculate passxdist aggregated for before and after date
odmatrix_passdist_filter_agreg <- odmatrix_passdist_filter[, .(total_passdist = sum(total_passdist, na.rm = TRUE),
                                                               total_pass = sum(total_pass, na.rm=T),
                                                               total_dist = sum(total_dist, na.rm = TRUE),
                                                               n_flights = sum(n_flights, na.rm = TRUE),
                                                               dist_pair = dist_pair[1],
                                                               lon_from = lon_from[1],
                                                               lat_from = lat_from[1],
                                                               lon_to = lon_to[1],
                                                               lat_to = lat_to[1]
                                                               
                                                               ),
                                             by = .(name_muni_uf_from, name_muni_uf_to, period)]

# deop zero's and origin = destinatiob
odmatrix_passdist_filter_agreg <- odmatrix_passdist_filter_agreg[total_passdist != 0]
odmatrix_passdist_filter_agreg <- odmatrix_passdist_filter_agreg[name_muni_uf_from != name_muni_uf_to]

# refactor
odmatrix_passdist_filter_agreg$period <- factor(odmatrix_passdist_filter_agreg$period,
                                                levels = c("01Fev-15Mar", "16Mar-30Apri"),
                                                labels = c("01Fev - 15Mar", "16Mar - 30Apr"))

# order
odmatrix_passdist_filter_agreg <- odmatrix_passdist_filter_agreg %>% 
  arrange(period, desc(total_passdist))

# export data
fwrite(odmatrix_passdist_filter_agreg, "data/output/output_publish/air_odmatrix_filter_agreg.csv")


# MAP --------------------------------

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


ggsave('./figures/dark_map.png', dpi=300, width = 16, units = 'cm')
ggsave('./figures/dark_map.pdf', width = 16, height = 12, units = 'cm')


# aa <- subset(odmatrix_passdist_filter_agreg, total_pass >500) 

## light theme
ggplot() + 
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

  

ggsave('./figures/light_map.png', dpi=300, width = 16, units = 'cm')
ggsave('./figures/light_map.pdf', dpi=300, width = 16, height = 12, units = 'cm')


########### CHART -------------------------------------------


# vai

# sum total passenger x km by day
odmatrix_passdist_filter_agreg_dia <- odmatrix_passdist_filter[, .(total_passdist = sum(total_passdist, na.rm = TRUE),
                                                                   total_pass = sum(total_pass, na.rm=T),
                                                                   total_dist = sum(total_dist, na.rm = TRUE),
                                                                   # lon_from = lon_from[1],
                                                                   # lat_from = lat_from[1],
                                                                   # lon_to = lon_to[1],
                                                                   # lat_to = lat_to[1],
                                                                   period = period[1]),
                                                               by = .(date)]

data_brazil_dists_first1 <- data_brazil_dists_first[!is.na(nr_passag_pagos)]
data_brazil_dists_first1 <- data_brazil_dists_first1[!is.na(nr_passag_gratis)]

# data_brazil_dists_first[, nr_passag_pagos := ifelse(is.na(nr_passag_pagos), 0, nr_passag_pagos)]
# data_brazil_dists_first[, nr_passag_gratis := ifelse(is.na(nr_passag_gratis), 0, nr_passag_gratis)]

# data_brazil_dists_first1 <- data_brazil_dists_first1[!(name_muni_uf_from == "sao paulo-sp" & name_muni_uf_to == "rio de janeiro-rj")] 
# data_brazil_dists_first1 <- data_brazil_dists_first1[!(name_muni_uf_from == "rio de janeiro-rj" & name_muni_uf_to == "sao paulo-sp")] 

# sum number of pass_dist by day OD pair
# data_brazil_dists_first1[, dist_cat := cut(distance, breaks = c(0, 500, 1000, 1500, 2000, 2500, Inf), dig.lab = 5)]

odmatrix_passdist_filter_agreg_dia1 <- data_brazil_dists_first1[between(date, as.Date("2020-02-01"), as.Date("2020-04-30")),
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


ggplot()+
  # geom_line(data = odmatrix_passdist_filter_agreg_dia,
  #           aes(x = date, y = total_passdist/1000000), color='gray50', lwd = 1, show.legend = FALSE)+
  geom_line(data = odmatrix_passdist_filter_agreg_dia1,
            aes(x = date, y = total_passdist/total_pass), color='gray50', lwd = 1, show.legend = FALSE) +
  # geom_smooth(data = odmatrix_passdist_filter_agreg_dia, 
  #             aes(x = date, y = total_passdist/1000000), color='gray50', size=1, fill='gray70') +
  # geom_point(data = odmatrix_passdist_filter_agreg_dia, 
  #           aes(x = date, y = total_passdist/1000000), lwd = 1, show.legend = FALSE)+
  
  # geom_line(data = odmatrix_passdist_filter_agreg_dia, 
  #           aes(x = date, y = total_passdist/1000000, color = "period"), lwd = 1)+
  # labs(color = "Period", x = "Day", y = "Passenger kilometers (Millions)")+
  labs(color = "Period", x = "Day", y = "")+
  geom_vline(data = odmatrix_passdist_filter_agreg_dia, aes(xintercept = as.Date(as.Date("2020-03-15"))))+
  # scale_y_log10(labels = comma ) +
  scale_x_date(date_labels = "%d/%b", breaks = c(as.Date("2020-02-01"),
                                                 as.Date("2020-03-15"),
                                                 as.Date("2020-04-30")))+
  theme_minimal() +
  theme(legend.position = 'bottom')



ggsave('./figures/passenger_kilometers_flown_log.png', dpi=300, width = 16, units = 'cm')
ggsave('./figures/passenger_kilometers_flown_log.pdf', dpi=300, width = 16, units = 'cm')


# calculate indicators
data_brazil_dists_first[, total_passengers := nr_passag_pagos + nr_passag_gratis, by=id_combinada]
data_brazil_dists_first[, km_x_passengers := distance*total_passengers, by=id_combinada]
data_brazil_dists_first[, km_per_pass := distance/total_passengers, by=id_combinada]

# drop zero's and origin = destinatiob
aa <- data_brazil_dists_first[name_muni_uf_from != name_muni_uf_to]
aa <- aa[between(date, as.Date("2020-01-01"), as.Date("2020-04-30")),]
aa <- aa[ total_passengers != 0]
aa <- aa[ distance !=0]


bb <- aa[, .( total_km_x_passengers = sum(km_x_passengers, na.rm=T)
              , mean_km_by_passengers = sum(km_x_passengers) / sum(total_passengers)
              #, wmean_km_by_passengers = weighted.mean(x=km_x_passengers, w=total_passengers, na.rm=T)
             ),
         by = date]

fwrite(bb, 'data/output/output_publish/air_daily_km_x_passengers.csv')

ggplot()+
  geom_line(data = odmatrix_passdist_filter_agreg_dia1,
            aes(x = date, y = total_passdist/total_pass), color='gray50', lwd = 1, show.legend = FALSE)

ggplot()+ geom_line(data = bb, aes(x = date, y = log10(total_km_x_passengers)), color='gray50', lwd = 1, show.legend = FALSE)
ggplot()+ geom_line(data = bb, aes(x = date, y = log10(mean_km_by_passengers)), color='gray50', lwd = 1, show.legend = FALSE)
ggplot()+ geom_line(data = bb, aes(x = date, y = log10(wmean_km_by_passengers)), color='gray50', lwd = 1, show.legend = FALSE)





# bb <- fread('daily_km_x_passengers.csv')

# CHART mean_km_by_passengers

ggplot()+
  geom_line(data = bb,
            aes(x = date, y = mean_km_by_passengers), color='#008080', size = .8, show.legend = FALSE) +
  geom_vline(data = bb, color='gray60', size=1.8, alpha=.4, aes(xintercept = as.Date(as.Date("2020-03-15"))))+
  scale_y_log10(labels = comma  ) +
  scale_x_date(date_labels = "%d %b", breaks = c(as.Date("2020-01-01"),
                                                 as.Date("2020-02-01"),
                                                 as.Date("2020-03-01"),
                                                 as.Date("2020-03-15"),
                                                 as.Date("2020-04-01"),
                                                 as.Date("2020-04-30")))+
  labs(y='Daily average of Kilometers\n per passenger (log)', x='') +
  theme_minimal() +
  
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.ticks.y = element_line(colour = "black"))

ggsave('./figures/avg_kilometers_per_passenger.pdf', dpi=300, width = 16, units = 'cm')
ggsave('./figures/avg_kilometers_per_passenger.png', dpi=300, width = 16, units = 'cm')




# CHART total_km_x_passengers

ggplot()+
  geom_line(data = bb,
            aes(x = date, y = total_km_x_passengers/1000), color='#008080', size = .8, show.legend = FALSE) +
  geom_vline(data = bb, color='gray60', size=1.8, alpha=.4, aes(xintercept = as.Date(as.Date("2020-03-15"))))+
  scale_y_log10(labels = comma  ) +
  scale_x_date(date_labels = "%d %b", breaks = c(as.Date("2020-01-01"),
                                                 as.Date("2020-02-01"),
                                                 as.Date("2020-03-01"),
                                                 as.Date("2020-03-15"),
                                                 as.Date("2020-04-01"),
                                                 as.Date("2020-04-30")))+
  labs(y='Daily total\npassenger kilometers/1000 (log)', x='') +
  theme_minimal() +
  
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.ticks.y = element_line(colour = "black"))

ggsave('./figures/total_passenger_kilometers.pdf', dpi=300, width = 16, units = 'cm')


Figure x1 - Daily total number of passenger kilometers flown.
Figure x2 - Daily average of kilometers travelled per passenger.

Figure x1 shows a sharp decrease in the number of passengers and flights. Air traffic in the country saw a 19-fold drop
from 225 million passengers kilometers per day between Jan-1st and mid-March to only 12 million a day in April. Nonetheless, Figure x2 shows a sudden increase in the average distance travelled by passengers in the period, 
from approximately 1,000 to 1,300 Km per passenger per day.

Combined, this figures illustrate how the COVID-19 pandemic caused a substantial
decline in air traffic in Brazil, particularly for shorter flights, what led to 
fewer passengers travelling longer distances on average.


bb[between(date, as.Date("2020-01-01"), as.Date("2020-03-12")),]$total_km_x_passengers %>% mean()
bb[between(date, as.Date("2020-04-01"), as.Date("2020-04-30")),]$total_km_x_passengers %>% mean()

bb[between(date, as.Date("2020-01-01"), as.Date("2020-03-12")),]$mean_km_by_passengers %>% mean()
bb[between(date, as.Date("2020-04-01"), as.Date("2020-04-30")),]$mean_km_by_passengers %>% mean()

