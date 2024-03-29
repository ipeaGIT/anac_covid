library(dplyr)
library(data.table)
library(ggplot2)
library(scales)
library(magrittr)
library(gghighlight)
library(viridis)
library(readr)

options(scipen=999)

# TRAFFIC FLOW BETWEEN MUNICIPALITIES ----------------------------------------------

file_combinada <- list.files(path = './data-raw/ANAC/', pattern = 'combinada', full.names = T)
combinada <- fread(file = file_combinada)

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

# save it
write_rds(data_cols, "./data/anac_covid/combinada_cols.rds")






# only brazilian flights --------------------------------------------
data_cols <- read_rds("../../data/anac_covid/combinada_cols.rds")

# filter only 2020
data_cols <- data_cols[year == 2020]

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



# open dists - all cities
airports <- flightsbr::read_airports(type = 'all')

# calculate distances between airports
dists <- fread("../../data/anac_covid/airports_dist-df.csv")

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


data_brazil_dists_first <- data_brazil_dists_first[!is.na(nr_passag_pagos)]
data_brazil_dists_first <- data_brazil_dists_first[!is.na(nr_passag_gratis)]

fwrite(data_brazil_dists_first, "../../data/anac_covid/flights_passengers_complete.csv")



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
fwrite(odmatrix_passdist_filter, "../../data/anac_covid/air_odmatrix_filter.csv")


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
fwrite(odmatrix_passdist_filter_agreg, "../../data/anac_covid/air_odmatrix_filter_agreg.csv")
