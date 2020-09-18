library(dplyr)
library(data.table)
library(ggplot2)
library(scales)
library(magrittr)
library(gghighlight)
library(viridis)

options(scipen=999)

# open basica df


files <- list.files(path = '../data-raw/', pattern = '-01|-02|-03|-04', full.names = T)
files <- grep("basica", files, value =  TRUE)
basica <- lapply(files, fread) %>% rbindlist()

data_cols <- basica %>% select(id_basica, 
                                  nr_voo, sg_empresa_icao,
                                  hr_partida_real, dt_partida_real, sg_icao_origem,
                                  nm_municipio_origem, sg_uf_origem, nm_pais_origem, 
                                  hr_chegada_real, dt_chegada_real,
                                  sg_icao_destino, nm_municipio_destino, sg_uf_destino, nm_pais_destino,
                                  nr_escala_destino,
                                  ds_modelo, nr_passag_pagos, nr_passag_gratis)

# set locale to english to convert weeknames
Sys.setlocale("LC_ALL","English")


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
