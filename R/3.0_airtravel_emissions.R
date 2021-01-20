#
# initial configuration -------
#

rm(list=ls())
gc(reset = T)
library(gghighlight)
source("R/0_loadpackages.R",local = TRUE)
`%nin%` = Negate(`%in%`)

anac_files <- list.files(path = "../../data-raw/ANAC/",pattern = 'basica',full.names = TRUE)


flight <- future.apply::future_lapply(anac_files,function(i){
  data.table::fread(i,dec=",",encoding = 'Latin-1')
}) %>% data.table::rbindlist()

#
# organize basica-data
#

flight[,nr_passag_total := nr_passag_gratis + nr_passag_pagos]
flightbr <- flight[nm_pais_origem %in% 'BRASIL' & 
                     nm_pais_destino %in% 'BRASIL',]
flightnbr <- flight[nm_pais_origem %nin% 'BRASIL' | 
                      nm_pais_destino %nin% 'BRASIL',]
nrow(flightbr) + nrow(flightnbr)
nrow(flight)
sum(flightbr$nr_passag_total,na.rm = TRUE) /sum(flight$nr_passag_total,na.rm = TRUE)
nrow(flightbr) / nrow(flight)

# kg_load := kg_bagagem_excesso + kg_bagagem_livre + 
#         kg_carga_gratis + kg_carga_paga + kg_correio + 
#         nr_passag_total * 75

flight <- flight[nm_pais_origem %in% 'BRASIL' & 
                   nm_pais_destino %in% 'BRASIL',
                 .(nr_ano_referencia,nr_mes_referencia,dt_referencia,
                   sg_icao_origem,sg_iata_origem,
                   nr_passag_gratis,nr_passag_pagos,
                   cd_tipo_linha,
                   kg_bagagem_livre,kg_bagagem_excesso,
                   kg_carga_paga,kg_carga_gratis,kg_correio,kg_peso,
                   id_equipamento,ds_modelo,sg_equipamento_icao,
                   lt_combustivel,km_distancia,nr_horas_voadas,
                   sg_icao_destino,sg_iata_destino)]
flight[,nr_passag_total := nr_passag_gratis + nr_passag_pagos]
flight[nr_passag_total == 0, nr_passag_total := NA]
flight[kg_peso == 0, kg_peso := NA]
flight[km_distancia == 0, km_distancia := NA]
flight[lt_combustivel == 0, lt_combustivel := NA]
flight[nr_horas_voadas == 0, nr_horas_voadas := NA]
flight[, dt_referencia := as.POSIXct(dt_referencia,tz = "America/Bahia")]
flight[, t_peso := units::set_units(kg_peso,'kg') %>% units::set_units('t')]

#
# co2 estimation ------
#
# ref FE: IPPCC
# https://www.ipcc-nggip.iges.or.jp/public/2006gl/pdf/2_Volume2/V2_3_Ch3_Mobile_Combustion.pdf
#
# ref pci: poder calorifico
# https://www.epe.gov.br/pt/publicacoes-dados-abertos/publicacoes/BEN-Series-Historicas-Completas
#
# ref density (rho):
# https://www.epe.gov.br/pt/publicacoes-dados-abertos/publicacoes/BEN-Series-Historicas-Completas
#

FE <- units::set_units(71500,'kg/TJ') 
limits_pci <- c(11090,10400) %>% 
  units::set_units('kcal/kg') %>% 
  units::set_units('TJ/kg')
pci <- mean(limits_pci) 
rho <- units::set_units(799,'kg/m^3')

#
# calculation
# liters to m^3
# 

flight[,lt_combustivel := lt_combustivel %>% units::set_units('l') %>% units::set_units('m^3')]
flight[,emi_co2 := units::set_units(lt_combustivel * FE * pci * rho,'t')]

# filter
# flight <- flight[nr_ano_referencia %in% c(2019,2020),]

# write expressions
readr::write_rds(flight,
                 "../../data/anac_covid/emissions_basica.rds",compress = "gz")


