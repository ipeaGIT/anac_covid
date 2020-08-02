#
# emission estimate draft -------
#
rm(list=ls())
library(data.table);library(magrittr)
library(openxlsx);library(XLConnect)

flight <- data.table::fread("data-raw/ANAC/basica2020-01/basica2020-01.txt",
                            encoding = "Latin-1",dec = ",")
flight <- flight[nm_municipio_origem %in% "GUARULHOS" & 
                   nm_municipio_destino %in% "MANAUS",][1,]
flight <- flight[,.(sg_icao_origem,sg_iata_origem,
                    id_equipamento,ds_modelo,sg_equipamento_icao,
                    lt_combustivel,km_distancia,nr_horas_voadas,
                    sg_icao_destino,sg_iata_destino)]
#
# "Taxi in/out times" --------
#

taxitimes_anac <- readODS::read_ods("data/emission_factor.ods",sheet = 2) %>% data.table::as.data.table()
taxitimes_anac <- taxitimes_anac[Airport %in% c('SBGR','SBEG'),.SD[.N],by = .(Airport,operating_mode)]

flight[taxitimes_anac[operating_mode %in% 'Taxi-in',],
       on = c('sg_icao_origem' = 'Airport'), Taxi_in := i.Time]
flight[taxitimes_anac[operating_mode %in% 'Taxi-out',],
       on = c('sg_icao_destino' = 'Airport'), Taxi_out := i.Time]

# "ACT_PRF.aircraft-engine"
aircraft_engine <- openxlsx::read.xlsx(xlsxFile = ef_path, sheet = "ACT_PRF.aircraft-engine", startRow = 0)
aircraft_engine <- aircraft_engine %>% data.table::as.data.table()

#
# "ACT_PRF.AIRCRAFT_LTO_VALUES" --------
#
ef_path <- "data-raw/EF_data/1.A.3.a Aviation - Annex 5 - LTO emissions calculator 2019.xlsm"
aircraft_lto <- openxlsx::read.xlsx(xlsxFile = ef_path, sheet = "ACT_PRF.AIRCRAFT_LTO_VALUES", startRow = 2)  %>%
  data.table::as.data.table()
aircraft_lto %>% head(2)
