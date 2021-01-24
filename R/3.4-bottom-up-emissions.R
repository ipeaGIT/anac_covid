#
# Tier3A approach
# 

rm(list=ls())
# tolower_noaccent <- function(i){
#   i <- sub("^\\s+", "",i)
#   i <- stringi::stri_trans_general(i,id = "Latin-ASCII") %>% 
#     tolower() %>% 
#     stringr::str_replace_all("-"," ")  %>% 
#     stringr::str_replace_all("'","")
# }

gc(reset = T)
library(gghighlight)
library(janitor)
source("R/0_loadpackages.R",local = TRUE)
`%nin%` = Negate(`%in%`)

anac_files <- list.files(path = "../../data-raw/ANAC/",pattern = 'basica',full.names = TRUE)

flight <- future.apply::future_lapply(anac_files,function(i){
  data.table::fread(i,dec=",",encoding = 'Latin-1')
}) %>% data.table::rbindlist()
flight <- flight[nm_pais_origem %in% 'BRASIL' & 
                   nm_pais_destino %in% 'BRASIL',]
#flight[,ds_modelo := tolower_noaccent(ds_modelo)]

tier3a_filepath <- "tier3a/tiera3a_data.xlsx"

openxlsx::getSheetNames(tier3a_filepath)

#dt <- fread("dados_aeronaves.csv",skip=1)
#dt[,MODELO := tolower_noaccent(MODELO)]

#
# 1) LTO
#

lto_ef <- openxlsx::read.xlsx(tier3a_filepath,"LTO_EF_FC_2019_EUROCENTER") %>% setDT()
my_func_string <- function(i){
  output <- stringr::str_split(i,"\\(")[[1]][1] %>% stringr::str_remove_all(" ")
  return(output)
}
lto_ef[,ICAO := lapply(.SD,my_func_string),.SDcols = "Aircraft",by = Aircraft]

# 1.1) add EF into "flight" data

flight[lto_ef,on = c("sg_equipamento_icao" = "ICAO"),
       `:=`(Number_engine = i.Number_of_engines,
            Type_engine = i.Type_of_engine,
            p7_trust = i.7p_trust,
            p30_trust = i.30p_trust,
            p85_trust = i.85p_trust,
            p100_trust = i.100p_trust)]

# > flight[is.na(Number_engine),]$ds_modelo %>% unique()
# [1] "AEROSPATIALE/ALENIA ATR 72 FREIGHTER"          
# [2] "AEROSPATIALE/ALENIA ATR 42-300 / 320"          
# [3] "AEROSPATIALE/ALENIA ATR 42-500"                
# [4] "EMBRAER EMB.110 BANDEIRNATE"                   
# [5] "AEROSPATIALE/ALENIA ATR 72 201/202"            
# [6] "ANTONOV AN-12"                                 
# [7] "AEROSPATIALE/ALENIA ATR 72-500/72-212A (500)  "
# [8] "CESSNA 208 CARAVAN"                            
# [9] ""                                              
# [10] "FAIRCHILD SWEARINGEN METRO"                    
# [11] "LOCKHEED L-182 / 282 / 382 (L-100) HERCULES"

# fix NA values from merge
# dt <- openxlsx::read.xlsx("Caracteristicasfisicaseoperacionaisdeaeronavescomerciais.xlsx",
#                           startRow = 4) %>% setDT()

dt1 <- fread("dados_aeronaves.csv")

dt[Modelo.da.aeronave %like% "ATR",]
lto_ef[Aircraft %like% "Aerospatiale",]$ICAO
flight[ds_modelo %like% "AEROSPATIALE",]$sg_equipamento_icao %>% unique()
flight[ds_modelo %like% "AEROSPATIALE",]$ds_modelo %>% unique()
nrow(flight)
nrow(flight[is.na(Number_engine),])

# add units
units::install_symbolic_unit("engines")
trust_cols <- c("p7_trust","p30_trust","p85_trust","p100_trust")
flight[,(trust_cols) := lapply(.SD,as.numeric),.SDcols = trust_cols]
flight[,(trust_cols) := lapply(.SD,units::set_units,"kg / (s * engines)"),.SDcols = trust_cols]
flight[,("Number_engine") := lapply(.SD,units::set_units,"engines"),.SDcols = "Number_engine"]


# 1.2) add "taxi-in" and "taxi-out" times into "aerodrome"

# 1.2.1) check "aerdrome" that is private or public
sg_icao_pri <- fread("cadastro-de-aerodromos-civis-privados.csv",skip = 2)
sg_icao_pub <- fread("cadastro-de-aerodromos-civis-publicos.csv",
                     skip=2,encoding = "UTF-8")

flight[sg_icao_pri,
        on = c("sg_icao_origem" = "Código OACI"),
        sg_icao_origem_type := "private"]
flight[sg_icao_pri,
        on = c("sg_icao_destino" = "Código OACI"),
        sg_icao_destino_type := "private"]
flight[sg_icao_pub,
        on = c("sg_icao_origem" = "V1"),
        sg_icao_origem_type := "public"]
flight[sg_icao_pub,
        on = c("sg_icao_destino" = "V1"),
        sg_icao_destino_type := "public"]

# 1.2.2) add taxi times and fill aerdromes specifics

tx <- openxlsx::read.xlsx(tier3a_filepath,"TEMPO_MEDIO_AERODROMO") %>% setDT()

flight[tx[year %in% 2018,],on = c("sg_icao_origem" = "aerodrome"),
       `:=`(taxi_out = i.taxi_out)]
flight[tx[year %in% 2018,],on = c("sg_icao_destino" = "aerodrome"),
       `:=`(taxi_in = i.taxi_in)]

# private aerodromes: taxi_in = 1 | taxi_out = 2 min 
flight[sg_icao_destino_type == "private",`:=`(taxi_in = 1,taxi_out = 2)


# aeródromos públicos onde voos regulares não são operados: 
# foram atribuídos, para cada um dos anos, a média dos
# tempos de taxi-in e taxi-out dos aeroportos cuja movimentação anual foi
# inferior a mil voos regulares
#
aero_low1k <- data.table::copy(flight)[sg_icao_origem_type == "public",]
aero_low1k <- aero_low1k[,.N,by= sg_icao_origem][N<1000,sg_icao_origem]
avg_time <- data.table::copy(flight)[sg_icao_origem %in% aero_low1k,]
avg_time <- avg_time[,.SD[1],by = sg_icao_origem]
avg_time <- avg_time[,lapply(.SD,mean,na.rm=TRUE),
                     .SDcols = c("taxi_in","taxi_out")]

aero_nop <- data.table::copy(flight)[sg_icao_origem_type == "public",]
aero_nop <- aero_nop[,.N,by=.(ds_grupo_di,sg_icao_origem)]
aero_nop <- aero_nop[,voo_regular := fifelse("REGULAR" %in%  ds_grupo_di ,
                                             "SIM","NAO"),by = sg_icao_origem]
aero_nop <- aero_nop[voo_regular == "NAO",]$sg_icao_origem

flight[sg_icao_origem %in% aero_nop,`:=`(taxi_in = avg_time$taxi_in,
                                       taxi_out = avg_time$taxi_out)]

# add 'taxi_in' and 'taxi_out' in airports with missing data 
# and low movimentation
# - this is going to impact few flight in 2017

flight[is.na(taxi_in) & sg_icao_destino %in% aero_low1k,taxi_in := avg_time$taxi_in]
flight[is.na(taxi_out) & sg_icao_origem %in% aero_low1k,taxi_out := avg_time$taxi_out]

# add missing to mising movimentation airport (similar_mav)
flight[,Nmov_origin := .N,by= sg_icao_origem]
flight[,Nmov_destin := .N,by= sg_icao_destino]

# 0-1000
tmpflight_out0 <-  data.table::copy(flight)[Nmov_origin <= 1000,taxi_out]
flight[is.na(taxi_out) & 
         Nmov_origin <= 1000 ,taxi_out := mean(tmpflight_out0,na.rm=TRUE)]

tmpflight_in0 <-  data.table::copy(flight)[Nmov_destin <= 1000,taxi_in]
flight[is.na(taxi_in) & 
         Nmov_destin <= 1000,taxi_in := mean(tmpflight_in0,na.rm=TRUE)]

# 1000-3000
tmpflight_out <-  data.table::copy(flight)[Nmov_origin > 1000 & Nmov_origin < 3000,taxi_out]
flight[is.na(taxi_out) & 
         Nmov_origin > 1000 & 
         Nmov_origin < 3000,taxi_out := mean(tmpflight_out,na.rm=TRUE)]

tmpflight_in <-  data.table::copy(flight)[Nmov_destin > 1000 & Nmov_destin < 3000,taxi_in]
flight[is.na(taxi_in) & 
         Nmov_destin > 1000 & 
         Nmov_destin < 3000,taxi_in := mean(tmpflight_in,na.rm=TRUE)]

# 3000 - 5000
tmpflight_out1 <-  data.table::copy(flight)[Nmov_origin > 3001 & Nmov_origin < 5000,taxi_out]
flight[is.na(taxi_out) & 
         Nmov_origin > 3001 & 
         Nmov_origin < 5000,taxi_out := mean(tmpflight_out1,na.rm=TRUE)]

tmpflight_in2 <-  data.table::copy(flight)[Nmov_destin > 3001 & Nmov_destin < 5000,taxi_in]
flight[is.na(taxi_in) & 
         Nmov_destin > 3001 & 
         Nmov_destin < 5000,taxi_in := mean(tmpflight_in2,na.rm=TRUE)]

#
# 1.3) add 'take off'/'approach'/''climbing'--------
#

power_lto <- openxlsx::read.xlsx(tier3a_filepath,"POWER_LTO")

flight[power_lto,on]




#
# fuel consumption LTO (to do)
#

#
# 1.3) APU
# 1.3.1) add 'fuselagem' information into 'flight' data

large_body <- c("B747","A330","A340","A380")
for(i in large_body){
  flight[sg_equipamento_icao %in% i,fuselagem := "Fuselagem larga do tipo jumbo"]
}



aeronave_cat[`Aircraft.code.(Type.designator)` %in% 
               flight$sg_equipamento_icao[1],]




aeronave_cat[exemplo_aeronaves %in% flight[1]$ds_modelo,]



























