#
# initial configuration  -------------------------------
#

rm(list=ls())
gc(reset = T)
source("R/0_loadpackages.R",local = TRUE)
`%nin%` = Negate(`%in%`)


#### read raw data   -------------------------------
anac_files <- list.files(path = "../../data-raw/ANAC/",pattern = 'basica',full.names = TRUE)
anac_files <- anac_files[anac_files %like% "2020" | anac_files %like% "2019"]
rm(flight)
flight_raw <- future.apply::future_lapply(anac_files,function(i){
  message(i)
  a <- data.table::fread(i,dec=",",encoding = 'Latin-1')
  return(a)
}) %>% data.table::rbindlist()


#
# check proportions of domestic/cargo vs passenger
#

flight_raw[nm_pais_origem %in% 'BRASIL' & nm_pais_destino %in% 'BRASIL',] %>% nrow()
flight_raw[cd_tipo_linha == "N" | cd_tipo_linha == "C",]  %>% nrow()
flight_raw[cd_tipo_linha == "N" | cd_tipo_linha == "C",]$cd_tipo_linha  %>% table()
#flightbr <- flight[cd_tipo_linha == "N" | cd_tipo_linha == "C",]
# flightbr <- flight[cd_tipo_linha == "N",]
# flightbr <- flightbr[nr_ano_referencia %in% c(2019,2020)]
# flightbr1 <- flightbr[,.N,by = .(cd_tipo_linha,nr_ano_referencia)]
# flightbr1[,prop := 100*round(N/sum(N),3), by = nr_ano_referencia]
# flightbr1
# 
# flightbr2 <- flightbr[data.table::between(dt_referencia,"2020-02-15","2020-04-14"),]
# flightbr2[data.table::between(dt_referencia,"2020-02-15","2020-03-15"),
#           time_phase := "15/Feb to 15/Mar"]
# flightbr2[data.table::between(dt_referencia,"2020-03-16","2020-04-14"),
#           time_phase := "16/Mar to 14/Apr"]
# flightbr2 <- flightbr2[,.N,by = .(cd_tipo_linha,time_phase)]
# flightbr2[,prop := 100*round(N/sum(N),3), by = time_phase]
# flightbr2
#
# organize basica-data   -------------------------------
#

#flight[,nr_passag_total := nr_passag_gratis + nr_passag_pagos]
# flightbr <- flight[nm_pais_origem %in% 'BRASIL' & 
#                      nm_pais_destino %in% 'BRASIL',]
# flightnbr <- flight[nm_pais_origem %nin% 'BRASIL' | 
#                       nm_pais_destino %nin% 'BRASIL',]
# nrow(flightbr) + nrow(flightnbr)
# nrow(flight)
# sum(flightbr$nr_passag_total,na.rm = TRUE) /sum(flight$nr_passag_total,na.rm = TRUE)
# nrow(flightbr) / nrow(flight)

# kg_load := kg_bagagem_excesso + kg_bagagem_livre + 
#         kg_carga_gratis + kg_carga_paga + kg_correio + 
#         nr_passag_total * 75

flight <- data.table::copy(flight_raw)
flight <- flight[nm_pais_origem %in% 'BRASIL' & 
                   nm_pais_destino %in% 'BRASIL',]

# N - Doméstica Mista: para operações de transporte aéreo de passageiros ou
# mistas, em que todos os aeródromos envolvidos estejam situados simultaneamente
# em território brasileiro;

flight <- flight[cd_tipo_linha == "N",]
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
pci_max <- max(limits_pci) 
rho <- units::set_units(799,'kg/m^3')

#
# calculation
# liters to m^3
# 

flight[,lt_combustivel := lt_combustivel %>% units::set_units('l') %>% 
         units::set_units('m^3')]
flight[,emi_co2 := units::set_units(lt_combustivel * FE * pci * rho,'t')]
flight[,emi_co2_max := units::set_units(lt_combustivel * FE * pci_max * rho,'t')]
# save data with emissions of all individual flights
readr::write_rds(flight,
                 "../../data/anac_covid/emissions_basica.rds",compress = "gz")





#
# fill empty data (check differences on fuel comsuption)
#

fuel_od <- data.table::copy(flight)[,mean(emi_co2,na.rm = TRUE),by = .(sg_icao_origem ,sg_icao_destino)]
flight_miss <- data.table::copy(flight)[is.na(emi_co2),]
flight_ok <- data.table::copy(flight)[!is.na(emi_co2),]
flight_miss[fuel_od,on = c("sg_icao_origem" = "sg_icao_origem",
                           "sg_icao_destino" = "sg_icao_destino"),
            emi_co2 := i.V1]
flight_fix <- data.table::rbindlist(list(flight_ok,flight_miss))

flight_fix[,sum(emi_co2,na.rm = TRUE),by = "nr_ano_referencia"]
flight_miss[,sum(emi_co2,na.rm = TRUE),by = "nr_ano_referencia"]
 flight[,sum(emi_co2,na.rm = TRUE),by = "nr_ano_referencia"]


#
# 2019 ANP data
#
dt2019 <- openxlsx::read.xlsx("querosene-aviacao-municipio-2019.xlsx",startRow = 5) %>% setDT() 
dt2019 <- dt2019[!is.na(CÓDIGO.IBGE),]
dt2019 <- dt2019[-1]
dt2019[,Vendas := Vendas %>%  units::set_units("liter") %>% units::set_units("m^3") ]
dt2019[,emi_co2 := units::set_units(Vendas * FE * pci * rho,'t')]
dt2019$emi_co2 %>% sum() %>% units::set_units("Mt")
dt2019$Vendas %>% sum()


# EPE - planilha gastos kerosene aviacao
# BEN (2017,2018,2019)
#
x <- c(3296,3387,3348) * 10^7 %>% units::set_units("Mcal") 
(x * FE) %>% units::set_units("Mt")  # emissions of CO2
(x / pci) %>% units::set_units("Mt") # Mtons of QAv


# Tier 1 and Tier 3A differences on Co2 emissions (for kerosene)
# based on ANAC inventory
# (source: pdf document page 30)

anac_emi <- data.table::data.table("year" = c(2005:2009,2010:2018),
                                   "tier1"=c(7644,7128,7838,8414,8464,9543.3,10683.6,11260.8,10802,10928,10804,9889,9865,10140),
                                   "tier3"=c(5764,5992,6532,6484,7632,9001,10277,10445,10016,10110,10264,9605,9632,9845))
anac_emi[,diff := 100* tier1/tier3 - 100]
anac_emi[year > 2009,]$diff %>% summary()

#
# resumo anual evaluation from ANAC database
#
list_anual_files <- list.files("../../data-raw/ANAC/",pattern = ".csv",full.names = T)
dt2020 <- lapply(list_anual_files, fread,dec = ",") %>% rbindlist()
names(dt2020) <-  janitor::make_clean_names(names(dt2020))

dt2020[combustivel_litros == 0, combustivel_litros := NA]
dt2020 <- dt2020[aeroporto_de_origem_pais == "BRASIL" &
                   aeroporto_de_destino_pais == "BRASIL",  ]
dt2020 <- dt2020[,sum(combustivel_litros,na.rm = TRUE),by = ano ]
dt2020[,fuel := V1 %>% units::set_units("liter") %>% units::set_units("m^3")]
dt2020[,emi_co2 := units::set_units(fuel * FE * pci * rho,'Mt')]
dt2020[ano > 2016,emi_co2]
plot(dt2020$ano,dt2020$fuel,type="o")

# 
# comparison ANAC Tier 3 and our method
#
diffs <- dt2020[ano > 2004 & ano < 2019,]
diffs[anac_emi,on = c("ano"="year"),tier3a := i.tier3]
diffs[,tier3a := (tier3a/1000) %>% units::set_units("Mt")]
diffs[,diff := 100*(1- as.numeric(emi_co2/tier3a))]
diffs$diff %>% summary()
#
# ratios in december (total moviments)
#
flight_tmp <- flight[nm_pais_origem %in% 'BRASIL' & 
         nm_pais_destino %in% 'BRASIL',status_flight := "nacional"]
flight_tmp[is.na(status_flight), status_flight := "internacional"]
flight_tmp <- flight_tmp[,.N,by = .(nr_ano_referencia,status_flight,nm_mes_partida_real)]
flight_tmp <- flight_tmp[nm_mes_partida_real == "DEZEMBRO",]
flight_tmp[,ratio := 100*round(N/sum(N),4),by= nr_ano_referencia]
flight_tmp

#
# ratios in december (total passangers)
#

flight_tmp0 <- data.table::copy(flight)[nm_pais_origem %in% 'BRASIL' & 
                       nm_pais_destino %in% 'BRASIL',status_flight := "nacional"]
flight_tmp0[is.na(status_flight), status_flight := "internacional"]
flight_tmp0 <- flight_tmp[,sum(nr_passag_total,na.rm=TRUE),by = .(nr_ano_referencia,status_flight,nm_mes_partida_real)]
flight_tmp0 <- flight_tmp[nm_mes_partida_real == "DEZEMBRO",]
flight_tmp0[,ratio := 100*round(V1/sum(V1),4),by= nr_ano_referencia]
flight_tmp0[nr_ano_referencia >= 2019,]

#
# estimate 'load-factor'
#
load_factor <- readr::read_rds("../../data/anac_covid/emissions_basica.rds")
load_factor_df <- data.table::copy(load_factor) %>% 
  .[,kg_peso := as.numeric(kg_peso)] %>% 
  .[,kg_payload := as.numeric(kg_payload)] %>% 
  .[!is.na(nr_passag_total) & nr_passag_total > 1,] %>% 
  .[kg_peso > 1 & kg_peso != "Inf", ] %>% 
  .[kg_payload > 1 & kg_payload != "Inf", ] %>% 
  .[,load_factor := kg_peso / kg_payload] %>% 
  .[,load_factor_check := identical(kg_peso,kg_payload)] 
load_factor_df$load_factor %>% summary()
load_factor_df$kg_peso %>% summary()
load_factor_df$kg_payload %>% summary()
load_factor_df[load_factor > 1.05,.(nr_passag_total,kg_peso,kg_payload)]
load_factor$load_factor_check %>% table()
