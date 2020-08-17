#
# initial configuration -------
#

rm(list=ls())
gc(reset = T)
library(gghighlight)
source("emission/0_loadpackages.R",local = TRUE)
`%nin%` = Negate(`%in%`)

anac_files <- list.files(path = "data-raw/anac",pattern = '.txt',full.names = TRUE)
flight <- lapply(anac_files,function(i){
  data.table::fread(i,dec=",",encoding = 'Latin-1')
}) %>% data.table::rbindlist()

ls_initial_list <- c("flight","anac_files","%nin%")
suppressWarnings(dir.create("figures/"))

#
# organize basica-data
#


flight <- flight[nm_pais_origem %in% 'BRASIL' & 
                   nm_pais_destino %in% 'BRASIL', ]
flight <- flight[!is.na(lt_combustivel) & lt_combustivel > 0,]
flight <- flight[,.(nr_ano_referencia,nr_mes_referencia,dt_referencia,
                    sg_icao_origem,sg_iata_origem,
                    nr_passag_gratis,nr_passag_pagos,
                    id_equipamento,ds_modelo,sg_equipamento_icao,
                    lt_combustivel,km_distancia,nr_horas_voadas,
                    sg_icao_destino,sg_iata_destino)]

flight[,nr_passag_total := nr_passag_gratis + nr_passag_pagos]
flight <- flight[!is.na(nr_passag_total) & nr_passag_total > 0,]
flight[, dt_referencia := as.POSIXct(dt_referencia,tz = "America/Bahia")]

break()
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
flightdf <- data.table::copy(flight)

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

flightdf[,lt_combustivel := lt_combustivel %>% units::set_units('l') %>% units::set_units('m^3')]
flightdf[,emi_co2 := units::set_units(lt_combustivel * FE * pci * rho,'t')]

#
# annual difference (ad)
#

ad <- data.table::copy(flightdf)
ad[,date := as.Date(dt_referencia)]
ad[,year := format(date,"%Y")]
ad[,year_month := format(date,"%m/%Y")]
ad <- ad[,lapply(.SD,sum),by = year,.SDcols = 'emi_co2']
ad[,total := emi_co2/max(emi_co2)]
ad$emi_co2/10^6

# emissions per day

number_flights <- data.table::copy(flightdf)[,.N,by = .(dt_referencia)]
setkey(number_flights,'dt_referencia')
temp_flight <- data.table::copy(flightdf)
data.table::setkeyv(temp_flight,c("nr_ano_referencia","dt_referencia"))
temp_flight <- temp_flight[, lapply(.SD, sum), 
                           .SDcols = c('lt_combustivel','emi_co2','nr_passag_total'), 
                           by = .(dt_referencia,nr_ano_referencia)]
temp_flight <- temp_flight[number_flights, on = 'dt_referencia']
temp_flight <- temp_flight[, id := 1:.N, by = .(nr_ano_referencia)]
temp_flight[,dia_mes := format(dt_referencia,"%d/%m")]
temp_flight[,nr_ano_referencia := as.integer(nr_ano_referencia)]

temp_flight[,`:=`(frollmean2 = data.table::frollmean(emi_co2,2),
                  frollmean3 = data.table::frollmean(emi_co2,3),
                  frollmean4 = data.table::frollmean(emi_co2,4),
                  frollmean7 = data.table::frollmean(emi_co2,7)),
            by = .(nr_ano_referencia)]


temp_flight[dia_mes %like% "01/01",xname := "Jan"]
temp_flight[dia_mes %like% "01/02",xname := "Feb"]
temp_flight[dia_mes %like% "01/03",xname := "Mar"]
temp_flight[dia_mes %like% "01/04",xname := "Apr"]
temp_flight[dia_mes %like% "01/05",xname := "May"]
temp_flight[dia_mes %like% "01/06",xname := "Jun"]
temp_flight[dia_mes %like% "30/06",xname := "Jul"]

temp_flight[,emi_co2_pp := units::set_units(emi_co2,'kg') / nr_passag_total]
temp_flight[,`:=`(frollmean2_pp = data.table::frollmean(emi_co2_pp,2),
                  frollmean3_pp = data.table::frollmean(emi_co2_pp,3),
                  frollmean4_pp = data.table::frollmean(emi_co2_pp,4),
                  frollmean7_pp = data.table::frollmean(emi_co2_pp,7)),
            by = .(nr_ano_referencia)]

temp_flight[,`:=`(name_total = 'total',
                  name_pp = 'per capita')]
temp_melt <- data.table::melt(data = temp_flight,
                              id.vars = c('nr_ano_referencia','id','xname'),
                              measure.vars =  list('name' = c('name_total','name_pp'),
                                                   'emissions' = c('emi_co2','emi_co2_pp'),
                                                   'mean' = c('frollmean7','frollmean7_pp')),
                              variable.factor = TRUE)
temp_melt$name <- factor(temp_melt$name, c("total","per capita"))
id_breaks <- which(!is.na(temp_flight[nr_ano_referencia %in% 2019,]$xname))
label_id <- as_labeller(c(`total` = "Total emissions (t)",
                          `2019` = '2019', `2020` = '2020',
                          `per capita` = "Emissions per capita \n(kg / passenger)"))

ggplot(data = temp_melt) + 
  geom_bar(aes(x = id, y = as.numeric(emissions), fill = as.factor(nr_ano_referencia)),
           position = "dodge",
           stat = "identity") +
  scale_fill_brewer(palette = "YlOrRd") + 
  theme_bw() + 
  theme(legend.position = "none") +
  geom_line(aes(x = id,
                y = mean), linetype = 1) +
  facet_grid(cols = vars(nr_ano_referencia), 
             rows = vars(name), scale = 'free',
             labeller = label_id) + 
  scale_x_continuous(breaks = id_breaks,
                     labels = temp_flight$xname[id_breaks]) +
  labs(x = NULL, y = NULL, title = "CO2 emissions",
       subtitle = "Emissions from January to June") +
  theme(strip.background = element_rect(color= 'black', fill = 'white'),
        axis.text.x = element_text(size = rel(1),angle = 0))

ggsave("figures/co2emission_time.png",width = 22,height = 14.2,
       units = "cm",scale = 0.85)


#
# efficiency analysis ------
#
temp_flight <- data.table::copy(flight)
airports <- temp_flight[,.N, by = . (sg_iata_origem,sg_iata_destino)][order(N,decreasing = TRUE),]
for(i in 1:10){ 
  # i = 1
  temp_ef <- data.table::copy(temp_flight)[sg_iata_origem %in% airports$sg_iata_origem[i] & 
                                             sg_iata_destino %in% airports$sg_iata_destino[i] & 
                                             !is.na(nr_passag_total) & 
                                             nr_passag_total > 0,]
  
  temp_dsmodelo <- temp_ef[,.N,by = ds_modelo][order(N,decreasing = TRUE),ds_modelo][1]
  temp_ef <- temp_ef[ds_modelo %in% temp_dsmodelo,]
  
  flightdf <- temp_ef[, lapply(.SD, mean), 
                      .SDcols = c('lt_combustivel','nr_horas_voadas','nr_passag_total'), 
                      by = .(dt_referencia,nr_ano_referencia)][order(dt_referencia),]
  flightdf <- flightdf[, id := 1:.N, by = .(nr_ano_referencia)]
  flightdf[,dia_mes := format(dt_referencia,"%d/%m")]
  flightdf[,nr_ano_referencia := as.integer(nr_ano_referencia)]
  
  flightdf[,lt_combustivel_pp := lt_combustivel / nr_passag_total]
  flightdf[,nr_horas_voadas_pp := nr_horas_voadas / nr_passag_total]
  
  p1 <- ggplot(data = flightdf) + 
    geom_line(aes(x = id, y = lt_combustivel_pp, 
                  color = as.factor(nr_ano_referencia))) +
    geom_point(aes(x = id, y = lt_combustivel_pp, 
                   color = as.factor(nr_ano_referencia))) +
    theme_bw() + 
    theme(legend.position = "none") +
    scale_y_continuous(breaks = 
                         seq(min(flightdf$lt_combustivel_pp),
                             max(flightdf$lt_combustivel_pp),leng = 10)) + 
    scale_x_continuous(breaks = seq(1,
                                    uniqueN(flightdf$id),9),
                       labels = unique(flightdf$dia_mes)[seq(1,uniqueN(flightdf$id),9)]) +
    labs(x = NULL, y = NULL, color = NULL,
         title = paste0(i,") ",airports$sg_iata_origem[i],
                        " to ", airports$sg_iata_destino[i], " by ",temp_dsmodelo),
         subtitle = "Daily mean fuel" ) +
    theme(axis.text.x = element_text(size = rel(0.85),angle = 15))
  p1
  p2 <- ggplot(data = flightdf) + 
    geom_line(aes(x = id, y = nr_horas_voadas_pp, 
                  color = as.factor(nr_ano_referencia))) +
    geom_point(aes(x = id, y = nr_horas_voadas_pp, 
                   color = as.factor(nr_ano_referencia))) +
    theme_bw() + 
    theme(legend.position = "bottom") +
    scale_y_continuous(breaks = 
                         seq(min(flightdf$nr_horas_voadas_pp),
                             max(flightdf$nr_horas_voadas_pp),leng = 10)) + 
    scale_x_continuous(breaks = seq(1,
                                    uniqueN(flightdf$id),9),
                       labels = unique(flightdf$dia_mes)[seq(1,uniqueN(flightdf$id),9)]) +
    labs(x = NULL, y = NULL, color = NULL,
         subtitle = paste0("Daily mean time")) +
    theme(axis.text.x = element_text(size = rel(0.85),angle = 15))
  
  pf <- p1 / p2
  filename <- paste0("figures/eff/",i,"_",airports$sg_iata_origem[i],
                     "_to_", airports$sg_iata_destino[i],".png")
  ggsave(filename,plot = pf, width = 21.4,height = 22,
         units = "cm",scale = 0.9)
}

#
# working with times periods -----
#
temp_flight <- data.table::copy(flight)
airports <- temp_flight[,.N, by = . (sg_iata_origem,sg_iata_destino)][order(N,decreasing = TRUE),]

dt <- lapply(1:10,function(i){ # i = 1
  temp_ef <- temp_flight[sg_iata_origem %in% airports$sg_iata_origem[i] & 
                           sg_iata_destino %in% airports$sg_iata_destino[i] & 
                           !is.na(nr_passag_total) & 
                           nr_passag_total > 0,]
  temp_dsmodelo <- temp_ef[,.N,by = ds_modelo][order(N,decreasing = TRUE),ds_modelo][1]
  temp_ef <- temp_ef[ds_modelo %in% temp_dsmodelo,]
  temp_ef[,dt_referencia := as.Date(dt_referencia)]
  temp_ef[data.table::between(dt_referencia,"2020-02-01","2020-03-16"),time_phase := "02-01 to 03-16"]
  temp_ef[data.table::between(dt_referencia,"2020-03-16","2020-04-30"),time_phase := "03-16 to 04-30"]
  temp_ef[data.table::between(dt_referencia,"2019-02-01","2019-03-16"),time_phase := "02-01 to 03-16"]
  temp_ef[data.table::between(dt_referencia,"2019-03-16","2019-04-30"),time_phase := "03-16 to 04-30"]
  
  h1 <- temp_ef[data.table::between(dt_referencia,"2020-02-01","2020-03-16"),nr_horas_voadas]
  h2 <- temp_ef[data.table::between(dt_referencia,"2020-03-16","2020-04-30"),nr_horas_voadas]
  
  f1 <- temp_ef[data.table::between(dt_referencia,"2020-02-01","2020-03-16"),lt_combustivel]
  f2 <- temp_ef[data.table::between(dt_referencia,"2020-03-16","2020-04-30"),lt_combustivel]
  
  p1 <- temp_ef[data.table::between(dt_referencia,"2020-02-01","2020-03-16"),nr_passag_total]
  p2 <- temp_ef[data.table::between(dt_referencia,"2020-03-16","2020-04-30"),nr_passag_total]
  
  res_h <- wilcox.test(h1, h2, alternative = "two.sided")
  res_f <- wilcox.test(f1, f2, alternative = "two.sided")
  res_p <- wilcox.test(p1, p2, alternative = "two.sided")
  
  dfoutput <- data.table::data.table(
    'origin' = airports$sg_iata_origem[i],
    'destination' = airports$sg_iata_destino[i],
    'flight time before' = mean(h1),
    'flight time after' = mean(h2),
    'wilcox.test flight time' = ifelse(res_h$p.value > 0.05,round(res_h$p.value,3),"< 0.05"),
    'fuel per flight before' = mean(f1),
    'fuel per flight after' = mean(f2),
    'wilcox.test fuel per flight' = ifelse(res_f$p.value > 0.05,round(res_f$p.value,3),"< 0.05"),
    'passenger per flight before' = mean(p1),
    'passenger per flight after' = mean(p2),
    'wilcox.test passanger per flight' = ifelse(res_p$p.value > 0.05,round(res_p$p.value,3),"< 0.05")
  )
  
  return(dfoutput)
}) %>% data.table::rbindlist()



