#
# initial configuration -------
#

rm(list=ls())
gc(reset = T)
library(gghighlight)
source("R/0_loadpackages.R",local = TRUE)
`%nin%` = Negate(`%in%`)

# read emissions

flight <- readr::read_rds("../../data/anac_covid/emissions_basica.rds")
flight <- flight[nr_ano_referencia %in% c(2019,2020),]
flight[,nr_passag_total := nr_passag_gratis + nr_passag_pagos]
flight[,N := .N, by = .(sg_iata_destino,sg_iata_origem,nr_ano_referencia)]

common_pairs <- flight[,.SD[1],by = .(sg_iata_destino,
                                      sg_iata_origem,
                                      nr_ano_referencia)]
common_pairs <- common_pairs[order(N,decreasing = TRUE),][1:10][,.(sg_iata_destino,
                                                                  sg_iata_origem)]


plot_hist <- function(i){ # i = 1
  
  tmp_flight <- flight[sg_iata_origem == common_pairs$sg_iata_origem[i] & 
                         sg_iata_destino == common_pairs$sg_iata_destino[i],]
  
  pre <- tmp_flight[data.table::between(dt_referencia,"2020-02-01","2020-03-16"),
                      nr_horas_voadas] %>% as.numeric()
  pos <- tmp_flight[data.table::between(dt_referencia,"2020-03-16","2020-04-30"),
                      nr_horas_voadas] %>% as.numeric()

  res_p <- wilcox.test(pre, pos, alternative = "two.sided")
  
  dfoutput <- data.table::data.table(
    "origem" = common_pairs$sg_iata_origem[i], 
    "destino" = common_pairs$sg_iata_destino[i],
    'pre_time' = mean(pre, na.rm = TRUE),
    'pos_time' = mean(pos, na.rm = TRUE),
    'wilcox_test' = ifelse(res_p$p.value > 0.05,round(res_p$p.value,3),"< 0.05"))
  
  return(dfoutput)
}

dt_time <- lapply(1:10,plot_hist) %>% data.table::rbindlist()


dt_time
flight1 <- flight[,`:=`(min = lapply(.SD,min,na.rm = TRUE),
                         q1 = lapply(.SD,quantile,probs = 0.25,na.rm = TRUE),
                         median = lapply(.SD,median,na.rm = TRUE),
                         mean = lapply(.SD,mean,na.rm = TRUE),
                         q3 = lapply(.SD,quantile,probs = 0.75,na.rm = TRUE),
                         max = lapply(.SD,max,na.rm = TRUE),
                         N = .N),
                   by = .(sg_iata_destino,sg_iata_origem,nr_ano_referencia),
                  .SDcols = "nr_horas_voadas"][,.SD[1],
                                            by = .(sg_iata_destino,
                                                   sg_iata_origem,
                                                   nr_ano_referencia)]
flight1[N>1000,][,.(sg_iata_destino,sg_iata_origem,nr_ano_referencia,
                    min,q1,median,mean,q3,max,N)]
