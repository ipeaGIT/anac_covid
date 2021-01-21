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
flight <- flight[nr_mes_referencia %nin% 12,]
flight <- flight[nr_ano_referencia %in% c(2019,2020),]

# emissions per day

number_flights <- data.table::copy(flight)[,.N,by = .(dt_referencia)]
setkey(number_flights,'dt_referencia')
temp_flight1 <- data.table::copy(flight)
data.table::setkeyv(temp_flight1,c("nr_ano_referencia","dt_referencia"))
temp_flight1 <- temp_flight1[, lapply(.SD, sum, na.rm=TRUE), 
                             .SDcols = c('lt_combustivel','emi_co2','nr_passag_total'), 
                             by = .(dt_referencia,nr_ano_referencia)]
temp_flight1 <- temp_flight1[number_flights, on = 'dt_referencia']
temp_flight1 <- temp_flight1[, id := 1:.N, by = .(nr_ano_referencia)]
temp_flight1[,dia_mes := format(dt_referencia,"%d/%m")]
temp_flight1[,nr_ano_referencia := as.integer(nr_ano_referencia)]

temp_flight1[,`:=`(frollmean2 = data.table::frollmean(emi_co2,n = 2, na.rm=TRUE),
                   frollmean3 = data.table::frollmean(emi_co2,n = 3, na.rm=TRUE),
                   frollmean4 = data.table::frollmean(emi_co2,n = 4, na.rm=TRUE),
                   frollmean7 = data.table::frollmean(emi_co2,n = 7, na.rm=TRUE)),
             by = .(nr_ano_referencia)]


# add month name

month_name_aux <- c("Jan","Fev","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dez")
dia_mes_aux <- c(paste0("01/0",1:9),paste0("01/",10:11),"30/11")
for(i in seq_along(dia_mes_aux)) temp_flight1[dia_mes %like% dia_mes_aux[i],month_name := month_name_aux[i]]

# emissions per capita
temp_flight1[,emi_co2_pp := units::set_units(emi_co2,'kg') / nr_passag_total]
temp_flight1[,`:=`(frollmean2_pp = data.table::frollmean(emi_co2_pp,2, na.rm=TRUE),
                   frollmean3_pp = data.table::frollmean(emi_co2_pp,3, na.rm=TRUE),
                   frollmean4_pp = data.table::frollmean(emi_co2_pp,4, na.rm=TRUE),
                   frollmean7_pp = data.table::frollmean(emi_co2_pp,7, na.rm=TRUE)),
             by = .(nr_ano_referencia)]

temp_flight1[,`:=`(name_total = 'total',
                   name_pp = 'per capita')]


temp_melt <- data.table::melt(data = temp_flight1,
                              id.vars = c('nr_ano_referencia','id','month_name'),
                              measure.vars =  list('name' = c('name_total','name_pp'),
                                                   'emissions' = c('emi_co2','emi_co2_pp'),
                                                   'mean' = c('frollmean7','frollmean7_pp')),
                              variable.factor = TRUE)
temp_melt$name <- factor(temp_melt$name, c("total","per capita"))

id_breaks <- which(!is.na(temp_flight1[nr_ano_referencia %in% 2019,]$month_name))
label_id <- as_labeller(c(`total` = "Total emissions (t)",
                          `2019` = '2019', `2020` = '2020',
                          `per capita` = "Emissions per capita \n(kg / passenger)"))

fig5a <- ggplot(data = temp_melt[name %in% "per capita",]) + 
  geom_point(aes(x = id, y = as.numeric(emissions), 
                 color = as.factor(nr_ano_referencia)), shape = 21) +
  theme_bw() + 
  theme(legend.position = c(0.90,0.80)) +
  geom_line(aes(x = id,
                y = mean, 
                color = as.factor(nr_ano_referencia)), 
            size = 1.05,
            linetype = 1) +
  scale_color_manual(values = c("gray72","dodgerblue1")) + 
  # facet_wrap(facets = vars(name),nrow = 2,
  #            scales = 'free_y',strip.position = "right",
  #            labeller = label_id) + 
  coord_cartesian(xlim = c(min(temp_melt$id),max(temp_melt$id)),
                  expand = FALSE) + 
  geom_point(data = data.frame(x = 1,y=0),
             aes(x = x,y = y ),color = 'white',size = 0.01) + 
  scale_x_continuous(breaks = id_breaks,
                     labels = temp_flight1$month_name[id_breaks]) +
  labs(x = "Month", y = expression("CO"[2]*" (kg/passenger)"), color = NULL#
       #title = expression("CO"[2]*" emissions"),
       #subtitle = "Emissions from January to November"
  ) +
  theme(strip.background = element_rect(color= 'black', fill = 'white'),
        axis.text.x = element_text(size = rel(1),angle = 0))
fig5a/fig5a

# ggsave("figures/fig5a-co2emission_time.png",plot = fig5a,width = 21,height = 20,
#       units = "cm",scale = 0.85)




#
#
# fig5 - working with times periods -----
#


lotacao <- data.table::copy(flight)[data.table::between(dt_referencia,"2020-02-01","2020-04-30"),]
lotacao[nr_passag_total == "Inf" | nr_passag_total == "-Inf",nr_passag_total := NA]
lotacao <- lotacao[!is.na(nr_passag_total),]
#summary(lotacao$nr_passag_total,na.rm=TRUE)
lotacao <- lotacao[,`:=`(min = lapply(.SD,min,na.rm = TRUE),
                         q1 = lapply(.SD,quantile,probs = 0.25,na.rm = TRUE),
                         median = lapply(.SD,median,na.rm = TRUE),
                         mean = lapply(.SD,mean,na.rm = TRUE),
                         q3 = lapply(.SD,quantile,probs = 0.75,na.rm = TRUE),
                         max = lapply(.SD,max,na.rm = TRUE),
                         N = .N),
                   by = ds_modelo,.SDcols = "nr_passag_total"]
lotacao <- lotacao[,.SD[1],by = ds_modelo][,.(ds_modelo,min,q1,median,q3,max,N)]
lotacao$ds_modelo

models_representative <- lotacao[max > 10,]$ds_modelo


#
# pax efficiency analysis ------
#

units::install_symbolic_unit("passanger")
temp_flight <- data.table::copy(flight)

temp_flight[,lt_combustivel := units::set_units(lt_combustivel,"liter")]
temp_flight[,nr_passag_total := units::set_units(nr_passag_total,"passanger")]
temp_flight[,km_distancia := units::set_units(km_distancia,"km")]
temp_flight[,fuel_efficiency := (nr_passag_total * km_distancia) / lt_combustivel]
temp_flight <- temp_flight[!is.na(fuel_efficiency),]

# data periods
temp_flight[data.table::between(dt_referencia,"2020-02-01","2020-03-16"),time_phase := "01/Feb to 16/Mar"]
temp_flight[data.table::between(dt_referencia,"2020-03-16","2020-04-30"),time_phase := "16/Mar to 30/Apr"]
temp_flight <- temp_flight[!is.na(time_phase),]

# add models info

temp_flight[ds_modelo %like% c("AEROSPATIALE/ALENIA"), capacity := "40-80"]
# BOING
temp_flight[ds_modelo %like% c("737-300"),capacity := "120-160"] # 269
temp_flight[ds_modelo %like% c("737-500"),capacity := "160-200"] # 168
temp_flight[ds_modelo %like% c("737-700"),capacity := "120-160"] # 149
temp_flight[ds_modelo %like% c("737-800"),capacity := "120-160"] # 162
temp_flight[ds_modelo %like% c("757-200"),capacity := "120-160"] # 130
temp_flight[ds_modelo %like% c("777-200/200 ER"),capacity := "280-320"] # 320
temp_flight[ds_modelo %like% c("777-300ER PAX"),capacity := "360-400"] # 396
temp_flight[ds_modelo %like% c("787-9 PAX"),capacity := "280-320"] # 290 
temp_flight[ds_modelo %like% c("767-300"),capacity := "240-280"] # 269
# CANADAIR REGIONAL JET 200
temp_flight[ds_modelo %like% c("JET 200"),capacity := "40-80"] # 50
# EMBRAER
temp_flight[ds_modelo %like% c("E190"),capacity := "800-120"] # 114
temp_flight[ds_modelo %like% c("195"),capacity := "120-160"]  # 124
temp_flight[ds_modelo %like% c("E195-E2"),capacity := "120-160"] # 136
# AIRBUS
temp_flight[ds_modelo %like% c("A319"),capacity := "120-160"] # 156
temp_flight[ds_modelo %like% c("A320"),capacity := "160-200"] # 186
temp_flight[ds_modelo %like% c("A321"),capacity := "200-240"] # 230
temp_flight[ds_modelo %like% c("A330"),capacity := "240-280"] # 277
temp_flight[ds_modelo %like% c("A350-900"),capacity := "240-280"] # 276 
temp_flight[ds_modelo %like% c("A350-900NEO"),capacity := "280-320"]# 298  


temp_flight <- temp_flight[!is.na(capacity),]


# averages
my_temp_flight <- data.table::copy(temp_flight)[, lapply(.SD, mean,na.rm = TRUE), 
                                                .SDcols = c('km_distancia','fuel_efficiency',
                                                            't_peso','emi_co2','nr_horas_voadas',
                                                            'nr_passag_total','lt_combustivel'),by = .(capacity,time_phase)]
#
# 
#
my_temp_flight <- list(
  my_temp_flight[time_phase %in% "01/Feb to 16/Mar",],
  my_temp_flight[time_phase %in% "16/Mar to 30/Apr",]
) %>% data.table::rbindlist()
data.table::setindex(my_temp_flight,time_phase)

my_temp_flight$capacity <- factor(my_temp_flight$capacity,c("40-80","120-160","160-200",
                                                            "200-240","240-280","360-400"))


my_temp_flight[,x_ini := lapply(.SD[1],first),.SDcols = c('km_distancia'),by = 'capacity']
my_temp_flight[,x_end := lapply(.SD[2],first),.SDcols = c('km_distancia'),by = 'capacity']
my_temp_flight[,y_ini_fuel_efficiency := lapply(.SD[1],first),.SDcols = c("fuel_efficiency"),by = 'capacity']
my_temp_flight[,y_end_fuel_efficiency := lapply(.SD[2],first),.SDcols = c("fuel_efficiency"),by = 'capacity']


my_temp_flight <- my_temp_flight[!is.na(x_end),]

# melt data for ggplot2
my_temp_flight <- data.table::melt(data = my_temp_flight,
                                   id.vars = c('capacity','time_phase','x_ini','x_end'),
                                   measure.vars = list(
                                     "y_ini" = c('y_ini_fuel_efficiency'),
                                     "y_end" =  c('y_end_fuel_efficiency')),
                                   variable.name = "category")
my_temp_flight[category %in% 1,category  := "fuel_efficiency"]

my_temp_flight[time_phase %in% "01/Feb to 16/Mar",`:=`(x_coord = x_ini,y_coord = y_ini)]  
my_temp_flight[time_phase %in% "16/Mar to 30/Apr",`:=`(x_coord = x_end,y_coord = y_end)]  

# label facet
label_id <- as_labeller(c(`fuel_efficiency` = "Fuel efficiency (pax-km/L)"))


fig5b <- ggplot(data = my_temp_flight) + 
  geom_point(data = my_temp_flight, aes(x = as.numeric(x_coord), y = as.numeric(y_coord), 
                                        color = as.factor(capacity), 
                                        shape = time_phase),size = 3) +
  scale_shape_manual(values= c(19,21)) + 
  scale_x_continuous(breaks = seq(0,2500,by = 250)) +
  scale_y_continuous(breaks = seq(0,50,by = 5)) + 
  scale_color_manual(values = viridis_pal(option = "A")(28)[round(seq(1,28,length.out = 7),0)],aesthetics = "color") + 
  labs(#title = ,
    #subtitle = ,
    color = 'Aircraft maximum available seats', x = "Average distance (km)",shape = "Period",
    y = "Average fuel efficiency (pax-km/L)") + 
  geom_segment(data = my_temp_flight, aes(x = as.numeric(x_ini),
                                          y = as.numeric(y_ini),
                                          xend = as.numeric(x_end),
                                          yend = as.numeric(y_end),
                                          colour = as.factor(capacity)),
               arrow=arrow(length = unit(0.10, "inches"),type = 'open')) + 
  theme_bw() + 
  theme(legend.position = "bottom") +
  #facet_grid(cols = vars(category),scales = "free",labeller = label_id) +
  guides(color = guide_legend(title.position = "top",override.aes = list(shape = NA),
                              ncol = 3,color = guide_legend(ncol=2)),
         shape = guide_legend(title.position = "top",ncol = 1)) 
fig5b

# merge figures-----
fig5 <- (fig5a / fig5b)  + plot_annotation(tag_levels = 'A',
                                           title = expression("CO"[2]*" emissions and fuel efficiency"),
                                           subtitle = "Variation between pre and post COVID outbreak",
                                           #caption = 'Disclaimer: None of these plots are insightful'
)

fig5
ggsave("figures/fig5-emissions.png",plot = fig5, width = 27.5, height = 30.0, units = "cm",
       scale = 0.6, dpi = 300)

#
#
# teste de hipotese--------
#
#

aircraft_seat_range <- unique(temp_flight$capacity)
dt <- lapply(1:length(aircraft_seat_range),function(i){  # i = 6
  # i = 1
  
  message(aircraft_seat_range[i])
  
  temp_ef <- data.table::copy(temp_flight)[capacity %in% aircraft_seat_range[i],]
  
  e1 <- temp_ef[time_phase %in% "01/Feb to 16/Mar",fuel_efficiency] %>% as.numeric()
  e2 <- temp_ef[time_phase %in% "16/Mar to 30/Apr",fuel_efficiency] %>% as.numeric()
  
  d1 <- temp_ef[time_phase %in% "01/Feb to 16/Mar",km_distancia] %>% as.numeric()
  d2 <- temp_ef[time_phase %in% "16/Mar to 30/Apr",km_distancia] %>% as.numeric()
  
  l1 <- temp_ef[time_phase %in% "01/Feb to 16/Mar",kg_peso] %>% as.numeric()
  l2 <- temp_ef[time_phase %in% "16/Mar to 30/Apr",kg_peso] %>% as.numeric()
  
  h1 <- temp_ef[time_phase %in% "01/Feb to 16/Mar",nr_horas_voadas] %>% as.numeric()
  h2 <- temp_ef[time_phase %in% "16/Mar to 30/Apr",nr_horas_voadas] %>% as.numeric()
  
  f1 <- temp_ef[time_phase %in% "01/Feb to 16/Mar",lt_combustivel] %>% as.numeric()
  f2 <- temp_ef[time_phase %in% "16/Mar to 30/Apr",lt_combustivel] %>% as.numeric()
  
  p1 <- temp_ef[time_phase %in% "01/Feb to 16/Mar",nr_passag_total] %>% as.numeric()
  p2 <- temp_ef[time_phase %in% "16/Mar to 30/Apr",nr_passag_total] %>% as.numeric()
  
  res_e <- wilcox.test(e1, e2, alternative = "two.sided")
  res_d <- wilcox.test(d1, d2, alternative = "two.sided")
  res_l <- wilcox.test(l1, l2, alternative = "two.sided")
  res_h <- wilcox.test(h1, h2, alternative = "two.sided")
  res_f <- wilcox.test(f1, f2, alternative = "two.sided")
  res_p <- wilcox.test(p1, p2, alternative = "two.sided")
  
  dfoutput <- data.table::data.table(
    'model' = aircraft_seat_range[i],
    'N' = nrow(temp_ef),
    'eff_before' = mean(e1, na.rm = TRUE),
    'eff_after' = mean(e2, na.rm = TRUE),
    'eff_test' = ifelse(res_e$p.value > 0.05,round(res_e$p.value,3),"< 0.05"),
    'weight_before' = mean(l1, na.rm = TRUE),
    'weight_after' = mean(l2, na.rm = TRUE),
    'weight_test' = ifelse(res_l$p.value > 0.05,round(res_l$p.value,3),"< 0.05"),
    'time_before' = mean(h1, na.rm = TRUE),
    'time_after' = mean(h2, na.rm = TRUE),
    'time_test' = ifelse(res_h$p.value > 0.05,round(res_h$p.value,3),"< 0.05"),
    'fpf_before' = mean(f1, na.rm = TRUE), # Fuel Per Flight
    'fpf_after' = mean(f2, na.rm = TRUE),
    'fpf_test' = ifelse(res_f$p.value > 0.05,round(res_f$p.value,3),"< 0.05"),
    'ppf_before' = mean(p1, na.rm = TRUE), # passenger_per_flight
    'ppf_after' = mean(p2, na.rm = TRUE),
    'ppf_test' = ifelse(res_p$p.value > 0.05,round(res_p$p.value,3),"< 0.05"),
    'dist_before' = mean(d1, na.rm = TRUE),
    'dist_after' = mean(d2,na.rm = TRUE),
    'dist_test' = ifelse(res_d$p.value > 0.05,round(res_d$p.value,3),"< 0.05")
  )
  
  # message(dfoutput)
  return(dfoutput)
})  %>% data.table::rbindlist()

dt[,percent := round(N/sum(N),2)]
dt
View(dt[,c(1:11)])
View(dt[,c(1,12:17)])









# ggsave("figures/efficiency.png",width = 19.2, height = 20.0, units = "cm",
#        scale = 0.9, dpi = 300)

#
# efficiency analysis ------
#

temp_flight <- data.table::copy(flight)
temp_flight[,fuel_efficiency := nr_passag_total * km_distancia / lt_combustivel]
ds_models <- temp_flight[,.N, by = . (ds_modelo)][order(N,decreasing = TRUE),]
for(i in 1:15){ 
  # i = 1
  temp_ef <- data.table::copy(temp_flight)[ds_modelo %in% ds_models[i],]
  
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