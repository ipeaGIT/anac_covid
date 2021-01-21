#
# initial configuration -------
#

rm(list=ls())
gc(reset = T)
library(gghighlight)
source("R/0_loadpackages.R",local = TRUE)
`%nin%` = Negate(`%in%`)


# read flight emissions estimates -------

# read data with emissions of all individual flights
flightdf <- readr::read_rds("../../data/anac_covid/emissions_basica.rds")
head(flightdf)

summary(flightdf$nr_passag_total)
summary(flightdf$emi_co2)


#
# annual difference (ad)  -------
#

ad <- data.table::copy(flightdf)
ad[,date := as.Date(dt_referencia)]
ad[,year := format(date,"%Y")]
ad[,year_month := format(date,"%m/%Y")]
ad <- ad[,lapply(.SD,sum, na.rm=TRUE),by = year,.SDcols = 'emi_co2']
ad[,total := emi_co2/max(emi_co2)]
ad$emi_co2/10^6
ad$total



######### get total emissions per day  -------------------------------

number_flights <- data.table::copy(flightdf)[,.N,by = .(dt_referencia)]
setkey(number_flights,'dt_referencia')

temp_flight <- data.table::copy(flightdf)
data.table::setkeyv(temp_flight,c("nr_ano_referencia","dt_referencia"))
temp_flight <- temp_flight[, lapply(.SD, sum, na.rm=TRUE), 
                           .SDcols = c('lt_combustivel','emi_co2','nr_passag_total'), 
                           by = .(dt_referencia,nr_ano_referencia)]

temp_flight <- temp_flight[number_flights, on = 'dt_referencia']
temp_flight <- temp_flight[, id := 1:.N, by = .(nr_ano_referencia)]
temp_flight[,dia_mes := format(dt_referencia,"%d/%m")]
temp_flight[,nr_ano_referencia := as.integer(nr_ano_referencia)]

head(temp_flight)
table(temp_flight$nr_ano_referencia)
summary(temp_flight$emi_co2)

# export
readr::write_rds(temp_flight, "./outputs/impact_input_emissions.rds", compress = 'gz')




##### prepare plot -----------------------------------

temp_flight[,`:=`(frollmean2 = data.table::frollmean(emi_co2,n = 2, na.rm=TRUE),
                  frollmean3 = data.table::frollmean(emi_co2,n = 3, na.rm=TRUE),
                  frollmean4 = data.table::frollmean(emi_co2,n = 4, na.rm=TRUE),
                  frollmean7 = data.table::frollmean(emi_co2,n = 7, na.rm=TRUE)),
            by = .(nr_ano_referencia)]


temp_flight[dia_mes %like% "01/01",xname := "Jan"]
temp_flight[dia_mes %like% "01/02",xname := "Feb"]
temp_flight[dia_mes %like% "01/03",xname := "Mar"]
temp_flight[dia_mes %like% "01/04",xname := "Apr"]
temp_flight[dia_mes %like% "01/05",xname := "May"]
temp_flight[dia_mes %like% "01/06",xname := "Jun"]
temp_flight[dia_mes %like% "01/07",xname := "Jul"]
temp_flight[dia_mes %like% "31/07",xname := "Aug"]

temp_flight[,emi_co2_pp := units::set_units(emi_co2,'kg') / nr_passag_total]
temp_flight[,`:=`(frollmean2_pp = data.table::frollmean(emi_co2_pp,2, na.rm=TRUE),
                  frollmean3_pp = data.table::frollmean(emi_co2_pp,3, na.rm=TRUE),
                  frollmean4_pp = data.table::frollmean(emi_co2_pp,4, na.rm=TRUE),
                  frollmean7_pp = data.table::frollmean(emi_co2_pp,7, na.rm=TRUE)),
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
  geom_point(aes(x = id, y = as.numeric(emissions), 
                 color = as.factor(nr_ano_referencia)), shape = 21) +
  theme_bw() + 
  theme(legend.position = c(0.065,0.415)) +
  geom_line(aes(x = id,
                y = mean, 
                color = as.factor(nr_ano_referencia)), 
            size = 1.05,
            linetype = 1) +
  scale_color_manual(values = c("gray72","dodgerblue1")) + 
  facet_wrap(facets = vars(name),nrow = 2,
             scales = 'free_y',strip.position = "right",
             labeller = label_id) + 
  coord_cartesian(xlim = c(min(temp_melt$id),max(temp_melt$id)),
                  expand = FALSE) + 
  geom_point(data = data.frame(x = 1,y=0),
             aes(x = x,y = y ),color = 'white',size = 0.01) + 
  scale_x_continuous(breaks = id_breaks,
                     labels = temp_flight$xname[id_breaks]) +
  labs(x = NULL, y = NULL, color = "Year",
       title = expression("CO"[2]*" emissions"),
       subtitle = "Emissions from January to June") +
  theme(strip.background = element_rect(color= 'black', fill = 'white'),
        axis.text.x = element_text(size = rel(1),angle = 0))

ggsave("figures/co2emission_time.png",width = 21,height = 20,
       units = "cm",scale = 0.85)




#
# working with times periods -----
#

temp_flight <- data.table::copy(flight)
temp_ef[,dt_referencia := as.Date(dt_referencia)]
ds_models <- temp_flight[data.table::between(dt_referencia,"2020-02-01","2020-03-16") & 
                           data.table::between(dt_referencia,"2020-03-16","2020-04-30"),][,
                                                                                          .N, by = . (ds_modelo)][order(N,decreasing = TRUE),]
dt <- lapply(1:8,function(i){ 
  # i = 8
  
  message(i)
  temp_ef <- data.table::copy(temp_flight)[ds_modelo %in% ds_models$ds_modelo[i] & 
                                             !is.na(nr_passag_total) & 
                                             nr_passag_total > 0,]
  temp_ef[,dt_referencia := as.Date(dt_referencia)]
  temp_ef[data.table::between(dt_referencia,"2020-02-01","2020-03-16"),time_phase := "02-01 to 03-16"]
  temp_ef[data.table::between(dt_referencia,"2020-03-16","2020-04-30"),time_phase := "03-16 to 04-30"]
  temp_ef[data.table::between(dt_referencia,"2019-02-01","2019-03-16"),time_phase := "02-01 to 03-16"]
  temp_ef[data.table::between(dt_referencia,"2019-03-16","2019-04-30"),time_phase := "03-16 to 04-30"]
  temp_ef <- temp_ef[!is.na(time_phase),]
  
  d1 <- temp_ef[time_phase %in% "02-01 to 03-16",km_distancia]
  d2 <- temp_ef[time_phase %in% "03-16 to 04-30",km_distancia]
  
  l1 <- temp_ef[time_phase %in% "02-01 to 03-16",kg_peso]
  l2 <- temp_ef[time_phase %in% "03-16 to 04-30",kg_peso]
  
  h1 <- temp_ef[time_phase %in% "02-01 to 03-16",nr_horas_voadas]
  h2 <- temp_ef[time_phase %in% "03-16 to 04-30",nr_horas_voadas]
  
  f1 <- temp_ef[time_phase %in% "02-01 to 03-16",lt_combustivel]
  f2 <- temp_ef[time_phase %in% "03-16 to 04-30",lt_combustivel]
  
  p1 <- temp_ef[time_phase %in% "02-01 to 03-16",nr_passag_total]
  p2 <- temp_ef[time_phase %in% "03-16 to 04-30",nr_passag_total]
  
  res_d <- wilcox.test(d1, d2, alternative = "two.sided")
  res_l <- wilcox.test(l1, l2, alternative = "two.sided")
  res_h <- wilcox.test(h1, h2, alternative = "two.sided")
  res_f <- wilcox.test(f1, f2, alternative = "two.sided")
  res_p <- wilcox.test(p1, p2, alternative = "two.sided")
  
  dfoutput <- data.table::data.table(
    'model' = ds_models$ds_modelo[i],
    'N' = nrow(temp_ef),
    'weight time before' = mean(l1, na.rm = TRUE),
    'weight time after' = mean(l2, na.rm = TRUE),
    'weight test' = ifelse(res_l$p.value > 0.05,round(res_l$p.value,3),"< 0.05"),
    'flight time before' = mean(h1, na.rm = TRUE),
    'flight time after' = mean(h2, na.rm = TRUE),
    'wilcox.test flight time' = ifelse(res_h$p.value > 0.05,round(res_h$p.value,3),"< 0.05"),
    'fuel per flight before' = mean(f1, na.rm = TRUE),
    'fuel per flight after' = mean(f2, na.rm = TRUE),
    'wilcox.test fuel per flight' = ifelse(res_f$p.value > 0.05,round(res_f$p.value,3),"< 0.05"),
    'passenger per flight before' = mean(p1, na.rm = TRUE),
    'passenger per flight after' = mean(p2, na.rm = TRUE),
    'wilcox.test passanger per flight' = ifelse(res_p$p.value > 0.05,round(res_p$p.value,3),"< 0.05"),
    'avg_km before' = mean(d1, na.rm = TRUE),
    'avg_km after' = mean(d2,na.rm = TRUE),
    'wilcox.test avg dist' = ifelse(res_d$p.value > 0.05,round(res_d$p.value,3),"< 0.05")
  )
  
  return(dfoutput)
}) %>% data.table::rbindlist()

View(dt[,c(1:11)])
View(dt[,c(1,12:17)])
#
# pax efficiency analysis ------
#

temp_flight <- data.table::copy(flight)
temp_flight[,fuel_efficiency := nr_passag_total * km_distancia / lt_combustivel]
temp_flight <- temp_flight[!is.na(fuel_efficiency),]
# data periods
temp_flight[data.table::between(dt_referencia,"2020-02-01","2020-03-16"),
            time_phase := "02-01 to 03-16"]
temp_flight[data.table::between(dt_referencia,"2020-03-16","2020-04-30"),
            time_phase := "03-16 to 04-30"]
temp_flight <- temp_flight[!is.na(time_phase),]
# check aircraft models
ds_models <- temp_flight[fuel_efficiency < 100, unique(ds_modelo)]
ds_models <- dplyr::setdiff(ds_models,"CESSNA 208 CARAVAN")
# averages
temp_flight <- temp_flight[ds_modelo %in% ds_models,]
temp_flight <- temp_flight[, lapply(.SD, mean,na.rm = TRUE), 
                           .SDcols = c('km_distancia','fuel_efficiency',
                                       't_peso'), 
                           by = .(ds_modelo,time_phase)]

#temp_flight2 <- data.table::copy(temp_flight)
temp_flight <- list(
  temp_flight[time_phase %in% "02-01 to 03-16",],
  temp_flight[time_phase %in% "03-16 to 04-30",]
) %>% data.table::rbindlist()
data.table::setindex(temp_flight,time_phase)


temp_flight[,x_ini := lapply(.SD[1],first),.SDcols = c('km_distancia'),by = 'ds_modelo']
temp_flight[,x_end := lapply(.SD[2],first),.SDcols = c('km_distancia'),by = 'ds_modelo']
for(i in c('fuel_efficiency','t_peso')){
  temp_flight[,paste0("y_ini_",i) := lapply(.SD[1],first),.SDcols = c(i),by = 'ds_modelo']
  temp_flight[,paste0("y_end_",i) := lapply(.SD[2],first),.SDcols = c(i),by = 'ds_modelo']
}

temp_flight <- temp_flight[!is.na(x_end),]

# melt data for ggplot2
temp_flight <- data.table::melt(data = temp_flight,
                                id.vars = c('ds_modelo','time_phase','x_ini','x_end'),
                                measure.vars = list(
                                  "y_ini" = c('y_ini_fuel_efficiency',
                                              'y_ini_t_peso'),
                                  "y_end" =  c('y_end_fuel_efficiency',
                                               'y_end_t_peso')),
                                variable.name = "category")
temp_flight[category %in% 1,category  := "fuel_efficiency"]        
temp_flight[category %in% 2,category  := "t_peso"]    

temp_flight[time_phase %in% "02-01 to 03-16",`:=`(x_coord = x_ini,y_coord = y_ini)]  
temp_flight[time_phase %in% "03-16 to 04-30",`:=`(x_coord = x_end,y_coord = y_end)]  

temp_flight[ds_modelo %in% 'AIRBUS A320-100/200',]

# remove outliers
temp_modelo <- temp_flight[category %in% 'fuel_efficiency' & 
                             y_coord > 100,ds_modelo]
temp_flight <- temp_flight[ds_modelo %nin% temp_modelo,]
# label facet
label_id <- as_labeller(c(`t_peso` = "Total load (t)",
                          `fuel_efficiency` = "Fuel efficiency (pax-km/L)"))



ggplot(data = temp_flight) + 
  geom_point(data = temp_flight
             , aes(x = x_coord, y = y_coord, 
                   color = as.factor(ds_modelo), 
                   shape = time_phase),size = 3) +
  scale_shape_manual(values= c(19,21)) + 
  scale_y_continuous(breaks = seq(0,50,by = 10)) + 
  labs(title = "Average fuel efficiency and total load of aircraft used on Brazil routes",
       subtitle = "Variation between period pre and post COVID outbreak",
       color = 'Aircraft type', x = "Average distance (km)",shape = "Period",
       y = NULL) + 
  geom_segment(data = temp_flight
               , aes(x = x_ini,y = y_ini,
                     xend = x_end,yend = y_end,
                     colour = as.factor(ds_modelo)),
               arrow=arrow(length = unit(0.10, "inches"),type = 'open')) + 
  theme_bw() + 
  theme(legend.position = "bottom") +
  facet_grid(cols = vars(category),scales = "free",labeller = label_id) +
  guides(color = guide_legend(title.position = "top",override.aes = list(shape = NA),
                              ncol = 3,color = guide_legend(ncol=2)),
         shape = guide_legend(title.position = "top",ncol = 1)) 

ggsave("figures/efficiency.png",width = 27.5, height = 18.0, units = "cm",
       scale = 0.80, dpi = 300)
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