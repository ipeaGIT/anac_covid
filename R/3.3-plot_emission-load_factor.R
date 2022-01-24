###### initial configuration -------

rm(list=ls())
gc(reset = T)
library(gghighlight)
source("R/0_loadpackages.R",local = TRUE)
`%nin%` = Negate(`%in%`)
`%nlike%` = Negate(`%like%`)

pre_covid_period <- c("2020-02-01","2020-03-15")
pos_covid_period <- c("2020-03-16","2020-04-30")
pre_covid_period_name <- "01/Feb to 15/Mar"
pos_covid_period_name <- "16/Mar to 30/Apr"

#setwd("L:/Proj_acess_oport/git_rafa/anac_covid")
###### read emissions------

flight_raw <- readr::read_rds("../../data/anac_covid/emissions_basica.rds")
flight <- data.table::copy(flight_raw) %>% 
  .[nr_ano_referencia %in% c(2019,2020),] %>% 
  .[,kg_peso := as.numeric(kg_peso)] %>% 
  .[,kg_payload := as.numeric(kg_payload)] %>% 
  .[!is.na(nr_passag_total) & nr_passag_total > 1,] %>% 
  .[kg_peso > 1 & kg_peso != "Inf", ] %>% 
  .[kg_payload > 1 & kg_payload != "Inf", ] %>% 
  .[,`:=`(sum_kg_peso = sum(kg_peso),
          sum_kg_payload = sum(kg_payload)),by = dt_referencia] %>% 
  .[,.SD[1],by = dt_referencia] %>% 
  .[,load_factor := sum_kg_peso / sum_kg_payload] %>% 
  .[,load_factor_check := identical(kg_peso,kg_payload)] %>% 
  .[,date := as.Date(dt_referencia,"%Y-%m-%d")] %>% 
  .[,dia_mes := format(date,"%d/%m")] %>% 
  data.table::setkeyv(.,c("date")) %>% 
  .[, id_date := 1:.N, by = .(date)] %>% 
  .[data.table::between(date,pre_covid_period[1],
                        pre_covid_period[2]),
    time_phase := pre_covid_period_name] %>% 
  .[data.table::between(date,pos_covid_period[1],
                        pos_covid_period[2]),
    time_phase := pos_covid_period_name] %>% 
  .[,frollmean7_load_factor := frollmean(load_factor,n = 7, na.rm=TRUE),
  by = .(nr_ano_referencia)]


#
# how average_load factor varied
flight1 <- data.table::copy(flight_raw) %>% 
  .[nr_ano_referencia %in% c(2019,2020),] %>% 
  .[,kg_peso := as.numeric(kg_peso)] %>% 
  .[,kg_payload := as.numeric(kg_payload)] %>% 
  .[!is.na(nr_passag_total) & nr_passag_total > 1,] %>% 
  .[kg_peso > 1 & kg_peso != "Inf", ] %>% 
  .[kg_payload > 1 & kg_payload != "Inf", ] %>% 
  .[,date := as.Date(dt_referencia,"%Y-%m-%d")] %>% 
  .[,dia_mes := format(date,"%d/%m")] %>% 
  data.table::setkeyv(.,c("date")) %>% 
  .[data.table::between(date,pre_covid_period[1],
                        pre_covid_period[2]),
    time_phase := pre_covid_period_name] %>% 
  .[data.table::between(date,pos_covid_period[1],
                        pos_covid_period[2]),
    time_phase := pos_covid_period_name] %>% 
  .[!is.na(time_phase),] %>% 
  .[,`:=`(sum_kg_peso = sum(kg_peso),
          sum_kg_payload = sum(kg_payload)),by = time_phase] %>% 
  .[,.SD[1],by = time_phase] %>% 
  .[,load_factor := sum_kg_peso / sum_kg_payload] %>% 
  .[,load_factor_check := identical(kg_peso,kg_payload)] 

flight1[,.(time_phase,load_factor )]

# graph --------

month_name_aux <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan")
dia_mes_aux <- c(paste0("01/0",1:9),paste0("01/",10:12),"31/12")
for(i in seq_along(dia_mes_aux)) flight[dia_mes %like% dia_mes_aux[i],month_name := month_name_aux[i]]

id_breaks <- which(!is.na(flight[nr_ano_referencia %in% 2019,]$month_name))

ggplot(flight)+
  geom_point(aes(x = id_date,y = load_factor
                 ,color = as.factor(nr_ano_referencia))
             , shape = 21)+
  theme_bw() + 
  theme(legend.position = c(0.90,0.20),
        panel.grid.minor = element_blank()) +
  geom_line(aes(x = id_date,
                y = frollmean7_load_factor, 
                color = as.factor(nr_ano_referencia)), 
            size = 1.05,
            linetype = 1) +
  scale_color_manual(values = c("gray72","dodgerblue1")) + 
  # geom_vline(xintercept = my_xintercept, color='red', linetype="dashed") +
  coord_cartesian(xlim = c(min(flight$id_date),max(flight$id_date)),
                  expand = FALSE) + 
  geom_point(data = data.frame(x = 1,y=0),
             aes(x = x,y = y ),color = 'white',size = 0.01) + 
  scale_x_continuous(breaks = id_breaks,
                     labels = flight$month_name[id_breaks]) +
  labs(x = "Month", y = "Load factor [-]", color = NULL) +
  theme(strip.background = element_rect(color= 'black', fill = 'white'),
        axis.text.x = element_text(size = rel(1),angle = 0))



