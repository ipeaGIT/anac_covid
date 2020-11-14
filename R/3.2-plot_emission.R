#
# "Taxi in/out times" --------
#


ttx <- data.table::copy(tt) %>% as.data.frame() %>% tibble::as_tibble() 
ttx$emi_co2 <- as.numeric(ttx$emi_co2)
ttx$lt_combustivel <- as.numeric(ttx$lt_combustivel)

source("http://blog.revolutionanalytics.com/downloads/calendarHeat.R")
calendarHeat(dates=ttx$date, values=ttx$emi_co2, varname="emi_co2")
p0 <- openair::calendarPlot(mydata = ttx, pollutant = 'emi_co2', year = 2019,
                            #breaks = seq(0,max(tt$emi_co2),by = 2000),
                            main = "Emissões de CO2 em 2019",
                            key.header = "Total (t)",key.position = "right")
p0 <- openair::calendarPlot(mydata = ttx, pollutant = 'emi_co2', year = 2020,
                            #breaks = seq(0,max(tt$emi_co2),by = 2000),
                            main = "Emissões de CO2 em 2020",
                            key.header = "Total (t)",key.position = "right")
lond <- importTraj("london", 2010)
# well, HYSPLIT seems to think there certainly were conditions where trajectories
# orginated from Iceland...
trajPlot(selectByDate(lond, start = "15/4/2010", end = "21/4/2010"))




times_anac[operating_mode %in% 'Taxi-in',],
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
