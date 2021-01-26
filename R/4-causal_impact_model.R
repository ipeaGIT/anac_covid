library(CausalImpact)
library(data.table)
library(ggplot2)
library(zoo)

`%nin%` <- Negate(`%in%`)




############ travel demand -----------------------------

# read travel demand data
 # t <- read_rds("./outputs/impact_input_passengers.rds")
 t <- readr::read_rds("./outputs/impact_input_emissions.rds")
 head(t)
 
 # put all dates in 2020
 t[, xx := paste0("2020-", format(dt_referencia, "%m-%d"))]
 t[, xx := lubridate::as_date(xx, format="%Y-%m-%d") ]
 t[, month := lubridate::month(dt_referencia) ]
 

# sort data set by date
t <- t[order(nr_ano_referencia, xx)]

# separate years
df2017 <- subset(t, nr_ano_referencia==2017)
df2018 <- subset(t, nr_ano_referencia==2018)
df2019 <- subset(t, nr_ano_referencia==2019)
df2020 <- subset(t, nr_ano_referencia==2020)


df2020[ month==12, sum(nr_passag_total)] /
  df2019[ month==12, sum(nr_passag_total)]

sum(df2020$nr_passag_total) # 46,587,063
sum(df2019$nr_passag_total) # 98,237,874

summary(df2020$nr_passag_total) # mean: 127,287
summary(df2019$nr_passag_total) # mean: 269,145

# quick inspection plot
ggplot() + 
  # geom_smooth(data= df2019, aes(x=xx, y=total_pass), color='gray50', size=1, fill='gray70') +
  geom_smooth(data= df2017, aes(x=xx, y=nr_passag_total),  color='gray60', size=1, fill='gray70') +
  geom_smooth(data= df2018, aes(x=xx, y=nr_passag_total),  color='gray40', size=1, fill='gray70') +
  geom_smooth(data= df2019, aes(x=xx, y=nr_passag_total),  color='gray20', size=1, fill='gray70') +
  geom_point( data= df2020, aes(x=xx, y=nr_passag_total), color='blue', alpha=.4, size=1) +
  # geom_smooth( data= df2020, aes(x=xx, y=nr_passag_total),color='blue',  alpha=.4, size=1) +
  geom_vline(aes(xintercept = as.Date('2020-03-12')), size = .25, linetype = 'dashed') +
  scale_x_date(date_breaks='4 weeks', date_labels = "%b/%d") +
  theme_minimal()


# set period
time.points <- seq.Date(from=as.Date("2020-01-01"),
                        to=as.Date('2020-12-31'),
                        by = 1)

# harmonize dates (2020 is a leap year), remove Feb 29th
df2020 <- subset(df2020, xx != as.Date("2020-02-29"))
time.points <- time.points[time.points %nin% as.Date("2020-02-29")]

# set pre and post period
pre.period <- c(as.Date('2020-01-01'), as.Date('2020-03-11'))
pos.period <- c(as.Date('2020-03-12'), as.Date('2020-12-31'))


# prepare model input
df <- zoo::zoo( cbind(df2020$nr_passag_total, 
                      df2019$nr_passag_total, 
                      df2018$nr_passag_total, 
                      df2017$nr_passag_total),
                time.points)

head(df)

# Causal Impact model
impact_pass <- CausalImpact(df,
                       pre.period,
                       pos.period)


# results
print(impact_pass)
summary(impact_pass, 'report')

# inspection plot
plot(impact_pass)
  

# save Causal Impact model output
readr::write_rds(impact_pass, "./outputs/impact_output_passengers.rds", compress = 'gz')






############ travel emissions -----------------------------


# read travel emission data
e <- readr::read_rds("./outputs/impact_input_emissions.rds")


head(e)

head(e)

summary(e$dt_referencia)


# create common date ignoring year
e[, emi_co2 :=  as.numeric(emi_co2) ]
e[, xx := format(dt_referencia, "%m-%d")]
e[, xx := lubridate::as_date(xx, format="%m-%d") ]




# sort data set by date
e <- e[order(nr_ano_referencia, xx)]
head(e)

# subset national flights in separate years
df2017 <- subset(e, nr_ano_referencia==2017 )
df2018 <- subset(e, nr_ano_referencia==2018 )
df2019 <- subset(e, nr_ano_referencia==2019 )
df2020 <- subset(e, nr_ano_referencia==2020 )

sum(df2018$emi_co2)

summary(df2017$dt_referencia)
summary(df2018$dt_referencia)
summary(df2019$dt_referencia)
summary(df2020$dt_referencia)


# quick inspection plot
ggplot() +
  # geom_smooth(data= df2019, aes(x=xx, y=total_pass), color='gray50', size=1, fill='gray70') +
  geom_smooth(data= df2017, aes(x=xx, y=emi_co2),  color='gray60', size=1, fill='gray70') +
  geom_smooth(data= df2018, aes(x=xx, y=emi_co2),  color='gray40', size=1, fill='gray70') +
  geom_smooth(data= df2019, aes(x=xx, y=emi_co2),  color='gray20', size=1, fill='gray70') +
  geom_point( data= df2020, aes(x=xx, y=emi_co2), color='blue', alpha=.4, size=1) +
  # geom_smooth( data= df2020, aes(x=xx, y=emi_co2),color='blue',  alpha=.4, size=1) +
  geom_vline(aes(xintercept = as.Date('2020-03-10')), size = .25, linetype = 'dashed') +
  theme_minimal()



## harmonize dates , (2020 ia a leap year)
df2020 <- subset(df2020, xx != as.Date("2020-02-29"))


# prepare model input
df <- zoo( cbind(df2020$emi_co2, df2019$emi_co2, df2018$emi_co2, df2017$emi_co2),
           time.points)

# Causal Impact model
impact_emis <- CausalImpact(df,
                       pre.period,
                       pos.period)



print(impact_emis)
summary(impact_emis, 'report')


# inspection plot
plot(impact_emis)


# save Causal Impact model output
readr::write_rds(impact_emis, "./outputs/impact_output_emissions.rds", compress = 'gz')

