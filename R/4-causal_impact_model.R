library(CausalImpact)
library(data.table)
library(ggplot2)
library(zoo)

`%nin%` <- Negate(`%in%`)




############ travel demand -----------------------------

# read travel demand data
t <- fread("./outputs/air_totalpass_fig3.csv")

# sort data set by date
t <- t[order(year, xx)]
  
# subset national flights in separate years
df2017 <- subset(t, year==2017 & international == 'National')
df2018 <- subset(t, year==2018 & international == 'National')
df2019 <- subset(t, year==2019 & international == 'National')
df2020 <- subset(t, year==2020 & international == 'National')



# quick inspection plot
ggplot() + 
  # geom_smooth(data= df2019, aes(x=xx, y=total_pass), color='gray50', size=1, fill='gray70') +
  geom_smooth(data= df2017, aes(x=xx, y=total_pass),  color='gray60', size=1, fill='gray70') +
  geom_smooth(data= df2018, aes(x=xx, y=total_pass),  color='gray40', size=1, fill='gray70') +
  geom_smooth(data= df2019, aes(x=xx, y=total_pass),  color='gray20', size=1, fill='gray70') +
  geom_point( data= df2020, aes(x=xx, y=total_pass), color='blue', alpha=.4, size=1) +
  geom_smooth( data= df2020, aes(x=xx, y=total_pass),color='blue',  alpha=.4, size=1) +
  geom_vline(aes(xintercept = as.Date('2020-03-15')), size = .25, linetype = 'dashed') +
  scale_x_date(date_breaks='3 weeks', date_labels = "%b/%d") +
  theme_minimal()


# harmonize dates (2020 is a leap year)
df2020 <- subset(df2020, xx != as.Date("2020-02-29"))

# set pre and post period
time.points <- seq.Date(from=as.Date("2020-01-01"),
                        to=as.Date('2020-07-31'),
                        by = 1)

pre.period <- c(as.Date('2020-01-01'), as.Date('2020-03-15'))
pos.period <- c(as.Date('2020-03-16'), as.Date('2020-07-31'))


# prepare model input
df <- zoo::zoo( cbind(df2020$total_pass, df2019$total_pass, df2018$total_pass, df2017$total_pass),
     time.points)



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
readr::write_rds(impact_pass, "./outputs/impact_output_passengers.csv")






############ travel emissions -----------------------------


# read travel emission data
e <- readr::read_rds('L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/anac_covid/emission_2017-2020.rds')
head(e)


# create common date ignoring year
e[, emi_co2 :=  as.numeric(emi_co2) ]
e[, xx := format(dt_referencia, "%m-%d")]
e[, xx := lubridate::as_date(xx, format="%m-%d") ]

# sort data set by date
e <- e[order(nr_ano_referencia, xx)]

# subset national flights in separate years
df2017 <- subset(e, nr_ano_referencia==2017 )
df2018 <- subset(e, nr_ano_referencia==2018 )
df2019 <- subset(e, nr_ano_referencia==2019 )
df2020 <- subset(e, nr_ano_referencia==2020 )

# quick inspection plot
ggplot() + 
  # geom_smooth(data= df2019, aes(x=xx, y=total_pass), color='gray50', size=1, fill='gray70') +
  geom_smooth(data= df2017, aes(x=xx, y=emi_co2),  color='gray60', size=1, fill='gray70') +
  geom_smooth(data= df2018, aes(x=xx, y=emi_co2),  color='gray40', size=1, fill='gray70') +
  geom_smooth(data= df2019, aes(x=xx, y=emi_co2),  color='gray20', size=1, fill='gray70') +
   geom_point( data= df2020, aes(x=xx, y=emi_co2), color='blue', alpha=.4, size=1) +
  geom_smooth( data= df2020, aes(x=xx, y=emi_co2),color='blue',  alpha=.4, size=1) +
  geom_vline(aes(xintercept = as.Date('2020-03-10')), size = .25, linetype = 'dashed') +
  theme_minimal()



# harmonize dates (2020 ia a leap year)
df2020 <- subset(df2020, xx != as.Date("2020-02-29"))


# harmonize dates 2
date_missing <- setdiff(df2017$xx, df2020$xx)
as.Date(date_missing)
df2017 <- subset(df2017, xx %nin%  as.Date(date_missing))
df2018 <- subset(df2018, xx %nin% as.Date(date_missing))



# set pre and post period
time.points <- seq.Date(from= as.Date(min(df2020$dt_referencia)),
                        to=as.Date(max(df2020$dt_referencia)),
                        by = 1)

pre.period <- c(as.Date('2020-01-01'), as.Date('2020-03-15'))
pos.period <- c(as.Date('2020-03-16'), as.Date('2020-04-30'))


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
readr::write_rds(impact_emis, "./outputs/impact_output_emissions.csv")

