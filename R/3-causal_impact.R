library(CausalImpact)
library(data.table)
library(ggplot2)

`%nin%` <- Negate(`%in%`)

############ travel demand -----------------------------
t <- t[order(year, xx)]
  
df2017 <- subset(t, year==2017 & international == 'National')
df2018 <- subset(t, year==2018 & international == 'National')
df2019 <- subset(t, year==2019 & international == 'National')
df2020 <- subset(t, year==2020 & international == 'National')


ggplot() + 
  # geom_smooth(data= df2019, aes(x=xx, y=total_pass), color='gray50', size=1, fill='gray70') +
  geom_smooth(data= df2017, aes(x=xx, y=total_pass),  color='gray60', size=1, fill='gray70') +
  geom_smooth(data= df2018, aes(x=xx, y=total_pass),  color='gray40', size=1, fill='gray70') +
  geom_smooth(data= df2019, aes(x=xx, y=total_pass),  color='gray20', size=1, fill='gray70') +
  # geom_point( data= df2020, aes(x=xx, y=total_pass), color='blue', alpha=.4, size=1) +
   geom_smooth( data= df2020, aes(x=xx, y=total_pass),color='blue',  alpha=.4, size=1) +
   geom_vline(aes(xintercept = as.Date('2020-03-10')), size = .25, linetype = 'dashed') +
  theme_minimal()

summary(df2019$xx)
summary(df2020$xx)

date_missing <- setdiff(df2020$xx, df2019$xx)
as.Date(date_missing)

# harmonize dates
df2020 <- subset(df2020, xx != as.Date(date_missing))

# set pre and post period
time.points <- seq.Date(from=as.Date("2020-01-01"),
                        to=as.Date('2020-07-31'),
                        by = 1)

pre.period <- c(as.Date('2020-01-01'), as.Date('2020-03-10'))
pos.period <- c(as.Date('2020-03-11'), as.Date('2020-07-31'))

df <- zoo( cbind(df2020$total_pass, df2019$total_pass, df2018$total_pass, df2017$total_pass),
     time.points)

impact <- CausalImpact(df,
                       pre.period,
                       pos.period)


plot(impact)

print(impact)
summary(impact, 'report')

series <- impact$series








############ travel emissions -----------------------------
t <- readr::read_rds('L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/anac_covid/emission_2017-2020.rds')
head(t)

t[, emi_co2 :=  as.numeric(emi_co2) ]



t[, xx := format(dt_referencia, "%m-%d")]
t[, xx := lubridate::as_date(xx, format="%m-%d") ]
t <- t[order(nr_ano_referencia, xx)]

df2017 <- subset(t, nr_ano_referencia==2017 )
df2018 <- subset(t, nr_ano_referencia==2018 )
df2019 <- subset(t, nr_ano_referencia==2019 )
df2020 <- subset(t, nr_ano_referencia==2020 )

ggplot() + 
  # geom_smooth(data= df2019, aes(x=xx, y=total_pass), color='gray50', size=1, fill='gray70') +
  geom_smooth(data= df2017, aes(x=xx, y=emi_co2),  color='gray60', size=1, fill='gray70') +
  geom_smooth(data= df2018, aes(x=xx, y=emi_co2),  color='gray40', size=1, fill='gray70') +
  geom_smooth(data= df2019, aes(x=xx, y=emi_co2),  color='gray20', size=1, fill='gray70') +
   geom_point( data= df2020, aes(x=xx, y=emi_co2), color='blue', alpha=.4, size=1) +
  geom_smooth( data= df2020, aes(x=xx, y=emi_co2),color='blue',  alpha=.4, size=1) +
  geom_vline(aes(xintercept = as.Date('2020-03-10')), size = .25, linetype = 'dashed') +
  theme_minimal()

summary(df2019$xx)
summary(df2020$xx)

# harmonize dates 1
date_missing <- setdiff(df2020$xx, df2019$xx)
as.Date(date_missing)
df2020 <- subset(df2020, xx != as.Date(date_missing))

# harmonize dates 2
date_missing <- setdiff(df2017$xx, df2020$xx)
as.Date(date_missing)
df2017 <- subset(df2017, xx %nin%  as.Date(date_missing))
df2018 <- subset(df2018, xx %nin% as.Date(date_missing))

summary(t$dt_referencia)
summary(df2020$dt_referencia)

# set pre and post period
time.points <- seq.Date(from= as.Date(min(df2020$dt_referencia)),
                        to=as.Date(max(df2020$dt_referencia)),
                        by = 1)

pre.period <- c(as.Date('2020-01-01'), as.Date('2020-03-10'))
pos.period <- c(as.Date('2020-03-11'), as.Date('2020-04-30'))

df <- zoo( cbind(df2020$emi_co2, df2019$emi_co2, df2018$emi_co2, df2017$emi_co2),
           time.points)

impact <- CausalImpact(df,
                       pre.period,
                       pos.period)


plot(impact)

print(impact)
summary(impact, 'report')

series <- impact$series
