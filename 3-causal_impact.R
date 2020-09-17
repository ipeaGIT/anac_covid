library(CausalImpact)


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
