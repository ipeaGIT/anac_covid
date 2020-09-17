library(CausalImpact)



############ travel demand -----------------------------
df2019 <- subset(t, year==2019 & international == 'National')
df2020 <- subset(t, year==2020 & international == 'National')


ggplot() + 
  # geom_smooth(data= df2019, aes(x=xx, y=total_pass), color='gray50', size=1, fill='gray70') +
   geom_point(data= df2019, aes(x=xx, y=total_pass),  color='gray50', size=1, fill='gray70') +
 # geom_point( data= df2020, aes(x=xx, y=total_pass), color='blue', alpha=.4, size=1) +
 # geom_smooth( data= df2020, aes(x=xx, y=total_pass),color='blue',  alpha=.4, size=1) +
 # geom_vline(aes(xintercept = as.Date('2020-03-10')), size = .25, linetype = 'dashed') +
  theme_minimal()

summary(df2019$xx)
summary(df2020$xx)

date_missing <- setdiff(df2020$xx, df2019$xx)
as.Date(date_missing)

# harmonize dates
df2019 <- subset(df2019, xx != as.Date(date_missing))
df2020 <- subset(df2020, xx != as.Date(date_missing))

pre.period <- c(as.Date('2020-01-01'), as.Date('2020-03-01'))
pos.period <- c(as.Date('2020-03-02'), as.Date('2020-07-31'))

as.Date('2020-01-01'):as.Date('2020-03-10') %>% length()
as.Date('2020-03-11'):as.Date('2020-07-31') %>% length()
min(df2020$xx):max(df2020$xx) %>% length()
min(df2019$xx):max(df2019$xx) %>% length()

pre.period <- c(1,69)
pos.period <- c(70, 212)



df <- cbind(df2020$total_pass, df2019$total_pass)


impact <- CausalImpact(df,
                       pre.period,
                       pos.period)

plot(impact)

print(impact)
summary(impact, 'report')






############ travel emissions -----------------------------
df2019 <- subset(t, year==2019 & international == 'National')
df2020 <- subset(t, year==2020 & international == 'National')


ggplot() + 
  # geom_smooth(data= df2019, aes(x=xx, y=total_pass), color='gray50', size=1, fill='gray70') +
  geom_point(data= df2019, aes(x=xx, y=total_pass),  color='gray50', size=1, fill='gray70') +
  # geom_point( data= df2020, aes(x=xx, y=total_pass), color='blue', alpha=.4, size=1) +
  # geom_smooth( data= df2020, aes(x=xx, y=total_pass),color='blue',  alpha=.4, size=1) +
  # geom_vline(aes(xintercept = as.Date('2020-03-10')), size = .25, linetype = 'dashed') +
  theme_minimal()

summary(df2019$xx)
summary(df2020$xx)

date_missing <- setdiff(df2020$xx, df2019$xx)
as.Date(date_missing)

# harmonize dates
df2019 <- subset(df2019, xx != as.Date(date_missing))
df2020 <- subset(df2020, xx != as.Date(date_missing))

pre.period <- c(as.Date('2020-01-01'), as.Date('2020-03-01'))
pos.period <- c(as.Date('2020-03-02'), as.Date('2020-07-31'))

as.Date('2020-01-01'):as.Date('2020-03-10') %>% length()
as.Date('2020-03-11'):as.Date('2020-07-31') %>% length()
min(df2020$xx):max(df2020$xx) %>% length()
min(df2019$xx):max(df2019$xx) %>% length()

pre.period <- c(1,69)
pos.period <- c(70, 212)



df <- cbind(df2020$total_pass, df2019$total_pass)


impact <- CausalImpact(df,
                       pre.period,
                       pos.period)

plot(impact)

print(impact)
summary(impact, 'report')