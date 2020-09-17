library(CausalImpact)

summary(df_quintile$dt)

# pre.period <- c(as.Date('2020-03-01'), as.Date('2020-03-13'))
# pos.period <- c(as.Date('2020-03-13'), as.Date('2020-08-10'))
pre.period <- c(1,13)
pos.period <- c(14, 151)


df_quintile <- df_quintile[order(quintile, dt)]
df1 <- subset(df_quintile, quintile==1)$distavg
df5 <- subset(df_quintile, quintile==5)$distavg
df <- cbind(df5, df1)



impact <- CausalImpact(df,
                       pre.period,
                       pos.period)

plot(impact)

print(impact)
summary(impact, 'report')