library(CausalImpact)
library(data.table)
library(ggplot2)
library(zoo)
library(patchwork)
library(magrittr)

`%nin%` <- Negate(`%in%`)





############ read data -----------------------------
setwd("L:/Proj_acess_oport/git_rafa/anac_covid")
# Read Causal Impact model output
impact_pass <- readr::read_rds("./outputs/impact_output_passengers.rds")
impact_emis <- readr::read_rds("./outputs/impact_output_emissions.rds")


# get data ready for plot
series_pass <- CausalImpact:::CreateDataFrameForPlot(impact_pass)
series_emis <- CausalImpact:::CreateDataFrameForPlot(impact_emis)
head(series_pass)
head(series_emis)
tail(series_emis)




my_x_breaks <- c(paste0("2020-0",seq(2,9,by = 2),"-01"),
                 paste0("2020-",seq(10,12,by = 2),"-01")) %>% as.Date()
my_x_labels <- c("fev","apr","jun","aug","oct","dec")


############ plot travel demand -----------------------------

plot_impact_passA <- 
  
  # filter
  series_pass %>%
  subset(., time <= as.Date("2020-12-31")) %>%
  subset(., metric == 'original') %>%

  # plot
  ggplot( aes(x = time)) + 
  
  # Add zero line to pointwise and cumulative plot
  geom_line(aes(y = baseline/1000), colour = "darkgrey", size = 0.8, linetype = "solid", na.rm = TRUE) +
  
  # Add prediction intervals
  geom_ribbon(aes(ymin = lower/1000, ymax = upper/1000),
              fill = "slategray2") +
  
  # Add point predictions
  geom_line(aes(y = mean/1000), 
            size = 0.6, colour = "darkblue", linetype = "dashed",
            na.rm = TRUE) +
  
  # Add observed data
  geom_line(aes(y = response/1000), size = 0.6,  na.rm = TRUE) +
  
  # details
  facet_grid(. ~ metric, scales = "free_y")  +
  theme_bw(base_size = 12) + xlab("") + ylab("Passengers (thousands)") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  #scale_x_date(date_breaks='3 months', date_labels = "%b", expand = c(0, 0)) +
  scale_x_continuous(breaks = my_x_breaks,labels = my_x_labels, expand = c(0, 0)) +
  geom_vline(xintercept = as.Date('2020-03-12'),
             colour = "darkgrey", size = 0.8, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank())


plot_impact_passA




plot_impact_passB <- 
  
  # filter
  series_pass %>%
  subset(., time <= as.Date("2020-12-31")) %>%
  subset(., metric == 'pointwise') %>%
  
  # plot
  ggplot( aes(x = time)) + 
  
  # Add zero line to pointwise and cumulative plot
  geom_line(aes(y = baseline/1000), colour = "darkgrey", size = 0.8, linetype = "solid", na.rm = TRUE) +
  
  # Add prediction intervals
  geom_ribbon(aes(ymin = lower/1000, ymax = upper/1000),
              fill = "slategray2") +
  
  # Add point predictions
  geom_line(aes(y = mean/1000), 
            size = 0.6, colour = "darkblue", linetype = "dashed",
            na.rm = TRUE) +
  
  # Add observed data
  geom_line(aes(y = response/1000), size = 0.6,  na.rm = TRUE) +
  
  # details
  facet_grid(. ~ metric, scales = "free_y")  +
  theme_bw(base_size = 12) + xlab("") + ylab("Difference on passengers\n(thousands)") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  #scale_x_date(date_breaks='3 months', date_labels = "%b", expand = c(0, 0)) +
  scale_x_continuous(breaks = my_x_breaks,labels = my_x_labels, expand = c(0, 0)) +
  geom_vline(xintercept = as.Date('2020-03-12'),
             colour = "darkgrey", size = 0.8, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank())


plot_impact_passB


############ plot travel emissions -----------------------------


plot_impact_emisC <- 
  
  # filter
  series_emis %>%
  subset(., time <= as.Date("2020-12-31")) %>%
  subset(., metric != 'cumulative') %>%
  subset(., metric == 'original') %>%
  
  # plot
  ggplot( aes(x = time)) + 
  
  # Add zero line to pointwise and cumulative plot
  geom_line(aes(y = baseline/1000), colour = "darkgrey", size = 0.8, linetype = "solid", na.rm = TRUE) +
  
  # Add prediction intervals
  geom_ribbon(aes(ymin = lower/1000, ymax = upper/1000),
              fill = "#fd8d3c", alpha=.9) +
  
  # Add point predictions
  geom_line(aes(y = mean/1000), 
            size = 0.6, colour = "darkblue", linetype = "dashed",
            na.rm = TRUE) +
  
  # Add observed data
  geom_line(aes(y = response/1000), size = 0.6,  na.rm = TRUE) +
  
  # details
  facet_grid(metric ~ ., scales = "free_y")  +
  theme_bw(base_size = 12) + xlab("") + ylab( bquote('CO'[2] ~ 'tons (thousands)')) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
 # scale_x_date(date_breaks='2 months', date_labels = "%b", expand = c(0, 0)) +
  scale_x_continuous(breaks = my_x_breaks,labels = my_x_labels, expand = c(0, 0)) +
  geom_vline(xintercept = as.Date('2020-03-12'),
             colour = "darkgrey", size = 0.8, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank())


plot_impact_emisC


plot_impact_emisD <-  # filter
  series_emis %>%
  subset(., time <= as.Date("2020-12-31")) %>%
  subset(., metric != 'cumulative') %>%
  subset(., metric == 'pointwise') %>%
  
  # plot
  ggplot( aes(x = time)) + 
  
  # Add zero line to pointwise and cumulative plot
  geom_line(aes(y = baseline/1000), colour = "darkgrey", size = 0.8, linetype = "solid", na.rm = TRUE) +
  
  # Add prediction intervals
  geom_ribbon(aes(ymin = lower/1000, ymax = upper/1000),
              fill = "#fd8d3c", alpha=.9) +
  
  # Add point predictions
  geom_line(aes(y = mean/1000), 
            size = 0.6, colour = "darkblue", linetype = "dashed",
            na.rm = TRUE) +
  
  # Add observed data
  geom_line(aes(y = response/1000), size = 0.6,  na.rm = TRUE) +
  
  # details
  facet_grid(metric ~ ., scales = "free_y")  +
  theme_bw(base_size = 12) + xlab("") +
  #ylab( bquote('Difference on CO'[2] ~ 'tons'~'\n(thousands)')) +
  ylab(expression(atop("",atop(textstyle('Difference on CO'[2]~ 'tons'),
                                      atop(textstyle("(thousands)")))))) + 
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_x_continuous(breaks = my_x_breaks,labels = my_x_labels, expand = c(0, 0)) +
  #scale_x_date(date_breaks='3 months', date_labels = "%b") +
  geom_vline(xintercept = as.Date('2020-03-12'),
             colour = "darkgrey", size = 0.8, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank())


plot_impact_emisD

break()
############ save plots  -----------------------------

plot <- 
  wrap_plots(plot_impact_passA , 
             plot_impact_passB , 
             plot_impact_emisC , 
             plot_impact_emisD) +
  plot_annotation(tag_levels = 'A')

plot
setwd("L:/Proj_acess_oport/git_jbazzo/anac_covid")
ggsave(plot, filename = './figures/figure2_impact.png', dpi=300,
       width = 26, height = 15, units = 'cm')
ggsave(plot, filename = './figures/figure2_impact.pdf', dpi=300,
       width = 26, height = 15, units = 'cm')





############ numeric passengers -----------------------------

### passengers
print(impact_pass)
summary(impact_pass, 'report')


# total (cumulative) that happened with pandemic
total_obs <- series_pass %>%
                  subset(., time <= as.Date("2020-12-31")) %>%
                  subset(., time >= as.Date("2020-03-12")) %>%
                  subset(., metric == 'original') %>% # original pointwise cumulative
                  .$response %>% # response
                  sum()

# total (cumulative) counterfactual IF pandemic had not happened
total_exp <- series_pass %>%
                  subset(., time <= as.Date("2020-12-31")) %>%
                  subset(., time >= as.Date("2020-03-12")) %>%
                  subset(., metric == 'original') %>% # original pointwise cumulative
                  .$mean %>% # mean
                  sum()

# abs change
total_exp - total_obs

# relative change
(total_exp - total_obs) / total_exp




# daily that happened with pandemic
daily_obs <- series_pass %>%
  subset(., time <= as.Date("2020-12-31")) %>%
  subset(., time >= as.Date("2020-03-12")) %>%
  subset(., metric == 'original') %>% # original pointwise cumulative
  .$response %>% # response
  mean()

# daily counterfactual IF pandemic had not happened
daily_exp <- series_pass %>%
  subset(., time <= as.Date("2020-12-31")) %>%
  subset(., time >= as.Date("2020-03-12")) %>%
  subset(., metric == 'original') %>% # original pointwise cumulative
  .$mean %>% # mean
  mean(na.rm=T)

# abs change
daily_exp - daily_obs

# relative change
(daily_exp - daily_obs) / daily_exp






############ numeric emis -----------------------
print(impact_emis)
summary(impact_emis, 'report')

head(series_emis)
table(series_emis$metric)


# 2020 Total emis in counterfactual without pandemic
subset(series_emis, metric=='original')$lower %>% sum()  # lower bound estimate
subset(series_emis, metric=='original')$mean %>% sum()   # mean estimate
subset(series_emis, metric=='original')$upper  %>% sum() # upper bound estimate

# 2020 Total emis observed with pandemic
subset(series_emis, metric=='original')$response %>% sum()


### only in the period with Pandemic 
  
  # total (cumulative) that happened with pandemic
  total_obs <- series_emis %>%
    subset(., time <= as.Date("2020-12-31")) %>%
    subset(., time >= as.Date("2020-03-12")) %>%
    subset(., metric == 'original') %>% # original pointwise cumulative
    .$response %>% # response
    sum()
  
  # total (cumulative) counterfactual without pandemic
  total_counter <- series_emis %>%
    subset(., time <= as.Date("2020-12-31")) %>%
    subset(., time >= as.Date("2020-03-12")) %>%
    subset(., metric == 'original') %>% # original pointwise cumulative
    .$mean %>% # mean
    sum()
  
  # daily that happened with pandemic
  daily_obs <- series_emis %>%
    subset(., time <= as.Date("2020-12-31")) %>%
    subset(., time >= as.Date("2020-03-12")) %>%
    subset(., metric == 'original') %>% # original pointwise cumulative
    .$response %>% # response
    mean()
  
  # daily counterfactual IF pandemic had not happened
  daily_exp <- series_emis %>%
    subset(., time < as.Date("2020-11-30")) %>%
    subset(., time > as.Date("2020-03-13")) %>%
    subset(., metric == 'original') %>% # original pointwise cumulative
    .$mean %>% # mean
    mean(na.rm=T)

# abs change
daily_exp - daily_obs

# relative change
(daily_exp - daily_obs) / daily_exp





# 1st 7 months (comparacao com Liu et al)
# blue line
counter <- series_emis %>%
              subset(., time < as.Date("2020-07-31")) %>%
              subset(., time > as.Date("2020-01-01")) %>%
              subset(., metric != 'cumulative') %>%
              subset(., metric == 'original') %>%
              .$mean %>% # mean response
              sum(na.rm=T)
# gray line
observed <- series_emis %>%
  subset(., time < as.Date("2020-07-31")) %>%
  subset(., time > as.Date("2020-01-01")) %>%
  subset(., metric != 'cumulative') %>%
  subset(., metric == 'original') %>%
  .$response %>% # mean response
  sum(na.rm=T)

counter - observed
