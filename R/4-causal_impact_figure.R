library(CausalImpact)
library(data.table)
library(ggplot2)
library(zoo)
library(patchwork)

`%nin%` <- Negate(`%in%`)





############ read data -----------------------------

# Read Causal Impact model output
impact_pass <- readr::read_rds("./outputs/impact_output_passengers.rds")
impact_emis <- readr::read_rds("./outputs/impact_output_emissions.rds")


# get data ready for plot
series_pass <- CausalImpact:::CreateDataFrameForPlot(impact_pass)
series_emis <- CausalImpact:::CreateDataFrameForPlot(impact_emis)
head(series_pass)
head(series_emis)
tail(series_emis)


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
  scale_x_date(date_breaks='3 months', date_labels = "%b") +
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
  theme_bw(base_size = 12) + xlab("") + ylab("Avoided passengers\n(thousands)") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_x_date(date_breaks='3 months', date_labels = "%b") +
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
  scale_x_date(date_breaks='3 months', date_labels = "%b") +
  geom_vline(xintercept = as.Date('2020-03-12'),
             colour = "darkgrey", size = 0.8, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank())


plot_impact_emisC





plot_impact_emisD <- 
  
  # filter
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
  theme_bw(base_size = 12) + xlab("") + ylab( bquote('Avoided CO'[2] ~ 'tons'~'(thousands)')) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_x_date(date_breaks='1 months', date_labels = "%b") +
  geom_vline(xintercept = as.Date('2020-03-12'),
             colour = "darkgrey", size = 0.8, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank())


plot_impact_emisD


############ save plots  -----------------------------

plot <- 
  wrap_plots(plot_impact_passA , 
             plot_impact_passB , 
             plot_impact_emisC , 
             plot_impact_emisD) +
  plot_annotation(tag_levels = 'A')

plot

ggsave(plot, filename = './figures/figure2_impact.png', dpi=300,
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
                  .$response %>% # mean response
                  sum()

# total (cumulative) counterfactual IF pandemic had not happened
total_exp <- series_pass %>%
                  subset(., time <= as.Date("2020-12-31")) %>%
                  subset(., time >= as.Date("2020-03-12")) %>%
                  subset(., metric == 'original') %>% # original pointwise cumulative
                  .$mean %>% # mean response
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
  .$response %>% # mean response
  mean()

# daily counterfactual IF pandemic had not happened
daily_exp <- series_pass %>%
  subset(., time <= as.Date("2020-12-31")) %>%
  subset(., time >= as.Date("2020-03-12")) %>%
  subset(., metric == 'original') %>% # original pointwise cumulative
  .$mean %>% # mean response
  mean(na.rm=T)

# abs change
daily_exp - daily_obs

# relative change
(daily_exp - daily_obs) / daily_exp






############ numeric emis -----------------------
print(impact_emis)
summary(impact_emis, 'report')

7541343 /2882909
2882909/7541343

# total (cumulative) that happened with pandemic
total_obs <- series_emis %>%
  subset(., time <= as.Date("2020-12-31")) %>%
  subset(., time >= as.Date("2020-03-12")) %>%
  subset(., metric == 'original') %>% # original pointwise cumulative
  .$response %>% # mean response
  sum()

# daily that happened with pandemic
daily_obs <- series_emis %>%
  subset(., time <= as.Date("2020-12-31")) %>%
  subset(., time >= as.Date("2020-03-12")) %>%
  subset(., metric == 'original') %>% # original pointwise cumulative
  .$response %>% # mean response
  mean()

# daily counterfactual IF pandemic had not happened
daily_exp <- series_emis %>%
  subset(., time < as.Date("2020-11-30")) %>%
  subset(., time > as.Date("2020-03-13")) %>%
  subset(., metric == 'original') %>% # original pointwise cumulative
  .$mean %>% # mean response
  mean(na.rm=T)

# abs change
daily_exp - daily_obs

# relative change
(daily_exp - daily_obs) / daily_exp
