library(CausalImpact)
library(data.table)
library(ggplot2)
library(zoo)
library(patchwork)

`%nin%` <- Negate(`%in%`)


############ prepare data -----------------------------


# Read Causal Impact model output
impact_pass <- readr::read_rds("./outputs/impact_output_passengers.rds")
impact_emis <- readr::read_rds("./outputs/impact_output_emissions.rds")

# results
print(impact_pass)
print(impact_emis)


# get data ready for plot
series_pass <- CausalImpact:::CreateDataFrameForPlot(impact_pass)
series_emis <- CausalImpact:::CreateDataFrameForPlot(impact_emis)
head(series_pass)
head(series_emis)



############ travel demand -----------------------------

plot_impact_pass <- 
  
  # filter
  series_pass %>%
  subset(., time < as.Date("2020-11-30")) %>%
  subset(., metric != 'cumulative') %>%
  # transform(., metric = c('A', 'B')) %>%

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
  facet_grid(metric ~ ., scales = "free_y")  +
  theme_bw(base_size = 15) + xlab("") + ylab("Number of passengers (in thousands)") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_x_date(date_breaks='1 month', date_labels = "%b") +
  geom_vline(xintercept = as.Date('2020-03-15'),
             colour = "darkgrey", size = 0.8, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


plot_impact_pass




############ travel emissions -----------------------------


plot_impact_emis <- 
  
  # filter
  series_emis %>%
  subset(., time < as.Date("2020-11-30")) %>%
  subset(., metric != 'cumulative') %>%
  # transform(., metric = c('A', 'B')) %>%
  
  # plot
  ggplot( aes(x = time)) + 
  
  # Add zero line to pointwise and cumulative plot
  geom_line(aes(y = baseline/1000), colour = "darkgrey", size = 0.8, linetype = "solid", na.rm = TRUE) +
  
  # Add prediction intervals
  geom_ribbon(aes(ymin = lower/1000, ymax = upper/1000),
              fill = "purple4", alpha=.5) +
  
  # Add point predictions
  geom_line(aes(y = mean/1000), 
            size = 0.6, colour = "darkblue", linetype = "dashed",
            na.rm = TRUE) +
  
  # Add observed data
  geom_line(aes(y = response/1000), size = 0.6,  na.rm = TRUE) +
  
  # details
  facet_grid(metric ~ ., scales = "free_y")  +
  theme_bw(base_size = 15) + xlab("") + ylab( bquote('CO'^2 ~ 'tons')) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_x_date(date_breaks='1 month', date_labels = "%b") +
  geom_vline(xintercept = as.Date('2020-03-15'),
             colour = "darkgrey", size = 0.8, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


plot_impact_emis



plot_impact_pass + plot_impact_emis



