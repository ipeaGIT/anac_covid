library(CausalImpact)
library(data.table)
library(ggplot2)
library(zoo)

`%nin%` <- Negate(`%in%`)


############ prepare data -----------------------------


# Read Causal Impact model output
impact_pass <- readr::read_rds("./outputs/impact_output_passengers.csv")
impact_emis <- readr::read_rds("./outputs/impact_output_emissions.csv")

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
  
  subset(series_pass, time < as.Date("2020-07-31")) %>%
  
  ggplot( aes(x = time)) + 
  
  # Add zero line to pointwise and cumulative plot
  geom_line(aes(y = baseline), colour = "darkgrey", size = 0.8, linetype = "solid", na.rm = TRUE) +
  
  # Add prediction intervals
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "slategray2") +
  
  # Add point predictions
  geom_line(aes(y = mean), 
            size = 0.6, colour = "darkblue", linetype = "dashed",
            na.rm = TRUE) +
  
  # Add observed data
  geom_line(aes(y = response), size = 0.6,  na.rm = TRUE) +
  
  # details
  facet_grid(metric ~ ., scales = "free_y")  +
  theme_bw(base_size = 15) + xlab("") + ylab("") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_x_date(date_breaks='4 weeks', date_labels = "%b/%d") +
  geom_vline(xintercept = as.Date('2020-03-15'),
             colour = "darkgrey", size = 0.8, linetype = "dashed")


plot_impact_pass




############ travel emissions -----------------------------


plot_impact_emis <- 
  
  subset(series_emis, time < as.Date("2020-07-31")) %>%
  
  ggplot( aes(x = time)) + 
  
  # Add zero line to pointwise and cumulative plot
  geom_line(aes(y = baseline), colour = "darkgrey", size = 0.8, linetype = "solid", na.rm = TRUE) +
  
  # Add prediction intervals
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "slategray2") +
  
  # Add point predictions
  geom_line(aes(y = mean), 
            size = 0.6, colour = "darkblue", linetype = "dashed",
            na.rm = TRUE) +
  
  # Add observed data
  geom_line(aes(y = response), size = 0.6,  na.rm = TRUE) +
  
  # details
  facet_grid(metric ~ ., scales = "free_y")  +
  theme_bw(base_size = 15) + xlab("") + ylab("") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_x_date(date_breaks='4 weeks', date_labels = "%b/%d") +
  geom_vline(xintercept = as.Date('2020-03-15'),
             colour = "darkgrey", size = 0.8, linetype = "dashed")


plot_impact_emis

