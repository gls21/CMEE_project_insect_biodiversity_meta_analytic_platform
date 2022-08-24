# Script to test efficiency of models run using the shiny app

# Set working directory as testing_efficiency

# Compare number of data points in model to average time taken to run the model and output the results

library(dplyr)
library(ggplot2)

# Load data
model_time <- read.csv("time_to_run_models.csv")

# Calculate mean time and standard deviation for each group
mean_times <- model_time %>% 
  group_by(data_points) %>%
  summarise(across(.cols = time_seconds, list(mean = mean, sd = sd)))

# Calculate variance. Variance is standard deviation squared
mean_times$time_seconds_var <- (mean_times$time_seconds_sd)^2

# Calculate standard error of the mean - SEM = square root of variance divided by sample size (10 as 10 repeats)
mean_times$time_seconds_sem <- sqrt(mean_times$time_seconds_var/10)

# Mean plus SEM
mean_times$mean_plus_SEM <- mean_times$time_seconds_mean + mean_times$time_seconds_sem
# Mean minus SEM
mean_times$mean_minus_SEM <- mean_times$time_seconds_mean - mean_times$time_seconds_sem


# Plot 
dev.off()
mean_times %>%
  ggplot(aes(x = data_points, y = time_seconds_mean)) +
  geom_point(stat="identity", size = 1.5) +
  geom_errorbar(aes(ymin = mean_minus_SEM, ymax = mean_plus_SEM), width = 10) + # width of the errorbars compared to the points.
  labs(x = "Number of rows of data", y = "Model fitting time (seconds)") +
  scale_x_continuous(breaks = c(seq(0,700,100)), limits = c(0,700)) +
  scale_y_continuous(breaks = c(seq(0,25,5)), limits = c(0,25)) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=16),
        panel.grid.major = element_blank(), # no grid lines
        panel.grid.minor = element_blank())
ggsave("model_fitting_time_graph.png")
dev.off()











