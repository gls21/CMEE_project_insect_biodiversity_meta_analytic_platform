# Script to test efficiency of uploading data using the shiny app

# Set working directory to testing_efficiency

# Compare size of file to average time taken to upload (to the shiny app, to googlesheets, and combined)

library(dplyr)
library(ggplot2)

# Load data
upload_time <- read.csv("time_to_upload_data.csv")

# Calculate mean time and standard deviation for each group
mean_upload_times <- upload_time %>% 
  group_by(file_size_kb) %>%
  summarise(across(.cols = c(time_upload_app, time_upload_googlesheets), list(mean = mean, sd = sd)))

# Calculate variance. Variance is standard deviation squared
mean_upload_times$time_upload_app_var <- (mean_upload_times$time_upload_app_sd)^2
mean_upload_times$time_upload_googlesheets_var <- (mean_upload_times$time_upload_googlesheets_sd)^2

# Calculate standard error of the mean - SEM = square root of variance divided by sample size (10 as 10 repeats)
mean_upload_times$time_upload_app_sem <- sqrt(mean_upload_times$time_upload_app_var/10)
mean_upload_times$time_upload_googlesheets_sem <- sqrt(mean_upload_times$time_upload_googlesheets_var/10)

# Mean plus SEM
mean_upload_times$mean_upload_app_plus_SEM <- mean_upload_times$time_upload_app_mean + mean_upload_times$time_upload_app_sem
mean_upload_times$mean_upload_googlesheets_plus_SEM <- mean_upload_times$time_upload_googlesheets_mean + mean_upload_times$time_upload_googlesheets_sem
# Mean minus SEM
mean_upload_times$mean_upload_app_minus_SEM <- mean_upload_times$time_upload_app_mean - mean_upload_times$time_upload_app_sem
mean_upload_times$mean_upload_googlesheets_minus_SEM <- mean_upload_times$time_upload_googlesheets_mean - mean_upload_times$time_upload_googlesheets_sem


# Plot 
dev.off()
mean_upload_times %>%
  ggplot(aes(x = file_size_kb)) +
  geom_point(aes(y = time_upload_app_mean, colour = "Shiny app")) +
  geom_point(aes(y = time_upload_googlesheets_mean, colour = "Google Sheets")) +
  geom_errorbar(aes(ymin = mean_upload_app_minus_SEM, ymax = mean_upload_app_plus_SEM), width = 10, colour = "black") +
  geom_errorbar(aes(ymin = mean_upload_googlesheets_minus_SEM, ymax = mean_upload_googlesheets_plus_SEM), width = 10, colour = "blue") +
  scale_colour_manual("Upload to:", 
                      breaks = c("Shiny app", "Google Sheets"),
                      values = c("black", "blue")) +
  labs(x = "File size (KB)", y = "Upload time (seconds)") +
  scale_x_continuous(breaks = c(seq(0,600,100)), limits = c(0,600)) +
  scale_y_continuous(breaks = c(seq(0,10,2)), limits = c(0,10)) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=16),
        panel.grid.major = element_blank(), # no grid lines
        panel.grid.minor = element_blank(),
        legend.title=element_text(size=12),
        legend.text=element_text(size=10))
ggsave("uploading_data_time_graph.png")
dev.off()
