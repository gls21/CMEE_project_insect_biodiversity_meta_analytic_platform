# Grace Skinner 25/5/22
# Script based on Christina Raw's NHM_MMA/Scripts/06.Writing/29.Making_summary_tables.R
# Inputs: robust_model.rds
#        sample_sizes.csv
# Outputs: robust_model_plot.png
#          robust_coefficients.txt
#          robust_coefficients.csv

# This script will extract the coefficients from the robust module and plot them. 

## Contents
#   1. Load Bayesian and Robust models 
#   2. Extract coefficients
#   3. Adjust reference level
#   4. Obtain other coefficients: CI, percentage_change (and CI for percentage change)
#   5. Plotting 
#   6. Save .txt and .csv tables

# ==============================================================================

# 1. Load packages and model 

library(tidyverse)
library(tibble) # to convert rownames to column

robust <- readRDS("../output_files/robust_model.rds")
sample_sizes <- read.csv("../output_files/sample_sizes.csv", row.names = 1)

# ------------------------------------------------------------------------------

# 2. Extract coefficients 

robust_coefficients <- robust %>%  summary() %>% .$coefficients %>% 
  round(digits = 2) %>% as.data.frame()

# ------------------------------------------------------------------------------

# 3. Adjust reference level

# Adjust reference level (intercept = conventional) to 0. That way I will be able 
# to report absolute LRR and percentage change relative to conventional. To do so,
# I have to take away the intercept (conventional) value ( = 0.02) to each level. 

names(robust_coefficients) <- c("Original_LRR", "SE", "t") # Change names to make code easier

# Remove as won't work when new MAs are added and new agricultural systems are present 
# row.names(robust_coefficients) <- c("Conventional", "Conservation", "Disturbed forest", 
#                                     "Fallow", "Mixed", "Non Bt", "Primary vegetation", 
#                                     "Sustainable", "Traditional", "Transgenic")

# Do like this instead
row.names(robust_coefficients) <- c(sample_sizes$Treatment)

robust_coefficients$Adjusted_LRR <- ""  # New column where I will store adjusted
# values after conventional (= 0.02) has been set to 0

robust_coefficients$Adjusted_LRR[1] <- 0 # Adjust conventional to 0

for (i in 1:length((robust_coefficients$Original_LRR))) { # Adjust other levels
  ifelse(i > 0, 
         robust_coefficients$Adjusted_LRR[i] <- robust_coefficients$Original_LRR[i] - 0.02)
}

str(robust_coefficients)   

# ------------------------------------------------------------------------------------

# 4. Obtain other coefficients: CI, percentage_change (plus CI for percentage change)

# Change Adjusted_LRR class to numeric (idk why it came out as character after the for loop)
robust_coefficients$Adjusted_LRR <- as.numeric(robust_coefficients$Adjusted_LRR)

# add in 
robust_coefficients <- robust_coefficients %>% 
  mutate(LCI = Adjusted_LRR - (1.96 * SE),
         UCI = Adjusted_LRR + (1.96 * SE)) %>%
  round(digits = 2) %>% 
  mutate(CI = paste( LCI, "to", UCI)) %>%
  mutate(percentage_change = 100 * (exp(Adjusted_LRR) - 1),
         LCI_percent = 100 * (exp(LCI) - 1),
         UCI_percent = 100 * (exp(UCI) - 1)) %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate(CI_percent = paste(LCI_percent, "to", UCI_percent)) 


# make rownames into a column, rather than just the rownames - so can use to plot later 
robust_coefficients <- tibble::rownames_to_column(robust_coefficients, "Treatment")

# rbind sample sizes table with robust_coefficients table
robust_coefficients <- merge(robust_coefficients, sample_sizes, by = "Treatment", sort = FALSE)

# ------------------------------------------------------------------------------------------------

# 5. Plotting 

### Plotting idea 1 - no significance stars
# robust_coefficients %>% 
#   filter(Treatment != "Conventional") %>% # remove conventional as this is the reference category
#   ggplot(aes(x = reorder(Treatment, -Adjusted_LRR), Adjusted_LRR)) + # Do in ascending adjusted_LRR ordr
#   geom_errorbar(aes(x = reorder(Treatment, -Adjusted_LRR), ymin=LCI, ymax=UCI), width=0.2, colour="black", alpha=0.9, size=1.3) +
#   geom_point(stat="identity", size = 3, colour = "black") +
#   geom_hline(yintercept = 0, color = "blue", linetype = "dashed") + # add reference line
#   labs(x="Treatment", y="Adjusted LRR") +
#   coord_flip() + 
#   theme_bw() +
#   theme(axis.text = element_text(size=16), 
#         axis.title = element_text(size=18),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())


# Add significance - bit difficult and may need to find better way but the below code does what I want it to do

### Plotting idea 2 - to include significance stars

# Make version where table is ordered in the same way as it will be displayed in ggplot2
ordered_robust_coefficients <- robust_coefficients %>% 
  arrange(desc(Adjusted_LRR), Treatment) %>% # arrange in descending adjusted_LRR and then by Treatment - makes Sustainable be row 1
  filter(Treatment != "Conventional")

# Find out which results are significant 
signif_results <- c()
t_values <- c()
adjusted_LRR <- c()
percentage_change <- c()
for (i in 1:nrow(ordered_robust_coefficients)) {
  if (ordered_robust_coefficients$t[i] >= 2  || ordered_robust_coefficients$t[i] <= -2) {
    signif_results <- c(signif_results, i)
    t_values <- c(t_values, ordered_robust_coefficients$t[i])
    adjusted_LRR <- c(adjusted_LRR, ordered_robust_coefficients$Adjusted_LRR[i])
    percentage_change <- c(percentage_change, ordered_robust_coefficients$percentage_change[i])
  }
}

# add 0.25 to this so puts the significance star just above the point (not directly on it)
signif_results_plus_0.25 <- signif_results + 0.25

## Plotting adjusted LRR with significance stars
robust_coefficients %>% 
  filter(Treatment != "Conventional") %>%
  ggplot(aes(x = reorder(Treatment, -Adjusted_LRR), Adjusted_LRR)) + 
  geom_errorbar(aes(x = reorder(Treatment, -Adjusted_LRR), ymin=LCI, ymax=UCI), width=0.2, colour="black", alpha=0.9, size=1.3) +
  geom_point(stat="identity", size = 4, colour = "black") +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
  labs(x="Treatment", y="Adjusted LRR") +
  coord_flip() +
  annotate("text", x = signif_results_plus_0.25, y = adjusted_LRR,   ######### Maybe need better way of doing this really 
           label = "*", size = 8) +
  theme_bw() +
  theme(axis.text = element_text(size=14), 
        axis.title = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ggsave("../output_files/robust_model_plot.png", height = 5, width = 10, dpi = 600)

## Plotting percentage change with significance stars
robust_coefficients %>% 
  filter(Treatment != "Conventional") %>%
  ggplot(aes(x = reorder(Treatment, -percentage_change), percentage_change)) + 
  geom_errorbar(aes(x = reorder(Treatment, -percentage_change), ymin=LCI_percent, ymax=UCI_percent), width=0.2, colour="black", alpha=0.9, size=1.3) +
  geom_point(stat="identity", size = 4, colour = "black") +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
  labs(x="Treatment", y="Percentage change") +
  coord_flip() +
  annotate("text", x = signif_results_plus_0.25, y = percentage_change,   ######### Maybe need better way of doing this really 
           label = "*", size = 8) +
  theme_bw() +
  theme(axis.text = element_text(size=14), 
        axis.title = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ggsave("../output_files/robust_model_plot_percentage_change.png", height = 5, width = 10, dpi = 600)

# -------------------------------------------------------------------------------------------

# 6. Save .txt and .csv tables

# I will make a word table using the following steps: 
#   1. Create a table or data.frame in R.
#   2. Write this table to a comma-separated .txt file using write.table().
#   3. Copy and paste the content of the .txt file into Word.
#   4. In Word,
#       a. select the text you just pasted from the .txt file
#       b. go to Table ??? Convert ??? Convert Text to Table.
#       c. make sure "Commas" is selected under "Separate text at", click OK
#       d. In this case, the 1st column doesn't have a column name so the columns names 
#          are incorrectly shifted one to the left

# save table
write.table(robust_coefficients, "../output_files/robust_coefficients.txt", sep = ",")

# save dataframe
write.csv(robust_coefficients, "../output_files/robust_coefficients.csv")
write.csv(robust_coefficients, "../meta_meta_analysis/data_and_models/robust_coefficients.csv")













