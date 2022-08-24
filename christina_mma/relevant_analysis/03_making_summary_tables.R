# Grace Skinner 23/5/22
# Script based on Christina Raw's NHM_MMA/Scripts/29.Making_summary_tables.R

## Contents
#   1. Load Bayesian and Robust models 
#   2. Extract coefficients
#   3. Adjust reference level
#   4. Obtain other coefficients: CI, percentage_change
#   5. Plotting 
#   6. Save tables 

rm(list=ls())

library(tidyverse)
library(tibble) # to convert rownames to column

# ==============================================================================

# 1. Load blmer and robust models
blmer <- readRDS("../output_files/blmer_model.rds")
robust <- readRDS("../output_files/robust_model.rds")

# ------------------------------------------------------------------------------

# 2. Extract coefficients 

blmer_coefficients <- blmer %>%  summary() %>% .$coefficients %>% 
  round(digits = 2) %>%  as.data.frame()

robust_coefficients <- robust %>%  summary() %>% .$coefficients %>% 
  round(digits = 2) %>% as.data.frame()

# ------------------------------------------------------------------------------

# 3. Adjust reference level

# Adjust reference level (intercept = conventional) to 0. That way I will be able 
# to report absolute LRR and percentage change relative to conventional. To do so,
# I have to take away the intercept (conventional) value ( = 0.02) to each level. 

names(robust_coefficients) <- c("Original_LRR", "SE", "t") # Change names to make code easier

row.names(robust_coefficients) <- c("Conventional", "Conservation", "Disturbed forest", 
                                    "Fallow", "Mixed", "Non Bt", "Primary vegetation", 
                                    "Sustainable", "Traditional", "Transgenic")

robust_coefficients$Adjusted_LRR <- ""  # New column where I will store adjusted
# values after conventional (= 0.02) has been set to 0

robust_coefficients$Adjusted_LRR[1] <- 0 # Adjust conventional to 0

for (i in 1:length((robust_coefficients$Original_LRR))) { # Adjust other levels
  ifelse(i > 0, 
         robust_coefficients$Adjusted_LRR[i] <- robust_coefficients$Original_LRR[i] - 0.02)
}

str(robust_coefficients)

# ------------------------------------------------------------------------------------

# 4. Obtain other coefficients: CI, percentage_change

# Change Adjusted_LRR class to numeric (idk why it came out as character after the for loop)
robust_coefficients$Adjusted_LRR <- as.numeric(robust_coefficients$Adjusted_LRR)

robust_coefficients <- robust_coefficients %>% 
  mutate(LCI = Adjusted_LRR - (1.96 * SE),
         UCI = Adjusted_LRR + (1.96 * SE),
         percentage_change = 100 * (exp(Adjusted_LRR) - 1)) %>% 
  round(digits = 2) %>% 
  mutate(CI = paste( LCI, "to", UCI))

# make rownames into a column, rather than just the rownames - so can use to plot later 
blmer_coefficients <- tibble::rownames_to_column(blmer_coefficients, "Treatment")
robust_coefficients <- tibble::rownames_to_column(robust_coefficients, "Treatment")

# ------------------------------------------------------------------------------------------------

# 5. Plotting 

### Plotting idea 1
robust_coefficients %>% 
  filter(Treatment != "Conventional") %>% # remove conventional as this is the reference category
  ggplot(aes(x = reorder(Treatment, -Adjusted_LRR), Adjusted_LRR)) + # Do in ascending adjusted_LRR ordr
  geom_errorbar(aes(x = reorder(Treatment, -Adjusted_LRR), ymin=LCI, ymax=UCI), width=0.2, colour="black", alpha=0.9, size=1.3) +
  geom_point(stat="identity", size = 3, colour = "black") +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed") + # add reference line
  labs(x="Treatment", y="Adjusted LRR") +
  coord_flip() + 
  theme_bw() +
  theme(axis.text = element_text(size=16), 
        axis.title = element_text(size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  

# Add significance - bit difficult and may need to find better way but the below code does what I want it to do

### Plotting idea 2

# Make version where table is ordered in the same way as it will be displayed in ggplot2
ordered_robust_coefficients <- robust_coefficients %>% 
  arrange(desc(Adjusted_LRR), Treatment) %>% # arrange in descending adjusted_LRR and then by Treatment - makes Sustainable be row 1
  filter(Treatment != "Conventional")

### Find out which results are significant 
signif_results <- c()
t_values <- c()
adjusted_LRR <- c()
for (i in 1:nrow(ordered_robust_coefficients)) {
  if (ordered_robust_coefficients$t[i] >= 2  || ordered_robust_coefficients$t[i] <= -2) {
    signif_results <- c(signif_results, i)
    t_values <- c(t_values, ordered_robust_coefficients$t[i])
    adjusted_LRR <- c(adjusted_LRR, ordered_robust_coefficients$Adjusted_LRR[i])
  }
}

# add 0.25 to this so puts the significance star just above the point (not directly on it)
signif_results_plus_0.25 <- signif_results + 0.25

## Plotting
pdf("../output_files/robust_model_plot.pdf")
p <- robust_coefficients %>% 
  filter(Treatment != "Conventional") %>%
  ggplot(aes(x = reorder(Treatment, -Adjusted_LRR), Adjusted_LRR)) + 
  geom_errorbar(aes(x = reorder(Treatment, -Adjusted_LRR), ymin=LCI, ymax=UCI), width=0.2, colour="black", alpha=0.9, size=1.3) +
  geom_point(stat="identity", size = 5, colour = "black") +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
  labs(x="Treatment", y="Adjusted LRR") +
  coord_flip() +
  annotate("text", x = signif_results_plus_0.25, y = adjusted_LRR,   ######### Maybe need better way of doing this really 
             label = "*", size = 8) +
  theme_bw() +
  theme(axis.text = element_text(size=16), 
        axis.title = element_text(size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(p)
graphics.off()

# -------------------------------------------------------------------------------------------

# 6. Save tables 

# I will make a word table using the following steps: 
#   1. Create a table or data.frame in R.
#   2. Write this table to a comma-separated .txt file using write.table().
#   3. Copy and paste the content of the .txt file into Word.
#   4. In Word,
#       a. select the text you just pasted from the .txt file
#       b. go to Table → Convert → Convert Text to Table…
#       c. make sure “Commas” is selected under “Separate text at”, click OK
#       d. In this case, the 1st column doesn't have a column name so the columns names are incorrectly shifted one to the left

write.table(blmer_coefficients, "../output_files/blmer_coefficients.txt", 
            sep = ",")
write.table(robust_coefficients, "../output_files/robust_coefficients.txt",
            sep = ",")















