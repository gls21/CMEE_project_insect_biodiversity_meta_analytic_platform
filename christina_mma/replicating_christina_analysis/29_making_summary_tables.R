# Grace Skinner 18/5/22
# Script based on Christina Raw's NHM_MMA/Scripts/29.Making_summary_tables.R

# ------------------------------------------------------------------------------

library(here)
library(tidyverse)

# ------------------------------------------------------------------------------

rm(list=ls())

# load blmer and robust models
blmer <- readRDS("../output_files/blmer_model.rds")
robust <- readRDS("../output_files/robust_model.rds")


# Extract coefficients 

blmer_coefficients <- blmer %>%  summary() %>% .$coefficients %>% 
  round(digits = 2) %>%  as.data.frame()

robust_coefficients <- robust %>%  summary() %>% .$coefficients %>% 
  round(digits = 2) %>% as.data.frame()

# Adjust reference level (intercept = conventional) to 0. That way I will be able 
# to report absolute LRR and percentage change relative to conventional. To do so,
# I have to add the intercept (conventional) value ( = 0.02) to each level.


names(robust_coefficients) <- c( "Original_LRR", "SE", "t") # Change names to make code easier

robust_coefficients$Adjusted_LRR <- ""  # New column where I will store adjusted
# values after conventional (= 0.02) has been set to 0

robust_coefficients$Adjusted_LRR[1] <- 0 # Adjust conventional to 0

for (i in 1:length((robust_coefficients$Original_LRR))) { # Adjust other levels
  ifelse(i > 0, 
         robust_coefficients$Adjusted_LRR[i] <- robust_coefficients$Original_LRR[i] - 0.02, 
         robust_coefficients$Adjusted_LRR[i] <- robust_coefficients$Original_LRR[i] - 0.02)
}


str(robust_coefficients)

# Obtain other coefficients: CI, percentage_change
# Change Adjusted_LRR class to numeric (idk why it came out as character after the for loop)

robust_coefficients$Adjusted_LRR <- as.numeric(robust_coefficients$Adjusted_LRR)

robust_coefficients <- robust_coefficients %>% 
  mutate(LCI = Adjusted_LRR - (1.96 * SE),
         UCI = Adjusted_LRR + (1.96 * SE),
         percentage_change = 100 * (exp(Adjusted_LRR) - 1)) %>% 
  round(digits = 2) %>% 
  mutate(CI = paste( LCI, "to", UCI))


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















