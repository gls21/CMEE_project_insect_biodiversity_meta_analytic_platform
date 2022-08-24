# Grace Skinner 25/5/22
# Script based on Christina Raw's NHM_MMA/Scripts/06.Writing/29.Making_summary_tables.R
# Input: blmer_model.rds
# Output: blmer_coefficients.txt

## Contents
#   1. Load Bayesian and Robust models 
#   2. Extract coefficients
#   3. Save table

# ==============================================================================

# 1. Load packages and model 

library(tidyverse)
library(tibble) # to convert rownames to column

blmer <- readRDS("../output_files/blmer_model.rds")

# ------------------------------------------------------------------------------

# 2. Extract coefficients 

blmer_coefficients <- blmer %>%  summary() %>% .$coefficients %>% 
  round(digits = 2) %>%  as.data.frame()

# make rownames into a column, rather than just the rownames 
blmer_coefficients <- tibble::rownames_to_column(blmer_coefficients, "Treatment")

# -------------------------------------------------------------------------------

# 3. Save table 

# I will make a word table using the following steps: 
#   1. Create a table or data.frame in R.
#   2. Write this table to a comma-separated .txt file using write.table().
#   3. Copy and paste the content of the .txt file into Word.
#   4. In Word,
#       a. select the text you just pasted from the .txt file
#       b. go to Table - Convert - Convert Text to Table
#       c. make sure Commas is selected under Separate text, click OK
#       d. In this case, the 1st column doesn't have a column name so the columns names are incorrectly shifted one to the left

write.table(blmer_coefficients, "../output_files/blmer_coefficients.txt", sep = ",")
















