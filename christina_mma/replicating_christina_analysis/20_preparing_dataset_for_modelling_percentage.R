# Grace Skinner 17/5/22
# Script based on Christina Raw's NHM_MMA/Scripts/20.PreparingDatasetForModelling_Percentage.R

# Content of script
#   1. Subset LRR and % change data
#   2. Transform LRR into % data and merge datasets
#   3. Input column for control biodiversity value ( = 100) and a column for the 
#      intervention biodiversity value that is the difference to the reference 
#      value.

# ----------------------------------------------------------------------

library(readxl) # for reading excel files
library(dplyr) # for data manipulation
library(writexl) # for exporting to an excel file

d <- read_xlsx("../christina_main_initial_data_files/04.Excel_Magpie_Crops_WithControl_Quantitative_spreadsheet (1).xlsx")

# -----------------------------------------------------------------------

# 1. Subset LRR and % change data

unique(d$Effect_size)
perc <- subset(d, Effect_size == "Percentage_change")
LRR <- subset(d,  Effect_size == "LRR" )

# -------------------------------------------------------------------------

# 2. Transform LRR into % data and merge data sets

LRR$Percentage_change = 100 * (exp(LRR$Value) - 1) # not sure where this formula came from?
perc <- rbind(perc, LRR)

# -------------------------------------------------------------------------

# 3. Input column for control biodiversity value (= 100) and a column for the 
#    intervention biodiversity value that is the difference to the reference 
#    value.

perc$Control_Percentage_Change <- 100 # Add column for control biodiversity value
perc <- mutate(perc, Intervention_Percentage_Change = 100 + Percentage_change)

## Or, without dplyr:
# perc$Intervention_Percentage_Change2 <- perc$Percentage_change + 100


# --------------------------------------------------------------------------

# Once I have the control and intervention value I can model

write_xlsx(perc, "../output_files/05.Excel_Percentage_Dataset_to_model.xlsx")
















