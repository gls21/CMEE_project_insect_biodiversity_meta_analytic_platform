# Grace Skinner 24/5/22
# Script based on Christina Raw's NHM_MMA/Scripts/04.Data curation/22.PreparingDatasetForModelling_LRR.R
# Input: 04.Excel_Magpie_Crops_WithControl_Quantitative_spreadsheet (1).xlsx
# Output: 07.Excel_Dataset_to_model_LRR_LONG.xlsx

# This script will prepare the dataset for robust modelling. 

# Content of script
#   1. Load packages and data
#   2. Add sample size = 1 for reviews; sample size = number of studies for meta-analyses
#   3. Fix issue with percentages that are 0 or negative 
#   4. Transform data from percentage change into LRR
#   5. Fix some inconsistent naming and collapse certain categories
#   6. Add weights
#   7. Change data set from wide to long
#   8. Reorder columns to make data frame clearer
#   9. Input treatment and control practices
#   10. Final cleaning of LONG format dataset 
#   11. Save long format dataframe 

# =============================================================================

# 1. Load packages and data

library(readxl) # for reading excel files
library(dplyr) # for data manipulation
library(writexl) # for exporting to an excel file
library(tidyr) # to tidy messy data 
library(knitr) # For making tables
library(kableExtra)# for pretty rmd tables

d <- read_xlsx("../christina_main_initial_data_files/04.Excel_Magpie_Crops_WithControl_Quantitative_spreadsheet (1).xlsx")

# -----------------------------------------------------------------------------------------------

# 2. Add sample size = 1 for reviews; sample size = number of studies for meta-analyses

colnames(d)[37] <- "LRR"  # Change column name from 'value' so it's clearer

d$Sample_size <- d$N_Studies # Add sample size
d$Sample_size[d$Synthesis_type == "Review"] <- 1

y <- select(d, Synthesis_type, Sample_size, N_Studies) # Checked it worked

which(d$Sample_size == 326) # Fix an error in the data set
d$N_Studies[191:203] <- 62
d$Sample_size[191:203] <- 62

# ----------------------------------------------------------------------------------------

# 3. Fix issue with percentages that are 0 or negative 

perc <- subset(d, d$Effect_size == "Percentage_change") # Subset data of interest
LRR <- subset(d, d$Effect_size == "LRR")

neg_perc <- subset(perc, Percentage_change < 0) # Subset negative percentages
perc <-subset(perc, perc$Percentage_change >= 0)  # Remove negative percentages 

# Now I will turn around negative percentages into positive percentages. I have 
# to switch control and treatment and then turn (e.g.) -30 into +130
# Because produces -Inf and NaNs for 0 and -ve values, respectively 

neg_perc$Percentage_change <- 100 + (-(neg_perc$Percentage_change)) # turn around % data

neg_perc <- rename(neg_perc, # switch control and treatment
                   agricultural_system = Control,
                   Control = agricultural_system)

perc <- relocate(perc, agricultural_system,  # relocate columns so they match in the rbind
                 .after = Control)

neg_perc <- relocate(neg_perc, agricultural_system,
                     .after = Control)

# In the perc data frame (= biodiversity percentage increase with treatment) I have to add 
# a 100 because in the when transforming LRR into percentage, a percentage value 
# lower than 100 will be a decrease in biodiversity (e.g., 23% increase should be 
# 123% increase so when transforming it into LRR it is an actual increase)
perc$Percentage_change <- perc$Percentage_change + 100

# Now I have to rbind both data frames
perc <- rbind(perc, neg_perc)

# Now lowest % change value is 100% i.e. no change 

length(which(perc$Percentage_change < 0)) # 0

# Now I only have positive percentage change data, so I am going to calculate the
# LRR again with this data

# ------------------------------------------------------------------------------

# 4. Transform data from percentage change into LRR

unique(perc$Effect_size)
unique(LRR$Effect_size)

length(which(perc$Percentage_change < 0)) # Check data is correct (no negative %)

# Formula to obtain LRR from percentage: LRR = ln((Percentage change/100) + 1) 

#perc <- mutate(perc, LRR = log(Percentage_change/100) + 1)

perc <- mutate(perc, LRR = log(Percentage_change/100)) # This is the original formula Christina used - not sure why +1 is missing? but leaving for now

perc$LRR  # Check it worked

# Rbind two data frames

LRR <- rbind(LRR, perc)

# -----------------------------------------------------------------------------

# 5. Fix some inconsistent naming and collapse certain categories

d <- LRR

unique(d$agricultural_system)

d$agricultural_system[d$agricultural_system == "conservation"] <- "Conservation"
d$agricultural_system[d$agricultural_system == "conventional"] <- "Conventional"

x <- as.data.frame(table(d$agricultural_system))

which(d$agricultural_system == "unclassified") # Change "unclassified" agricultural  
d[135:142, 27] <- "coculture"                  # system and treatment as co culture

# Collapse crop categories
unique(d$Crop) 
d$Crop[d$Crop == "Bt_Cotton"]<- "Cotton"
d$Crop[d$Crop == "Bt_Sunflower"]<- "Sunflower"
d$Crop[d$Crop == "Bt_Potato"]<- "Potato"
d$Crop[d$Crop == "Bt_Rape"]<- "Rape_seed"
d$Crop[d$Crop == "Bt_Maize"]<- "Maize"
d$Crop[d$Crop == "Bt_Rice"]<- "Rice"
d$Crop[d$Crop == "GM_Rape"]<- "Rape_seed"
d$Crop[d$Crop == "Bt_Corn"]<- "Corn"
d$Crop[d$Crop == "Bt_Broccoli"]<- "Broccoli"
d$Crop[d$Crop == "GM_Potato"]<- "Potato"
d$Crop[d$Crop == "GM_Cotton"]<- "Cotton"
d$Crop[d$Crop == "GM_Tomato"]<- "Tomato"
d$Crop[d$Crop == "Gm_Strawberry"]<- "Strawberry"

# -----------------------------------------------------------------------------

# 6. Add weights 
#   Reviews will have a wight of 1, and meta-analysis a 
#   weight = n studies included in the meta-analysis

d$Weight <- ""  
d$Weight[d$Synthesis_type == "Review"] <- 1
d$Weight[d$Synthesis_type == "Meta-Analysis"] <- d$N_Studies[d$Synthesis_type == "Meta-Analysis"]

x <- select(d, Synthesis_type, N_Studies, Weight) # Check adding weights worked
x <- x[!duplicated(x),]

# ----------------------------------------------------------------------------

# 7. Change data set from wide to long

LRR <- d # I do this because I already had the code written with LRR as data

LRR$ID <- 1:nrow(LRR) # Add column for comparison number
colnames(LRR)[37] <- "Treatment_LRR"

LRR$Control_LRR <- 0

long_LRR <- pivot_longer(LRR, cols = c(37,51), 
                         names_to = "Treatment", values_to = "LRR",
                         values_drop_na = FALSE)
# doubled number of rows 

# -----------------------------------------------------------------------------

# 8. Reorder columns to make data frame clearer

names(long_LRR)
long_LRR <- relocate(long_LRR, magpie_class, .after = Crop)
long_LRR <- relocate(long_LRR, LRR, .after = Sample_size)
long_LRR <- relocate(long_LRR, ID, .before = Effect_size)

# -----------------------------------------------------------------------------

# 9. Input agricultural system and control into treatment column

for(i in 1:nrow(long_LRR)){
  if(long_LRR$Treatment[i] == "Treatment_LRR"){
    long_LRR$Treatment[i] <- long_LRR$agricultural_system[i]
  } else {
    long_LRR$Treatment[i] <- long_LRR$Control[i]
  }
}

# ------------------------------------------------------------------------------
 
# 10. Final cleaning of LONG format dataset 

long_LRR$Treatment[long_LRR$Treatment == "conservation"] <- "Conservation"
long_LRR$Treatment[long_LRR$Treatment == "conventional"] <- "Conventional"
long_LRR$Treatment[long_LRR$Treatment == "traditional"] <- "Traditional"
long_LRR$Treatment[long_LRR$Treatment == "Conventional_NonWeeded"] <- "Conventional"
long_LRR$Treatment[long_LRR$Treatment == "Conventional_Weeded"] <- "Conventional"
long_LRR$Treatment[long_LRR$Treatment == "Fragmented_forest"] <- "Disturbed_forest"
long_LRR$Treatment[long_LRR$Treatment == "Logged_forest"] <- "Disturbed_forest"
long_LRR$Treatment[long_LRR$Treatment == "Fragmentded_forest"] <- "Disturbed_forest"
long_LRR$Treatment[long_LRR$Treatment == "Primary_forest"] <- "Primary_vegetation"
long_LRR$Treatment[long_LRR$Treatment == "Contiguous_forest"] <- "Primary_vegetation"
long_LRR$Treatment[long_LRR$Treatment == "Intercrop"] <- "Mixed"
long_LRR$Treatment[long_LRR$Treatment == "mixed"] <- "Mixed"
long_LRR$Treatment[long_LRR$Treatment == "Simplified"] <- "Conventional"

str(long_LRR)
long_LRR$Crop <- as.factor(long_LRR$Crop)
long_LRR$Treatment <- as.factor(long_LRR$Treatment)
long_LRR$magpie_class <- as.factor(long_LRR$magpie_class)
long_LRR$Weight <- as.numeric(long_LRR$Weight)
long_LRR$biodiveristy_metric_category <- as.factor(long_LRR$biodiveristy_metric_category)

treatment_obs <- as.data.frame(table(long_LRR$Treatment))
colnames(treatment_obs) <- c("Treatment", "N_Observations")
treatment_obs <- treatment_obs[order(treatment_obs$N_Observations, decreasing = TRUE),]
kbl(treatment_obs, row.names = FALSE)

# Collapse organic and sustainable
long_LRR$Treatment[long_LRR$Treatment == "Organic"] <- "Sustainable"

# Include monoculture in conventional 
long_LRR$Treatment[long_LRR$Treatment == "Monoculture"] <- "Conventional"

# Subset agricultural systems with >= 10 observations
treatment_obs <- as.data.frame(table(long_LRR$Treatment))
colnames(treatment_obs) <- c("Treatment", "N_Observations")
treatment_obs <- treatment_obs[order(treatment_obs$N_Observations, decreasing = TRUE),]
treatment_obs <- subset(treatment_obs, treatment_obs$N_Observations >= 10)
long_LRR <- long_LRR[long_LRR$Treatment %in% treatment_obs$Treatment, ] 

# Yaap paper has 2 slightly different versions of the author, so going to make them the same
long_LRR$Author[long_LRR$Author == "Betsy Yaap1, Matthew J. Struebig2,3*, Gary Paoli1 and Lian Pin Koh5"] <- "Betsy Yaap1, Matthew J. Struebig2,3*, Gary Paoli1 and Lian Pin Koh4"

# Find unique combinations of paper_number and paper_ID 
id_number <- unique(long_LRR[,c("Paper_number","Paper_ID")])
id_number <- id_number %>%
  arrange(Paper_ID) # Delaquis.E appears twice
# Filter the data by this author 
dupl_author <- long_LRR %>%
  filter(Paper_ID == "Delaquis.E")
# Most of them have the paper ID as 36, but a couple have it as 35
# Change the ones that have 35 to 36 for consistency 
long_LRR$Paper_number[long_LRR$Paper_number == 35] <- 36

# Change incorrect spelling of biodiversity_metric_category column name 
names(long_LRR)[34] <- "biodiversity_metric_category"

# Change NAs in biodiversity metric categories to unclassified 
long_LRR$biodiversity_metric_category[is.na(long_LRR$biodiversity_metric_category)] <- "unclassified"

# ------------------------------------------------------------------------------------

# 11. Save long format dataframe 

write_xlsx(long_LRR, "../output_files/07.Excel_Dataset_to_model_LRR_LONG.xlsx")
write_xlsx(long_LRR, "../meta_meta_analysis/data_and_models/christina_mma.xlsx")















