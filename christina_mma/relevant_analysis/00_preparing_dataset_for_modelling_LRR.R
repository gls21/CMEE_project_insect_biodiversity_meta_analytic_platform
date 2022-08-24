# Grace Skinner 20/5/22
# Script based on Christina Raw's NHM_MMA/Scripts/22.PreparingDatasetForModelling_LRR.R

# In this script I am going to prepare the data set for the new modelling approach (using LRR).

# Content of script
#   1. Add sample size = 1 for reviews; number of studies for meta-analyses (fixing issue with -ve and 0 percentages)
#   2. Percentage -> LRR
#   3. Fix some rows values 
#   4. Add weights
#   5. Change data set from wide to long
#   6. Reorder columns to make data frame more clear
#   7. Input treatment and control practices
#   8. Final cleaning and save long format dataframe 

rm(list=ls())

library(readxl) # for reading excel files
library(dplyr) # for data manipulation
library(writexl) # for exporting to an excel file
library(tidyr) # to tidy messy data 
library(knitr) # For making tables
library(kableExtra)# for pretty rmd tables

# =============================================================================

# 1. Add sample size = 1 for reviews; number of studies for meta-analyses
#    Also fix percentage issue

d <- read_xlsx("../christina_main_initial_data_files/04.Excel_Magpie_Crops_WithControl_Quantitative_spreadsheet (1).xlsx")

colnames(d)[37] <- "LRR"  # Change column name from 'value' so it's clearer

d$Sample_size <- d$N_Studies # Add sample size
d$Sample_size[d$Synthesis_type == "Review"] <- 1

y <- select(d, Synthesis_type, Sample_size, N_Studies) # Checked it worked

which(d$Sample_size == 326) # Fix an error in the data set
d$N_Studies[191:203] <- 62
d$Sample_size[191:203] <- 62

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

# 2. Transform data from percentage change into LRR

# Subset LRR and percentage change data

unique(perc$Effect_size)
unique(LRR$Effect_size)

length(which(perc$Percentage_change < 0)) # Check data is correct (no negative %)

# Transform % change into LRR
# Formula to obtain LRR from percentage: LRR= ln((Percentage change/100) + 1) 

perc <- mutate(perc, LRR = log(Percentage_change/100))

perc$LRR  # Check it worked

# Rbind two data frames

LRR <- rbind(LRR, perc)

# -----------------------------------------------------------------------------

# 3. Fix some rows values 

d <- LRR

unique(d$agricultural_system)

d$agricultural_system[d$agricultural_system == "conservation"] <- "Conservation"
d$agricultural_system[d$agricultural_system == "conventional"] <- "Conventional"

x <- as.data.frame(table(d$agricultural_system))

which(d$agricultural_system == "unclassified") # Change "unclassified" agricultural  
d[135:142, 27] <- "coculture"                  # system and treatment as co culture

unique(d$Crop) # Collapse crop categories
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

# 4. Add weights and save wide format dataframe 
#   Reviews will have a wight of 1, and meta-analysis a 
#   weight = n studies included in the meta-analysis

d$Weight <- ""  
d$Weight[d$Synthesis_type == "Review"] <- 1
d$Weight[d$Synthesis_type == "Meta-Analysis"] <- d$N_Studies[d$Synthesis_type == "Meta-Analysis"]

x <- select(d, Synthesis_type, N_Studies, Weight) # Check adding weights worked
x <- x[!duplicated(x),]

# Save wide format data set
write_xlsx(d, "../output_files/06.Excel_Dataset_to_model_LRR_WIDE.xlsx")

# ----------------------------------------------------------------------------

# 5. Change data set from wide to long

LRR <- d # I do this because I already had the code written with LRR as data

LRR$ID <- 1:nrow(LRR) # Add column for comparison number
colnames(LRR)[37] <- "Treatment_LRR"

LRR$Control_LRR <- 0

long_LRR <- pivot_longer(LRR, cols = c(37,51), 
                         names_to = "Treatment", values_to = "LRR",
                         values_drop_na = FALSE)
# doubled number of rows 

# -----------------------------------------------------------------------------

# 6. Reorder columns to make data frame more clear

names(long_LRR)
long_LRR <- relocate(long_LRR, magpie_class, .after = Crop)
long_LRR <- relocate(long_LRR, LRR, .after = Sample_size)
long_LRR <- relocate(long_LRR, ID, .before = Effect_size)

# -----------------------------------------------------------------------------

# 7. Input treatment and control practices

for(i in 1:nrow(long_LRR)){
  if(long_LRR$Treatment[i] == "Treatment_LRR"){
    long_LRR$Treatment[i] <- long_LRR$agricultural_system[i]
  } else {
    long_LRR$Treatment[i] <- long_LRR$Control[i]
  }
}

# ------------------------------------------------------------------------------
 
# 8. Final cleaning and save long format dataframe 

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
long_LRR$Treatment[long_LRR$Treatment == "Intercrop"] <- "mixed"
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

# Save data frame
write_xlsx(long_LRR, "../output_files/07.Excel_Dataset_to_model_LRR_LONG.xlsx")
















