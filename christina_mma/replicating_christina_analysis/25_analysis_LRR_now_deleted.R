# Grace Skinner 18/5/22
# Script based on Christina Raw's NHM_MMA/Scripts/25.Analysis_LRR.Rmd

# Christina's script was an R markdown file

# In this script I am going to perform a Bayesian analysis of the data.

# Data set: quantitative observation of biodiversity under different agricultural systems. 
# Goal: see how different agricultural systems impact biodiversity.

# ------------------------------------------------------------------------------

library(blme) # For Bayesian analysis
library(readxl) # For reading the excel data set
library(here) # For specifying file paths
library(knitr) # For making tables
library(kableExtra)# for pretty rmd tables
library(dplyr) # For some data wrangling
library(tidyr) # For some data wrangling

# ------------------------------------------------------------------------------

# 1. Take a look at the data

d <- read_excel("../output_files/07.Excel_Dataset_to_model_LRR_LONG.xlsx")

treatment_obs <- as.data.frame(table(d$Treatment))
colnames(treatment_obs) <- c("Treatment", "N_Observations")
treatment_obs <- treatment_obs[order(treatment_obs$N_Observations, decreasing = TRUE),]
kbl(treatment_obs, row.names = FALSE)

# Collapse sustainable and organic, as organic is a sustainable agricultural practice. 
# Include in the analysis only those treatments that have > 10 observations
# Add weights

d$Treatment[d$Treatment == "Organic"] <- "Sustainable"

treatment_obs <- as.data.frame(table(d$Treatment))
colnames(treatment_obs) <- c("Treatment", "N_Observations")
treatment_obs <- treatment_obs[order(treatment_obs$N_Observations, decreasing = TRUE),]

treatment_obs <- subset(treatment_obs, treatment_obs$N_Observations >= 10)

d <- d[d$Treatment %in% treatment_obs$Treatment, ] 

# ------------------------------------------------------------------------------

# Residuals histogram and qqplot of data without the control zeroes

# Null model with only random effect to see qq plot of weighted residuals 
nonzero <- read_excel("../Output_files/06.Excel_Dataset_to_model_LRR_WIDE.xlsx")
m0 <- blmer(LRR ~ 1 + (1|Crop), weights = Weight, data = nonzero) # gives warning about not converge
hist(residuals(m0))
qqnorm(resid(m0))

# It looks fairly normal to me!

# ------------------------------------------------------------------------------

# Residuals histogram and qqplot of data with the control zeroes

# Null model with only random effect to see qq plot of weighted residuals 
m1 <- blmer(LRR ~ 1 + (1|Crop), weights = Weight, data = d)
hist(residuals(m1))
qqnorm(resid(m1))

# Zero-inflated, but that's all. The zeros won't influence in the output. 


# --------------------------------------------------------------------------------

### 2. Analysis

# Re-level d so conventional is reference
d$Treatment <- as.factor(d$Treatment)
d$Treatment <- relevel(d$Treatment, ref = "Conventional")


### Model 1: how does agriculture vary across agricultural systems?
m1 <- blmer(LRR ~ Treatment + biodiveristy_metric_category + (1|ID) + (1|Crop), data = d, control = lmerControl(optimizer ="Nelder_Mead"))
summary(m1)

# Crop does not explain much variance as random effect.
# There is significant biodiversity change between conventional and: conservation, primary vegetation and sustainable (organic + sustainable).
# However, this does not tell me how each metric varies under the different agricultural systems. 
# To see that, I would need to include an interaction


### Model 2: how does agriculture vary across agricultural systems with a closer look to biodiversity metric.

# Fixed effects (I am interested in their mean)
# - Agricultural system: I want to know biodiversity mean in each agricultural system ( = Treatment)
# - Biodiversity metric: I want to know the mean of each type of biodiversity metric in each type of agricultural system, 
#   so, maybe introduce an interaction? metric:agricultural system?
  
# Random effects (variance)
# - Crop: grouping factor
# - ID: actually this one is only to account for non independence of the two data points that come from the same control-treatment comparison

m2 <- blmer(LRR ~  Treatment + biodiveristy_metric_category:Treatment + (1|Crop) + (1|ID), weights = Weight, data = d, control = lmerControl(optimizer ="Nelder_Mead"))
summary(m2)

# Warning because not all combinations of the interaction have data, so the model drops them.
# Residual variance increases when including the interaction, which I think means that this model is worse explaining the variance.

# My goal with this analysis is too see how biodiversity varies under agricultural systems within crop and magpie class. 
# Therefore, I believe subsetting first per crop and then per magpie class will be more inormative.


### Analysis per crop

crop <- unique(d$Crop)
crop_list <- list() 
for (i in crop){
  data <- dplyr :: filter( d , Crop == i)
  crop_list[[i]] <- data
}

# From a named list x, create an environment containing all list components as objects
list2env(lapply(crop_list, as.data.frame), .GlobalEnv)

## Try with crops with more than 50 observations

# Cassava: only two treatments (mixed and monoculture) -> t.test
t.test(LRR~Treatment, data = Cassava )

# Cotton
cotton <- blmer(LRR ~  Treatment + biodiveristy_metric_category:Treatment  + (1|ID), weights = Weight, data = Cotton, control = lmerControl(optimizer ="Nelder_Mead"))
summary(cotton)

# Maize
maize <- blmer(LRR ~  Treatment + biodiveristy_metric_category:Treatment  + (1|ID), weights = Weight, data = Cotton, control = lmerControl(optimizer ="Nelder_Mead"))
summary(maize)

## I can see that doing it per crop is not the best approach given the data set.
# Next, I will try per magpie class.


### Analysis per magpie class

magpie <- unique(d$magpie_class)
magpie_list <- list() 
for (i in magpie){
  data <- dplyr :: filter( d , magpie_class == i)
  magpie_list[[i]] <- data
}
list2env(lapply(magpie_list, as.data.frame), .GlobalEnv)

# Cereals
cereals <- blmer(LRR ~  Treatment + biodiveristy_metric_category:Treatment  + (1|ID), weights = Weight, data = Cereals, control = lmerControl(optimizer ="Nelder_Mead"))
summary(cereals)

# Oil
oil <- blmer(LRR ~  Treatment + biodiveristy_metric_category:Treatment  + (1|ID), weights = Weight, data = `Oil crops`, control = lmerControl(optimizer ="Nelder_Mead"))
summary(oil)

# I could continue but I guess I am going to keep getting these really small estimates. 
# I think what is happening is that when i subset per crop and magpie class, 
# the zeroes included in the control are playing a much stronger role and actually *do* influence the model output.















