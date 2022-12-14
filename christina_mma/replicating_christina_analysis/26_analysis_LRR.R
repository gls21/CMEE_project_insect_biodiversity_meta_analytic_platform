# Grace Skinner 18/5/22
# Script based on Christina Raw's NHM_MMA/Scripts/26.Analysis_LRR.Rmd

# Christina's script was an R markdown file

# In this script I am going to perform a Bayesian analysis of the data 
# to see how different agricultural systems impact biodiversity.
# The data set consists of quantitative observation of biodiversity under different agricultural systems. 

# Contents
# 1. Take a look at the data
# - 1.1 Residuals histogram and qqplot of data without the control zeroes
# - 1.2 Residuals and qqplot of data with the control zeroes
# 2. Analysis
# - 2.1 Brief introduction of the method
# - 2.2 Re level data so conventional is reference
# - 2.3 Fixed effects
# - 2.4 Random effects
# - 2.5 Model
# - 2.6 Model goodness-of-fit
# - 2.7 Model with Crop as fixed effect
# - 2.8 Taxon level
# 3. Analysis per biodiversity metric
# - 3.1 Biodiversity metric data barplot
# - 3.2 Biodiversity metric data **per crop**
# - 3.3 Biomass and diversity analysis **per crop**: Maize and Oil palm
# - 3.4 Biodiversity metric data **per magpie class**
# - 3.5 biomass, diversity and enzymatic activity analysis **per magpie class**: Cereals and Oil crops

# After my meeting with Andy on 14/4/22, we decided to input crop / magpie class as fixed effect

# ------------------------------------------------------------------------------

library(blme) # For Bayesian analysis
library(readxl) # For reading the excel data set
library(here) # For specifying file paths
library(knitr) # For making tables
library(kableExtra)# for pretty rmd tables
library(dplyr) # For some data wrangling
library(tidyr) # For some data wrangling
library(ggplot2) # For making cool plots
library(MuMIn) # For model selection

# ==============================================================================

# 1. Take a look at the data

d <- read_excel("../output_files/07.Excel_Dataset_to_model_LRR_LONG.xlsx")

str(d)
d$Crop <- as.factor(d$Crop)
d$Treatment <- as.factor(d$Treatment)
d$magpie_class <- as.factor(d$magpie_class)
d$Weight <- as.numeric(d$Weight)
d$biodiveristy_metric_category <- as.factor(d$biodiveristy_metric_category)

treatment_obs <- as.data.frame(table(d$Treatment))
colnames(treatment_obs) <- c("Treatment", "N_Observations")
treatment_obs <- treatment_obs[order(treatment_obs$N_Observations, decreasing = TRUE),]
kbl(treatment_obs, row.names = FALSE)

# Collapse organic and sustainable
d$Treatment[d$Treatment == "Organic"] <- "Sustainable"

# Include monoculture in conventional 
d$Treatment[d$Treatment == "Monoculture"] <- "Conventional"

# Subset agricultural systems with >= 10 observations
treatment_obs <- as.data.frame(table(d$Treatment))
colnames(treatment_obs) <- c("Treatment", "N_Observations")
treatment_obs <- treatment_obs[order(treatment_obs$N_Observations, decreasing = TRUE),]
treatment_obs <- subset(treatment_obs, treatment_obs$N_Observations >= 10)
d <- d[d$Treatment %in% treatment_obs$Treatment, ] 

# ------------------------------------------------------------------------------

# 1.1 Residuals histogram and qqplot of data without the control zeroes

# Null model with only random effect to see qq plot of weighted residuals 
nonzero <- read_excel("../output_files/06.Excel_Dataset_to_model_LRR_WIDE.xlsx")
m0 <- blmer(LRR ~ 1 + + (1|Paper_ID) + (1|Crop), weights = Weight, data = nonzero)
hist(residuals(m0))
qqnorm(resid(m0))
# Looks fairly normal

# ------------------------------------------------------------------------------

# 1.2 Residuals histogram and qqplot of data with the control zeroes

# Null model with only random effect to see qq plot of weighted residuals 
m1 <- blmer(LRR ~ 1 + (1|Crop) + (1|Paper_ID), weights = Weight, data = d)
hist(residuals(m1))
qqnorm(resid(m1))
# Zero-inflated, but that's all. The zeros won't influence in the output.

# ==============================================================================

### 2. Analysis

# 2.1 Brief intro to method
# I will perform a linear mixed model in a Bayesian environment

# ------------------------------------------------------------------------------

# 2.2 Re-level data set so conventional is the reference category

d$Treatment <- as.factor(d$Treatment)
d$Treatment <- relevel(d$Treatment, ref = "Conventional")

# ------------------------------------------------------------------------------

# 2.3 Fixed effects

# I want to include crop and treatment as fixed effect. 
# I also want to know the mean of each type of biodiversity metric in each type of agricultural system so
# I will create models where I include and exclude the fixed effects and the interaction treatment:biodiversity_metric_category
# and do model selection using the package MuMin.

m0 <- blmer(LRR ~ Treatment + (1|ID), weights = Weight, data = d, control = lmerControl(optimizer ="Nelder_Mead"))
m1 <- blmer(LRR ~ Treatment + Crop + (1|ID), weights = Weight, data = d, control = lmerControl(optimizer ="Nelder_Mead"))
m2 <- blmer(LRR ~  Treatment + biodiveristy_metric_category:Treatment + (1|ID), weights = Weight, data = d, control = lmerControl(optimizer ="Nelder_Mead"), na.action = "na.omit")
m3 <- blmer(LRR ~  Treatment + Crop + biodiveristy_metric_category:Treatment + (1|ID), weights = Weight, data = d, control = lmerControl(optimizer ="Nelder_Mead"), na.action = "na.omit")

model.sel(m0, m1, m2, m3)
r.squaredGLMM(m0)
r.squaredGLMM(m1)
r.squaredGLMM(m2)
r.squaredGLMM(m3)

# According to the model selection table, m0 is the best fit.
# Also looking at the R2, m0 has one of the highest marginal and conditional R2 (although only by a small margin of difference).
# Therefore, I will only include treatment as fixed effect.

# ---------------------------------------------------------------------------------

# 2.4 Random effects

# Since Crop is a clustering factor of the data, I want to see whether including crop as random effect is a better fit for the model.

m0 <- blmer(LRR ~  Treatment + (1|ID), weights = Weight, data = d, control = lmerControl(optimizer ="Nelder_Mead"), na.action = "na.omit")
m4 <- blmer(LRR ~  Treatment + (1|ID) + (1|Crop), weights = Weight, data = d, control = lmerControl(optimizer ="Nelder_Mead"), na.action = "na.omit")

anova(m0, m4)
# Including crop as random effect renders basically the same model fit.

# ---------------------------------------------------------------------------------

# 2.5 Model 

m4 <- blmer(LRR ~  Treatment + (1|ID) + (1|Crop), weights = Weight, data = d, control = lmerControl(optimizer ="Nelder_Mead"), na.action = "na.omit")
summary(m4)
saveRDS(m4, "model.rds") # Functions to write a single R object to a file, and to restore it.
# Saved in replicating christina analysis folder

# Compared to conventional agriculture, biodiversity was significantly higher under conservation agriculture, disturbed forest, primary vegetation and sustainable agriculture.
# Under mixed agriculture, biodiversity was lower relative to conventional agriculture

# -------------------------------------------------------------------------------

# 2.6  Model goodness-of-fit

# Residuals vs Fitted
plot(m4)
# Not good. Non-linear relationship between predictors and Heteroscedasticity.

# Normality of residuals
qqnorm(residuals(m4))
# Actually not that bad. 

# Residuals vs Leverage
ggplot(data.frame( lev = hatvalues(m2), pearson = residuals( m2, type = "pearson")),
       aes(x = lev, y = pearson)) +
  geom_point() +
  theme_bw()

# I do not really know how to interpret this plot.

## scale-location plot, with red smoothed line
scale_loc_plot <- function(m, line.col = "red", line.lty = 1, line.lwd = 2) {
  plot(m4, sqrt(abs(resid(.))) ~ fitted(.),
       type = c("p", "smooth"),
       par.settings = list(plot.line =
                             list(alpha=1, col = line.col,
                                  lty = line.lty, lwd = line.lwd)))
}

scale_loc_plot(m4)
# Again we can see heteroscedasticity.

# -------------------------------------------------------------------------------

# 2.7 Model with Crop as fixed effect

m5 <- blmer(LRR ~ Treatment + Crop + (1|ID), weights = Weight, data = d, control = lmerControl(optimizer ="Nelder_Mead"), na.action = "na.omit")
summary(m5)

# -------------------------------------------------------------------------------

# 2.8 Taxon level

m3 <- blmer(LRR ~ Treatment + Kingdom + (1|ID), weights = Weight, data = d, control = lmerControl(optimizer ="Nelder_Mead"), na.action = "na.omit")
summary(m3)

sum(is.na(d$Phylum))
sum(is.na(d$Kingdom))


# ===============================================================================

### 3. Analysis per biodiversity metric 

# 3.1 Biodiversity metric data barplot

# Make separate data frames for each biodiversity metric category
metric <- unique(d$biodiveristy_metric_category)
metric_list <- list() 
for (i in metric){
  data <- dplyr :: filter( d , biodiveristy_metric_category == i)
  metric_list[[i]] <- data
}

list2env(lapply(metric_list, as.data.frame), .GlobalEnv)

ggplot(d, aes(x = biodiveristy_metric_category, fill = agricultural_system)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# -----------------------------------------------------------------------------

# 3.2 Biodiversity metric data per crop

# Biomass
kable(table(biomass$Crop, biomass$agricultural_system), caption = "Metric: biomass") %>%
  kable_styling(full_width = F, position = "float_left")

# Development
kable(table(development$Crop, development$agricultural_system), caption = "Metric: development") %>%
  kable_styling(full_width = F, position = "right")

# Diversity
kable(table(diversity$Crop, diversity$agricultural_system), caption = "Metric: diversity")

# Efficiency
kable(table(efficiency$Crop, efficiency$agricultural_system), caption = "Metric: efficiency") %>%
  kable_styling( full_width = F, position = "float_left")

# Enzymatic activity 
kable(table(Enzymatic_activity$Crop, Enzymatic_activity$agricultural_system), caption = "Metric: Enzymatic_activity") %>%
  kable_styling( full_width = F, position = "right")

# Reproduction
kable(table(reproduction$Crop, reproduction$agricultural_system), caption = "Metric: reproduction") %>%
  kable_styling( full_width = F, position = "float_left")

# Survival
kable(table(survival$Crop, survival$agricultural_system), caption = "Metric: survival") %>%
  kable_styling( full_width = F, position = "right")

# ------------------------------------------------------------------------------

# 3.3 Biomass and diversity analysis per **crop**: Maize and Oil Palm

# An analysis could be performed for: biomass (maize) and diversity (oil palm)

## Biomass: Maize
maize <- subset(d, Crop == "Maize" & biodiveristy_metric_category == "biomass")
unique(maize$Paper_ID)
# There are three papers that provide biomass data from maize crop

model_maize <- blmer(LRR ~  Treatment + (1|ID), weights = Weight, data = maize, control = lmerControl(optimizer ="Nelder_Mead"))
summary(model_maize)

## Diversity: Oil Palm
oil <- subset(d, Crop == "Oil_palm" & biodiveristy_metric_category == "diversity")
unique(oil$Paper_ID)
# There are 6 papers that provide diversity data for oil palm crop

model_oil <- blmer(LRR ~  Treatment + (1|ID), weights = Weight, data = oil, control = lmerControl(optimizer ="Nelder_Mead"))
summary(model_oil) 

# -----------------------------------------------------------------------------

# 3.4 Biodiversity metric data per magpie class

# Biomass
kable(table(biomass$magpie_class, biomass$agricultural_system), caption = "Metric: biomass")  %>%
  kable_styling( full_width = F, position = "float_left") 

# Development
kable(table(development$magpie_class, development$agricultural_system), caption = "Metric: development") %>%
  kable_styling( full_width = F, position = "right")

# Diversity
kable(table(diversity$magpie_class, diversity$agricultural_system), caption = "Metric: diversity") %>%
  kable_styling( full_width = F, position = "right")

# Efficiency
kable(table(efficiency$magpie_class, efficiency$agricultural_system), caption = "Metric: efficiency") %>%
  kable_styling( full_width = F, position = "float_left")

# Enzymatic activity
kable(table(Enzymatic_activity$magpie_class, Enzymatic_activity$agricultural_system), caption = "Metric: Enzymatic_activity") %>%
  kable_styling( full_width = F, position = "right")

# Reproduction
kable(table(reproduction$magpie_class, reproduction$agricultural_system), caption = "Metric: reproduction") %>%
  kable_styling( full_width = F, position = "float_left")

# Survival
kable(table(survival$magpie_class, survival$agricultural_system), caption = "Metric: survival") %>%
  kable_styling( full_width = F, position = "right")

# An analysis could be performed for biomass (cereals and oil crops), diversity (oil crops) and enzymatic activity (oil crops)

# -------------------------------------------------------------------------------

# 3.5 Biomass, diversity and enzymatic activity analysis per **magpie class**: cereals and oil crops

## Biomass: Cereals 
cereals <- subset(d, magpie_class == "Cereals" & biodiveristy_metric_category == "biomass")
unique(cereals$Paper_ID)
# There are four papers that provide data for biomass for the cereals magpie class

biomass_cereals <- blmer(LRR ~  Treatment + (1|ID), weights = Weight, data = cereals, control = lmerControl(optimizer ="Nelder_Mead"))
summary(biomass_cereals)


## Biomass: Oil crops
oil_crops <- subset(d, magpie_class == "Oil crops" & biodiveristy_metric_category == "biomass")
unique(oil_crops$Paper_ID)
# There are two papers that provide data for biomass for the oil crops magpie class

biomass_oil_crops <- blmer(LRR ~  Treatment + (1|ID), weights = Weight, data = oil_crops, control = lmerControl(optimizer ="Nelder_Mead"))
summary(biomass_oil_crops)


## Diversity: Oil crops
oil_crops <- subset(d, magpie_class == "Oil crops" & biodiveristy_metric_category == "diversity")
unique(oil_crops$Paper_ID)
# There are nine papers that provide data for diversity for the oil crops magpie class

diversity_oil_crops <- blmer(LRR ~  Treatment + (1|ID), weights = Weight, data = oil_crops, control = lmerControl(optimizer ="Nelder_Mead"))
summary(diversity_oil_crops)


## Enzymatic activity: Oil crops
oil_crops <- subset(d, magpie_class == "Oil crops" & biodiveristy_metric_category == "Enzymatic_activity")
unique(oil_crops$Paper_ID)
# There are two papers that provide enzymatic activity data for the oil crops magpie class

enzymatic_oil_crops <- blmer(LRR ~  Treatment + (1|ID), weights = Weight, data = oil_crops, control = lmerControl(optimizer ="Nelder_Mead"))
summary(enzymatic_oil_crops)






