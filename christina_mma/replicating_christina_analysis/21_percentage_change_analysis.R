# Grace Skinner 17/5/22
# Script based on Christina Raw's NHM_MMA/Scripts/21.Analysis_clean.Rmd
# Christina's script was an R markdown file

# In this script I am going to analyse how biodiversity responds to different agricultural practices. 
# The dataset consists of measures of biodiversity under different agricultural systems and crops.

# -------------------------------------------------------------------

### Model components:
# y: continuous, biodiversity quantitative measure 
# x: categorical, agricultural system. Factor with 8 levels: conservation, conventional, ipm, mixed, organic, traditional, transgenic and unclassified. 

# The response of biodiversity to agriculture varies across taxonomic and functional groups 
# and is mediated by a wide array of variables, such as geographical location, spatial scale, level of intensification or proximity to natural habitat, to name a few. 

# Random effects*
# To test variation in biodiversity response across locations and taxa: geographical location, taxon and functional group
# To test variations in biodiversity response caused by to detailed practices: cover crop type, bt type, application of fertilizers/pesticides/herbicides 
# To control for methodology: sampling method
# To control for variation caused by data nested structure: Paper ID


### Model limitations
# Heterogeneous data set
# Small sample size: 
# General consensus of 10 observation per term included in the model.
# There is not enough data to confidently test relationships between agricultural practices and biodiversity, 
# let alone test variance explained by random effects -> over fitting


### Approaches
# 1. Collapsing categories to increase sample size
# This does not make sense as the goal of this project is to assess how the different agricultural practices affect biodiversity.

# 2. From maximal to simplified model 
# I do not have enough data to generate a maximal model

# 3. From simplified model to (if needed) a more complex model
# I am going to test the random effects one by one and check the goodness of fit of each model. 
# Once I know which variables are relevant, I will build a maximal model and test its goodness of fit. 
# However, I will not be able to trust the maximal model output as I do not have enough data.


# ------------------------------------------------------------------

library(readxl) # To read excel files
library(here) # To fetch files 
library(lme4) # To run more complex linear models
library(dplyr) # To handle data
library(lmtest) # To perform likelihood ratio tests
library(knitr) # To make tables

here()
d <- read_xlsx(here("christina_mma", "output_files", "05.Excel_Percentage_Dataset_to_model.xlsx")) 
# Not sure why here() is superior here 

# ------------------------------------------------------------------

### Factorize the categorical variables I will use in the model.
str(d)
unique(d$biodiveristy_metric_category)
d$agricultural_system <- as.factor(d$agricultural_system)
d$Crop <- as.factor(d$Crop)
d$magpie_class <- as.factor(d$magpie_class)
d$Phylum <- as.factor(d$Phylum)

unique(d$Crop) 
# Change Bt_CropName into just CropName

# Probably a more efficient way of doing this?
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

# Some NAs produced

# -----------------------------------------------------------------

### Subset per data type: DIVERSITY DATA
unique(d$biodiveristy_metric_category) 
# Subset diversity data
diversity <- subset(d, d$biodiveristy_metric_category == "diversity")
hist(diversity$Intervention_Percentage_Change)

### Model
# Basic lm model
m0 <- lm(Intervention_Percentage_Change ~ agricultural_system, data = diversity)
summary(m0)
plot(m0)
# Residuals vs. fitted not great and Q-Q plot indicates extreme tails of data, another distribution model could potentially fit better

# Tukey test
m0.aov <- aov(m0)
tukey.test <- TukeyHSD(m0.aov)
tukey.test

### Test random effects

# Paper ID
m1 <- lmer(Intervention_Percentage_Change ~ agricultural_system + (1|Paper_ID), data = diversity)
summary(m1) 
# Paper ID takes up a lot of the variance, probably because as the papers are syntheses, they probably report data on many different species. 
# So, does including paper_ID as random effect make sense?

# Crop
m2 <- lmer(Intervention_Percentage_Change ~ agricultural_system + (1|Crop), data = diversity)
summary(m2) 
# Christina concludes crop explains 25% of variance. I think it's more like 40%?

# Phylum 
m3 <- lmer(Intervention_Percentage_Change ~ agricultural_system + (1|Phylum), data = diversity)
summary(m3) 
# Christina concludes phylum explains more than crop (36%) but I think it explains less (still 36% though)?

### Lots of within group variance because the dataset is heterogeneous

# ------------------------------------------------------------------------------------------------

### Does including random effects improve fit? - LRTs
m1 <- lmer(Intervention_Percentage_Change ~ agricultural_system + (1|Paper_ID), data = diversity)
m2 <- lmer(Intervention_Percentage_Change ~ agricultural_system + (1|Paper_ID) + (1|Crop), data = diversity)
lrtest(m1, m2)
# Doesn't work due to the variance of a random effect being 0 from crop?

m1 <- lmer(Intervention_Percentage_Change ~ agricultural_system + (1|Paper_ID), data = diversity)
m2 <- lmer(Intervention_Percentage_Change ~ agricultural_system + (1|Paper_ID) + (1|Phylum), data = diversity)
lrtest(m1, m2)
# Doesn't work due to NAs

# -------------------------------------------------------------------------------------

### GLM
# Due to data looking like it had a poisson distribution
hist(diversity$Intervention_Percentage_Change)

diversity$abundance <- round(diversity$Intervention_Percentage_Change, digits = 0) # Because poisson is integer

g0 <- glm(abundance ~ agricultural_system, family = "poisson", data = diversity)
summary(g0)

dispar <- 2606.1/150 # residual deviance / number of DoF associated with this
dispar # 17.374
# Very over-dispersed, not good.

# ----------------------------------------------------------------------

## Can't mix all data together because they have different distributions?

# Diversity data has the following data types:
kable(table(diversity$Biodiversity_measure))

# For example, abundance has a poisson distribution, 
# whereas species richness can have a normal distribution. 
# Therefore, I have to apply different models to each type of data.

## Abundance N=64
# Normally poisson but not sure because abundance comes from transforming the percentages
abundance <- subset(diversity, Biodiversity_measure == "Abundance") # Make abundance data set
hist(abundance$Intervention_Percentage_Change)
# This doesn't look like poisson anyway 
# Try out glmm
abundance$abundance <-  round(abundance$Intervention_Percentage_Change, digits = 0) # Because poisson is integer
g0 <- glm(abundance ~ agricultural_system, family = "poisson", data = abundance)
summary(g0)
# Very strange values and very over dispersed...not good.
# Try basic lm 
m1 <- lm(Intervention_Percentage_Change ~ agricultural_system, data = abundance)
summary(m1)
plot(m1)
# Bad diagnostic plots
# So what is the distribution?

# ----------------------------------------------------------------------

### Conclusions...
# What is the data type and distribution?
# Try to fix glm model for all diversity data set, keep investigating
# Need more data
# Fill out NAs
# Repeat for coarse analysis. Might be able to do more things with increased sample size
# Question: can I add the control data to the treatment data and just treat it as more data?
