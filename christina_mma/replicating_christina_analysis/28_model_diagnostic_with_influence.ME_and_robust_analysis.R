# Grace Skinner 18/5/22
# Script based on Christina Raw's NHM_MMA/Scripts/28. Model_diagnostic_with_influence.ME_output_images.Rmd

# Christina's script was an R markdown file

# Since it looked like there were influential observations in the model, 
# in this script I am going to assess the presence of influential observations and groups 
# using the package influence.ME.

# ------------------------------------------------------------------------------

library(influence.ME)
library(readxl)
library(here)
library(tidyverse)
library(robustlmm)
library(blme)

# ------------------------------------------------------------------------------

# 1. Prepare data set

d <- read_excel("../output_files/07.Excel_Dataset_to_model_LRR_LONG.xlsx")

d$Crop <- as.factor(d$Crop)
d$Treatment <- as.factor(d$Treatment)
d$magpie_class <- as.factor(d$magpie_class)
d$Weight <- as.numeric(d$Weight)
d$biodiveristy_metric_category <- as.factor(d$biodiveristy_metric_category)

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

#Re level data set so conventional is the reference category
d$Treatment <- as.factor(d$Treatment)
d$Treatment <- relevel(d$Treatment, ref = "Conventional")

# ------------------------------------------------------------------------------

# 2. Model

m4 <- blmer(LRR ~  Treatment + (1|ID) + (1|Crop), weights = Weight, data = d, control = lmerControl(optimizer ="Nelder_Mead"), na.action = "na.fail")

# ------------------------------------------------------------------------------

# 3. Test influential data

# I tested for influential data using the package influence.ME and following the tutorial provided by Nieuwenhuis, R., *et al.* (2012) <https://journal.r-project.org/archive/2012/RJ-2012-011/RJ-2012-011.pdf>
# To  detect influential data in generalized mixed effects models, one has to measure the influence of a particular higher level group
# on the estimates of a predictor measured at that level. 
# The straightforward way is to delete all observations from the data that are nested within a single higher level group, 
# then re-estimate the regression model, and finally evaluate the change in the estimated regression parameters. 
# This procedure is then repeated for each higher-level group separately. 
# The influence function in the influence.ME package performs this procedure automatically, 
# and returns an object containing information on the parameter estimates excluding the influence of each higher level group separately. 

estex.m4 <- influence(m4, group = "ID")

# DFBETAS is a standardized measure that indicates the level of influence observations have on single parameter estimates (Fox, 2002). 
# Regarding mixed models, this relates to the influence a higher-level unit has on the parameter estimate.
# DFBETAS is calculated as the difference in the magnitude of the parameter estimate between the model including and the model excluding the higher level case. 
# Values exceeding a cut-off value are regarded as overly influencing the regression outcomes for that specific estimate.

# Obtain dfbetas values
dfbetas <- as.data.frame(dfbetas(estex.m4))

# Obtain dfbetas cutoff value. Dfbetas larger than this value are influential observations
2/sqrt(length(unique(d$ID)))  # 0.1070575

# Filter the dfbetas values for influential observations 
influential_dfbetas <- dfbetas %>% filter_all(any_vars(. > 0.1070575))
influential_dfbetas[influential_dfbetas < 0.1070575] <- "NA" 

# Select the columns that have influential data.
influential_dfbetas <- dplyr::select(influential_dfbetas, "(Intercept)", TreatmentConservation,
                              TreatmentDisturbed_forest, Treatmentmixed,
                              TreatmentPrimary_vegetation)


# ![](Images/dfbetas_dataframe.PNG) # This is an R markdown thing to get a direct link to something
# This will just show the plot that is produced with the below line of code 


plot(estex.m4, which="dfbetas", parameters = c(1, 2, 3, 5, 7), xlab="DFbetaS", ylab="ID")

### Cook's distance

# The measure of Cook’s distance allows to determine the influence a single higher-level group
# has on the estimates of multiple variables simultaneously. 
# Cases are regarded as too influential if the associated value for Cook’s Distance exceeds the cut-off value of: 
# 4/n in which n refers to the number of groups in the grouping factor under evaluation.

# Obtain Cook's distance values
cook <- cooks.distance(estex.m4, sort=TRUE)

# Obtain Cook's distance cutoff values. Values larger than this value are influential observations
4/length(unique(d$ID)) # 0.01146132

plot(estex.m4, which = "cook", cutoff = 0.01146132, sort = TRUE, xlab = "Cook´s Distance", ylab = "Observation ID") 
# In red are the the observations that are influential on the model.


### Testing for Changes in Statistical Significance (sigtest)

# The sigtest function is used to test for changing levels of significance after deletion of each of the 349 ID groups. 
# In this case I will apply it on the treatments that showed influential data
# When Change.Sig = TRUE, the significance of the estimate for that agricultural system changed when removing the influential observations.

# Intercept (conventional)
sigtest(estex.m4, test=1.96)$Intercept %>% subset( Changed.Sig == "TRUE")

# Conservation
sigtest(estex.m4, test=1.96)$TreatmentConservation %>% subset(Changed.Sig == "TRUE")

# Mixed
sigtest(estex.m4, test=1.96)$Treatmentmixed %>% subset(Changed.Sig == "TRUE")

# Disturbed forest 
sigtest(estex.m4, test=1.96)$TreatmentDisturbed_forest %>% subset(Changed.Sig == "TRUE")
# When the influential observations are removed, the significance of disturbed forest changes from significant to non-significant. 
# The blme t-test estimate is 2.131, whereas here it lowers below 1.96. 
# I went back to the data set to see these observations. 
# It is true that the values for those observations are high, but the data is correct. 
# So I would not want to remove them.

# Primary vegetation 
sigtest(estex.m4, test=1.96)$TreatmentPrimary_vegetation %>% subset(Changed.Sig == "TRUE")


# ----------------------------------------------------------------------------------

### Robust package

# I tried fitting the model using the robustlmm package (Koller, M., 2016) 
# and following the example provided in the reference <https://cran.r-project.org/web/packages/robustlmm/vignettes/rlmer.pdf>. 
# The robustlmm package robustly fits a linear mixed-effects model 
# by fitting a random effects contamination model that accounts for 
# potential contamination or outliers on different sources of variability. 

# Robust model
robust_model <- rlmer(LRR ~  Treatment + (1|ID) + (1|Crop), data = d)
summary(robust_model)
# The estimates values are quite strange.

plot(robust_model)


## Tuning the fit - not sure I really understand the following bits:

# The estimates of σ and θ have a low efficiency if the same tuning parameters are used 
# as for estimating the fixed and random effects. 
# To get a higher efficiency, we have to increase the tuning parameter of ρ(σ) e and ρ(σ). 
# We use the update function to fit a model with a higher efficiency for the estimates of σ and θ

robust2 <- update(robust_model, rho.sigma.e = psi2propII(smoothPsi, k = 2.28), 
                  rho.sigma.b = psi2propII(smoothPsi, k = 2.28))

summary(robust2)

# To fit only the element of θ that corresponds to the “sample” random effect with higher efficiency, we use:
rsb <- list(psi2propII(smoothPsi), psi2propII(smoothPsi, k = 2.28))
robust3 <- update(robust2, rho.sigma.b = rsb)

# To compare the estimates of the various fits we did so far, we can use the function compare
compare(robust_model, robust2, robust3, show.rho.functions = FALSE)

# We can see the estimates and standard errors (in parenthesis) of the variables 
# when increasing the efficiency of the robust model. 
# The estimates change vary from the ones obtained in the original model, which is the blme model: 

### Robust plots

plot(robust2)
plot(robust3)
# It seems that increasing the efficiency of the robust model
# brings the residuals closer to a mean of zero and improves the qq-plot. 


### Conclusion
# Only the influential observations for disturbed forest affected the significance of the estimate. 
# However, the data was correct. Therefore, I will not remove the influential observations for the distubed forest, 
# as the data are correct, or the rest of the influential observations, 
# as they do not affect the significance of the estimates. 

# As for the robust package, it looked like it improved the qq plot and residuals vs fitted plots, 
# therefore I will use the robust model. 

### Save models to work with them
saveRDS(m4, "../output_files/blmer_model.rds")
saveRDS(robust3, "../output_files/robust_model.rds")


