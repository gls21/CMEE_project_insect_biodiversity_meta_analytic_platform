# Grace Skinner 24/5/22
# Script based on Christina Raw's NHM_MMA/Scripts/05.Analysis/28. Model_diagnostic_with_influence.ME_and_Robust_analysis.Rmd
# Input: 07.Excel_Dataset_to_model_LRR_LONG.xlsx
# Output: robust_model.rds
#         sample_sizes.csv

# This script will run a robust model on the LONG format LRR dataset 

## Contents
#   1. Load packages and dataset 
#   2. Run robust models, tune the fit, and plot diagnostics
#   3. Save robust model and sample size table 
 
# ================================================================================

# 1. Load packages and dataset 

library(readxl)
library(tidyverse)
library(robustlmm)

d <- read_excel("../output_files/07.Excel_Dataset_to_model_LRR_LONG.xlsx")

# Re level data set so conventional is the reference category
d$Treatment <- as.factor(d$Treatment)
d$Treatment <- relevel(d$Treatment, ref = "Conventional")

# Get table of sample sizes - how many instances do we have of each agricultural system?
sample_sizes <- as.data.frame(table(d$Treatment))
colnames(sample_sizes) <- c("Treatment", "Frequency")

# Remove as won't work when new MAs are added and new agricultural systems exist 
# sample_sizes$Treatment <- c("Conventional", "Conservation", "Disturbed forest",
#                                     "Fallow", "Mixed", "Non Bt", "Primary vegetation",
#                                     "Sustainable", "Traditional", "Transgenic")

# ------------------------------------------------------------------------------

# 2. Run robust models, tune the fit, and plot diagnostics

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

### Tuning the fit:

# The estimates of ?? and ?? have a low efficiency if the same tuning parameters are used 
# as for estimating the fixed and random effects. 
# To get a higher efficiency, we have to increase the tuning parameter of ??(??) e and ??(??). 
# We use the update function to fit a model with a higher efficiency for the estimates of ?? and ??

robust2 <- update(robust_model, rho.sigma.e = psi2propII(smoothPsi, k = 2.28), 
                  rho.sigma.b = psi2propII(smoothPsi, k = 2.28))

summary(robust2)

# To fit only the element of ?? that corresponds to the "sample" random effect with higher efficiency, we use:
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

################# Just run the best model (so don't have to run all 3 models everytime)
rsb <- list(psi2propII(smoothPsi), psi2propII(smoothPsi, k = 2.28))
best_robust <- rlmer(LRR ~  Treatment + (1|ID) + (1|Crop), data = d, 
                     rho.sigma.e = psi2propII(smoothPsi, k = 2.28),
                     rho.sigma.b = rsb)


### Conclusion
# Only the influential observations for disturbed forest affected the significance of the estimate. 
# However, the data was correct. Therefore, I will not remove the influential observations for the disturbed forest, 
# as the data are correct, or the rest of the influential observations, 
# as they do not affect the significance of the estimates. 

# As for the robust package, it looked like it improved the qq plot and residuals vs fitted plots, 
# therefore I will use the robust model. 

# ------------------------------------------------------------------------------------

# 3. Save robust model and sample size table 
saveRDS(best_robust, "../output_files/robust_model.rds")
write.csv(sample_sizes, "../output_files/sample_sizes.csv")


