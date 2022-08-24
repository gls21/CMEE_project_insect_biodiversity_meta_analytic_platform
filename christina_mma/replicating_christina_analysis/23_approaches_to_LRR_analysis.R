# Grace Skinner 18/5/22
# Script based on Christina Raw's NHM_MMA/Scripts/23.Approaches_to_LRR _Analysis.Rmd

# Christina's script was an R markdown file

# In this script I am going to analyse biodiversity response to agricultural practices using the log response ratio data. 
# This data comes either directly from log response ratio results of meta-analyses,
# or from back transforming percentage change data into log response ratio. 

# -----------------------------------------------------------------------------

rm(list=ls())
gc()

library(readxl)
library(here)
library(ggplot2)
library(lme4)
library(fitdistrplus)
library(blme)

d <- read_xlsx(here("christina_mma", "output_files", "07.Excel_Dataset_to_model_LRR_LONG.xlsx"))
# Different to file Christina uses in her script??

### Without using here:
d2 <- read_xlsx("../output_files/07.Excel_Dataset_to_model_LRR_LONG.xlsx")

# ------------------------------------------------------------------------------

### Distribution

hist(d$LRR, xlim = c(-4.477337, 2.586478), xlab = "LRR", main = "Histogram of LRR" )
descdist(d$LRR, discrete = FALSE)
qqnorm(d$LRR)

# The data has a zero-inflated Normal/Gaussian distribution
# Due to BD not differing between control and treatment in 200 cases
# And adding 349 0 LRR control values 

100*sum(d$LRR == 0)/nrow(d) # Sp 77.5% of data is 0 

# -------------------------------------------------------------------------

### Option 1: Heavy-tailed distribution

# It does not work because there aren't enough observations for certain groups. 
# However, I don't think the problem is heavy tailed, but zero inflation.

# ------------------------------------------------------------------------

### Option 2: Bayesian approach with blmer function in blme package

m <- blmer(LRR ~ agricultural_system + magpie_class + biodiveristy_metric_category + (1|Crop) + (1|ID), data = d)

summary(m)
plot(m)

m <-blmer(formula, data = NULL, REML = TRUE,
          control = lmerControl(), start = NULL, verbose = 0L,
          subset, weights, na.action, offset, contrasts = NULL,
          devFunOnly = FALSE, cov.prior = wishart,
          fixef.prior = NULL, resid.prior = NULL, ...) # gives error

# ----------------------------------------------------------------------------

### Option 3: hurdle models
# Package glmTMB for zero-inflated generalised linear mixed modelling 
# ZIGMMs = Zero-inflated gaussian mixed models 

# ----------------------------------------------------------------------------

### Option 4: linear models?

m <- lm(LRR ~ Treatment, data = d )
summary(m)
plot(m)

m1 <- lmer(LRR ~ Treatment + magpie_class + biodiveristy_metric_category + (1|Crop) + (1|ID), data = d )
summary(m1)
plot(m1)

m2 <- lmer(LRR ~ 1 + (1|Treatment), data = d)
summary(m2)

unique(d$Treatment)

treatment <- subset(d, Treatment != "Control")
qqnorm(d$LRR)

# model diagnostic plots indicate that this is not the best fit

# -----------------------------------------------------------------------------

# Conclusion

# The zero inflation an artifact of the data set structure, caused by adding all the control data which is = 0. 
# The zero inflation is not actually biologically caused, it is only a consequence of the data set structure. 
# Therefore, the hurdle model does not make sense biologically. 
# If it were, for example, presence (=1) absence (= 0) data, then a hurdle model would make sense biologically.
# In my case, the zeroes are not true zeroes, true data.

# Furthermore, the zero inflation should not influence the output.
# Imagine a random value is added to all the data, then the zero-inflation problem would be solved, 
# but the model output would still remain the same. 
# The zero-inflation is not actually causing any deviations to the results. 

# Next steps = Analysis with the Bayesian approach Andy suggested.



















