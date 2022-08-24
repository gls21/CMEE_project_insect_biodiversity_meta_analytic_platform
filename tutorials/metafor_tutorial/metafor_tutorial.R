# Random effects model metafor demo 

# install metafor package
# install.packages("metafor")

library(metafor)

# look at bcg vaccines against TB dataset
dat.bcg

# data are in the form of 2x2 tables:
#
#           TB+   TB-
# treated   tpos  tneg
# control   cpos  cneg

# calculate log risk ratios (RR) and corresponding sampling variances
# 4 variables as input 
# slab adds study labels which will help when we make forest plot
dat <- escalc(measure="RR", ai=tpos, bi=tneg,
                            ci=cpos, di=cneg,
              slab=paste(author, ", ", year, sep=""), data=dat.bcg)

dat # yi are the log(RR), vi the corresponding variances 
# negative yi means risk is lower in treatment group 

# random-effects model
res <- rma(yi, vi, data=dat)
# rma() is same as rma.uni(). Think random-effects model is default 
res

# tau^2 is estimate of variance in true effects (0.31)

# test for heterogeneity - p-value<0.0001 so there is significant heterogeneity between studies
# So true effectiveness of vaccine differs across studies 

# Also get estimate of average effectiveness across all studies (-0.71) and CIs

# Back-transform log risk ratio by exponentiation it to get a risk ratio
# predict pooled risk ratio and CI
predict(res, transf=exp, digits=2)
# 0.49 so on average vaccinated people have about half the risk of infection as non-vaccinated people


# Make forest plot
forest(res)
forest(res, addpred=TRUE, header=TRUE) # add header
print(forest(res, addpred=TRUE, header=TRUE)) # see default xlim axis limits for eg
forest(res, addpred=TRUE, header=TRUE, xlim=c(-8,6)) # less white space
forest(res, addpred=TRUE, header=TRUE, xlim=c(-8,6), atransf=exp) # x axis transformation 
forest(res, addpred=TRUE, header=TRUE, xlim=c(-8,5), atransf=exp, at=log(c(.05, .25, 1, 4))) # specify where to place tick marks

# funnel plot 
funnel(res) # lowest value on y axis is at top
# expect up-side-down funnel shape given little heterogeneity and no publication bias
funnel(res, ylim=c(0,.8), las=1) # specify y axis limits and tickmarks, and display them horizontally 
funnel(res, ylim=c(0,.8), las=1, digits=list(1L,1)) # to make it 0.0, not just 0 - don't drop trailing zeros.


## New dataset about vaccines against cholera
# calculate log odds ratios and corresponding sampling variances for
# the meta-analysis on the effectiveness of vaccines against cholera
dat.graves2010

dat <- escalc(measure="OR", ai=ai, n1i=n1i,
              ci=ci, n2i=n2i, data=dat.graves2010)
dat

# random-effects model
res <- rma(yi, vi, data=dat)
res
predict(res, transf=exp, digits=2) # back-transform via exponentiation

# contour-enhanced funnel plot
funnel(dat$yi, dat$vi, yaxis="seinv",
       xlim=c(-3,2), ylim=c(.00001,8), xaxs="i", yaxs="i", las=1,
       level=c(.10, .05, .01), shade=c("white", "gray55", "gray75"), # add regions to illustrate the significance of studies
       legend=TRUE, back="gray90", hlines=NULL, ylab="Precision (1/se)")


## new dataset
# meta-analysis on the risk of lung cancer in women exposed to environmental
# tobacco smoke (ETS) from their smoking spouse (yi are log odds ratios)
dat.hackshaw1998 # already has yi and vi
res <- rma(yi, vi, data=dat.hackshaw1998)
res
predict(res, transf=exp, digits=2) # women have on average a 24% higher odds of lung cancer if have smoking spouse 
funnel(res, ylim=c(0,.8), las=1, digits=list(1L,1))
# appears to be missing points on bottom left which could show publication bias

# trim and fill method to fill in these gaps - input missing studies
funnel(trimfill(res), las=1, ylim=c(0,.8), digits=list(1L,1), legend=TRUE)
trimfill(res) # slightly lower than before 


## new dataset 

# meta-analysis on the correlation between employment interview assessments performance
# and job performance (using r-to-z transformed correlation for the analysis)
dat.mcdaniel1994

dat <- escalc(measure="ZCOR", ri=ri, ni=ni, data=dat.mcdaniel1994)
res <- rma(yi, vi, data=dat)
res
predict(res, transf=transf.ztor) # back-transform
# these 2 things correlate by about 0.23. 

# Here we have lots of studies 
# outlier/influence diagnostics
par(mar=c(5,6,4,2))
plot(influence(res), cex=0.8, las=1)
# e.g. covariance ratio tells you how studies affect precision of model - here 2 studies decrease precision by 10-15%



## Go back to bcg dataset 

# Baujat plot - look at influence of studies 
# Top right = outliers and influential
# Bottom left = not outliers and not influential 
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg,
              slab=paste(author, ", ", year, sep=""), data=dat.bcg)
res <- rma(yi, vi, data=dat)
baujat(res, bty="l", xlim=c(0,2), ylim=c(0,.25))


# cumulative meta-analysis - looking at evolution of evidence over time
sav <- cumul(res, order=dat$year)
sav
forest(sav, xlim=c(-5,2.5), header=TRUE)
# 1st one is just 1 study, 2nd one is the 1st study plus the 2nd one and so on 



## Back to lung cancer risk dataset 
res <- rma(yi, vi, data=dat.hackshaw1998)

# radial plot/Galbraith plot
radial(res)
# x axis = precision of estimates
# y axis = z scores, significance scores 
# not widely used 


## New dataset
# meta-analysis on the effectiveness of wrist acupuncture point P6 stimulation
# for preventing postoperative nausea
# uses risk ratio 
res <- rma(measure="RR", ai=ai, n1i=n1i, ci=ci, n2i=n2i, data=dat.lee2004)
res
predict(res, transf=exp, digits=2)

# L'Abbé plot
labbe(res, transf=exp, bty="l")
# plots treated vs untreated groups
# dark line shows when risk is the same in the 2 groups 
# dashed line is the relationship we find - i.e. risk lower in treatment group



## New dataset

# meta-analysis based on 20 hypothetical trials to examine the effectiveness
# of a particular treatment/medication
dat <- escalc(measure="OR", ai=xTi, n1i=nTi, ci=xCi, n2i=nCi,
              add=1/2, to="all", data=dat.viechtbauer2021)
dat

# Specify this is a fixed effect model 
res <- rma(yi, vi, data=dat, method="FE")
res

# GOSH plot
sav <- gosh(res, subset=20000) # run meta-analysis using 20000 random subsets of the data
plot(sav, out=6, xlim=c(-0.25,1.25), breaks=100, hh=0.2)
# points represent MA result. Colour (red or blue) is based on whether study 6 is included or excluded
# Big difference between when included or not 


## Back to bcg dataset

# mixed-effects meta-regression model with absolute latitude and type of
# allocation as moderators/predictors
# latitude is continuous
# allocation is categorical 
dat.bcg
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg,
              slab=paste(author, ", ", year, sep=""), data=dat.bcg)
res <- rma(yi, vi, mods = ~ ablat + alloc, data=dat)
res


### reporter function
# generates an analysis report for you - uses rmarkdown 
# template for writing up results

dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg,
              slab=paste(author, ", ", year, sep=""), data=dat.bcg)
res <- rma(yi, vi, data=dat)
res

reporter(res) # rmarkdown file
reporter(res, format="pdf")
reporter(res, format="word")

# make study 6 an outlier and see how it changes the analysis output
dat$yi[6] <- 2.5
res <- rma(yi, vi, data=dat)
reporter(res)

# can't do this with meta-regression











