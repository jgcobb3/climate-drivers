############### Climate perception project ########## 
## SESYNC Research Support
##
##Data used in this project comes from three sources: (1) A survey of NRCS and FSA employees conducted by the USDA Climate Hubs in 2016/2017, (2) Crop idemnity payments made by FSA to farmers for weather-related 
##crop loss between 2013-2016, and (3) mean and standard deviations of drought data over 1, 3, 5, 10, and 15 year periods.
##
##
## DATE CREATED: 08/08/2019
## DATE MODIFIED: 
## AUTHORS: Rachel Schattman, Benoit Parmentier  
## Version: 1
## PROJECT: Climate Percecption
## ISSUE: 
## TO DO:



######## Load packages 
library(margins)
library(ggeffects)
library(sjmisc)

library("rstanarm")

####### Create path
source(file.path(script_path,bayes_ordinal_logistic_model.R))


######## Good resources
# https://osf.io/gyfj7/download
# https://cran.r-project.org/web/packages/ggeffects/vignettes/ggeffects.html 
# https://cran.r-project.org/web/packages/ggeffects/vignettes/introduction_plotmethod.html

### using plotly
library(plotly)
plot1 <- plot_ly(dataDR, x = ~PDSI_MEAN_2002, color = ~y_var, type = "violin")

# This creates nice interactive plots that are not substantially different than the ones I made using ggerrorplot last year - they all 
# use the actual data rather than the polr model output. If we want to include any voilin plots, I think the ones I made last year are better.
# though plotly is much easier to use than ggerrorplot!

### using rstanarm
# http://mc-stan.org/rstanarm/articles/rstanarm.html 
y_rep <- posterior_predict(list_mod[[1]])
dim(y_rep) #columns are the number of observations in the data set, rows are the posterior draws
PlotDat <- dataDR # create copy so original data frame is protected
y_rep_DF <- as.data.frame(y_rep) 

plot(y_rep)

Plot_MEAN2016 <- boxplot(y_rep_DF)

### using marginal effects

plot1 <- marginal_effects(list_mod[[1]], data=list_mod[[1]]$data, categorical = TRUE)


  
marginal_effects(mod, "dependent variable", categorical = TRUE)
### End Script

