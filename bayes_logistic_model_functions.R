############### Climate perception project ########## 
##
##
#Data used in this project comes from three sources: (1) A survey of NRCS and FSA employees conducted by the USDA Climate Hubs in 2016/2017, (2) Crop idemnity payments made by FSA to farmers for weather-related 
#crop loss between 2013-2016, and (3) mean and standard deviations of drought data over 1, 3, 5, 10, and 15 year periods.
##
## This is an example script using mixed models to test differences in time series
## for different locations/subjects. Autocorrelation structures is taken into account
## in the methods.
##
## DATE CREATED: 09/27/2018
## DATE MODIFIED: 10/23/2018
## AUTHORS: Rachel Schattman, Benoit Parmentier  
## Version: 1
## PROJECT: Climate Percecption
## ISSUE: 
## TO DO:
##
## COMMIT: modifying code
##

## Very good reference:
#http://rpsychologist.com/r-guide-longitudinal-lme-lmer

###################################################
#

###### Library used

## ------------------------------------------------------------------------
library(MASS)
library(lme4)
library(rstanarm)
library("bayesplot")
library("ggplot2")
library("loo")
library("parallel")

##########

###### Functions used in this script and sourced from other files

run_model_assessment <- function(mod,model_type="bayes_stan",k_threshold = 0.7){
  
  if (model_type!="bayes_stan") {
    stop("Model type not implemented")
  }
  
  if(model_type=="bayes_stan"){
    debug(loo)
    mod$formula
    loo_obj <- try(loo(mod, k_threshold = k_threshold))
  }
  
  return(loo_obj)
}

run_model_ordinal_logistic <- function(model_formula,model_type="bayes_stan",data,prior = NULL, prior_counts = dirichlet(1),
                                       shape = NULL,chains = 4, num_cores = NUL, seed_val = 1234, iter_val = 200){
  
  if (model_type!="bayes_stan") {
    stop("Model type not implemented")
  }
  
  if(model_type=="bayes_stan"){
    mod <- try(stan_polr(formula=model_formula,
                         data = data_subset, 
                         prior = prior, 
                         prior_counts = dirichlet(1),
                         shape = shape,
                         chains = chains, 
                         cores = num_cores, 
                         seed = seed_val, 
                         iter = iter_val))
  }
  
  return(mod)
}


################################# End of script ######################################
