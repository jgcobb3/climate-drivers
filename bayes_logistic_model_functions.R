############### Climate perception project ########## 
##
##
#Data used in this project comes from three sources: (1) A survey of NRCS and FSA employees conducted by the USDA Climate Hubs in 2016/2017, (2) Crop idemnity payments made by FSA to farmers for weather-related 
#crop loss between 2013-2016, and (3) mean and standard deviations of drought data over 1, 3, 5, 10, and 15 year periods.
##
##
## DATE CREATED: 09/27/2018
## DATE MODIFIED: 11/27/2018
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
  
  ## This function provides a model assessment for different options to run ordinal logistic models. 
  #Option includes:
  # - polr from the MASS package
  # - loo for stan_polr from the rstanarm package (bayesian model with STAN algorithm)
  #
  ## CREATED ON: 10/01/2018
  ## MODIFIED ON: 10/24/2018
  
  if (model_type!="bayes_stan") {
    stop("Model type not implemented")
  }
  
  if(model_type=="bayes_stan"){
    #debug(loo)
    mod$formula
    loo_obj <- try(loo(mod, k_threshold = k_threshold))
  }
  
  return(loo_obj)
}

run_model_ordinal_logistic <- function(model_formula,model_type="bayes_stan",data,prior = normal(location = 0, scale = NULL, autoscale = TRUE), prior_counts = dirichlet(1),
                                       shape = NULL,chains = 4, num_cores = NULL, seed_val = 1234, iter_val = 200){ #11/20 changed prior from NULL to normal

  ## This function provides different options to run ordinal logistic models. Option includes:
  # - polr from the MASS package
  # - stan_polr from the rstanarm package (bayesian model with STAN algorithm)
  #
  ## CREATED ON: 09/30/2018
  ## MODIFIED ON: 10/24/2018
  #
  #INPUTS:
  #1)model_formula: string defining the formula to run
  #2)model_type: "plor" or "bayes_stan"
  #3)data: input data as data.frame
  #4prior = NULL # This should be NORMAL
  #5)prior_counts = dirichlet(1)
  #6)shape = NULL
  #7)chains = 4
  #8)num_cores = NUL
  #9)seed_val = 1234
  #10)iter_val = 200
  #OUTPUTS
  #mod: model object that can be of different types.
  
  ###### Begin #####
  
  if (model_type=="polr"){
    stop("Model type not implemented")
    #mod <- polr(formula=model_formula,
    #       data = data_subset)
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
