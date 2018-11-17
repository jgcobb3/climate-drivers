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

create_dir_fun <- function(outDir,out_suffix=NULL){
  #if out_suffix is not null then append out_suffix string
  if(!is.null(out_suffix)){
    out_name <- paste("output_",out_suffix,sep="")
    outDir <- file.path(outDir,out_name)
  }
  #create if does not exists
  if(!file.exists(outDir)){
    dir.create(outDir)
  }
  return(outDir)
}

#Used to load RData object saved within the functions produced.
load_obj <- function(f){
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

################### Start script ###################

#Benoit setup
script_path <- "/nfs/bparmentier-data/Data/projects/soilsesfeedback-data/scripts"
#mosaicing_functions <- "weighted_mosaicing_functions_07252018.R"
#source(file.path(script_path,mosaicing_functions))

#########cd ###################################################################
#####  Parameters and argument set up ########### 

#ARGS 1
in_dir <- "/nfs/bparmentier-data/Data/projects/soilsesfeedback-data/data"
#ARGS 2
out_dir <- "/nfs/bparmentier-data/Data/projects/soilsesfeedback-data/outputs"
#ARGS 3:
create_out_dir_param=TRUE #create a new ouput dir if TRUE
#ARGS 7
out_suffix <-"_10232018" #output suffix for the files and ouptut folder
#ARGS 8
num_cores <- 2 # number of cores

in_filename <- "NRCS_FSAMergeDataset_w_PDSI2_7_28_18.csv"

################# START SCRIPT ###############################

######### PART 0: Set up the output dir ################

options(scipen=999)

#set up the working directory
#Create output directory

if(is.null(out_dir)){
  out_dir <- in_dir #output will be created in the input dir
  
}
#out_dir <- in_dir #output will be created in the input dir

out_suffix_s <- out_suffix #can modify name of output suffix
if(create_out_dir_param==TRUE){
  out_dir <- create_dir_fun(out_dir,out_suffix)
  setwd(out_dir)
}else{
  setwd(out_dir) #use previoulsy defined directory
}

#######################################
### PART 1: Read in DATA #######

dataDR <- read.csv(file.path(in_dir,in_filename), #"/nfs/bparmentier-data/Data/projects/soilsesfeedback-data/data/NRCS_FSAMergeDataset_w_PDSI2_7_28_18.csv", 
                   header = TRUE)

y_var_name <- "Concern_DryDrought"

dataDR$y_var <- dataDR[[y_var_name]]

dataDR$stdiv <- factor(dataDR$stdiv)
dataDR$Agency <- factor(dataDR$Agency)
dataDR$y_var <- factor(dataDR$y_var)


#### Setting up models var inputs
x_var_clean <- c("PercLossDrought", 
                 "stdiv", 
                 "Agency", 
                 "PDSI_MEAN_2016",
                 "PDSI_MEAN_2014",
                 "PDSI_MEAN_2012",
                 "PDSI_MEAN_2007",
                 "PDSI_MEAN_2002",
                 "PDSI_STD_2016",
                 "PDSI_STD_2014",
                 "PDSI_STD_2012",
                 "PDSI_STD_2007",
                 "PDSI_STD_2002")

y_var_clean <- y_var_name

variables_used <- c(y_var_clean, x_var_clean)

##subset dataset for variables

data_subset <- dataDR [,variables_used]
data_subset$y_var <- factor(data_subset[[y_var_name]])

data_subset <- na.omit(data_subset)

data_subset$y_var <- factor(data_subset[[y_var_name]])

## ------------------------------------------------------------------------

mod_noPDSI <- "y_var ~ PercLossDrought + stdiv"
mod_mean2016 <- "y_var ~ PercLossDrought + PDSI_MEAN_2016"
mod_mean2014 <- "y_var ~ PercLossDrought + PDSI_MEAN_2014"
mod_mean2012 <- "y_var ~ PercLossDrought + PDSI_MEAN_2012"
mod_mean2007 <- "y_var ~ PercLossDrought + PDSI_MEAN_2007"
mod_mean2002 <- "y_var ~ PercLossDrought + PDSI_MEAN_2002"
mod_STD2016 <- "y_var ~ PercLossDrought + PDSI_STD_2016"
mod_STD2014 <- "y_var ~ PercLossDrought + PDSI_STD_2014"
mod_STD2012 <- "y_var ~ PercLossDrought + PDSI_STD_2012"
mod_STD2007 <- "y_var ~ PercLossDrought + PDSI_STD_2007"
mod_STD2002 <-  "y_var ~ PercLossDrought + PDSI_STD_2002"
               
list_model_formulas <- list(mod_noPDSI,mod_mean2016,mod_mean2014,mod_mean2012,mod_mean2007,mod_mean2002,
                    mod_STD2016,mod_STD2014,mod_STD2012,mod_STD2007,mod_STD2002)
## ------------------------------------------------------------------------

### This should be a loop or a function:

model_type <- "bayes_stan"


mod2 <- run_model_ordinal_logistic(list_model_formulas[[2]],
                           data = data_subset, 
                           prior = NULL,
                           prior_counts = dirichlet(1),
                           shape = NULL,
                           chains = 4, 
                           num_cores = 4, 
                           seed_val = 1234, 
                           iter_val = 200)
## does not work
#list_mod <- mclapply(list_model_formulas,
#                     FUN=run_model_ordinal_logistic,
#                     data = data_subset, 
#                     prior = NULL,
#                     prior_counts = dirichlet(1),
#                     shape = NULL,
#                     chains = 4, 
#                     num_cores = 4, 
#                     seed_val = 1234, 
#                     iter_val = 200,
#                    mc.preschedule = F,
#                    mc.cores = 1)
         
list_mod <- lapply(list_model_formulas[1:3],
                     FUN=run_model_ordinal_logistic,
                     data = data_subset, 
                     prior = NULL,
                     prior_counts = dirichlet(1),
                     shape = NULL,
                     chains = 4, 
                     num_cores = 4, 
                     seed_val = 1234, 
                     iter_val = 200)

list_mod[[3]]
#save(mod,file= paste("C:\\Users\\rschattman\\Documents\\Research\\climate-drivers\\model",i,"output.rdata", sep ="")) # This save would be useful if you wanted to save each of the 11 models as their own file
  
mod_outfilename <- paste0("list_mod_",out_suffix,".RData")
save(list_mod, 
     file = mod_outfilename)


## ------------------------------------------------------------------------
options(mc.cores = 1)                      # loo default is 1 core
#plot(loo(mod2_mean2016, k_threshold = 0.7))
#plot(loo(mod3_mean2014, k_threshold = 0.7))
#plot(loo(mod4_mean2012, k_threshold = 0.7))
#plot(loo(mod5_mean2007, k_threshold = 0.7))
#plot(loo(mod6_mean2002, k_threshold = 0.7))
#plot(loo(mod7_STD2016, k_threshold = 0.7))
#plot(loo(mod8_STD2014))                    #no observations with pareto_k > 0.7
#plot(loo(mod9_STD2012))                    #no observations with pareto_k > 0.7
#plot(loo(mod10_STD2007, k_threshold = 0.7))
#plot(loo(mod11_STD2002, k_threshold = 0.7))

### store output of loo in object for further check and outputs
#loo_mod2 <- try(loo(mod2_mean2016, k_threshold = 0.7))
#loo_mod3 <- try(loo(mod3_mean2014,k_threshold = 0.7))
#loo_mod4 <- try(loo(mod4_mean2012, k_threshold = 0.7))
#loo_mod5 <- try(loo(mod5_mean2007, k_threshold = 0.7))
#loo_mod6 <- try(loo(mod6_mean2002, k_threshold = 0.7))
#loo_mod7 <- try(loo(mod7_STD2016, k_threshold = 0.7))
#loo_mod8 <- try(loo(mod8_STD2014))                    #no observations with pareto_k > 0.7
#loo_mod9 <- try(loo(mod9_STD2012))                    #no observations with pareto_k > 0.7
#loo_mod10 <- try(loo(mod10_STD2007, k_threshold = 0.7))
#loo_mod11 <- try(loo(mod11_STD2002, k_threshold = 0.7))



debug(run_model_assessment)

loo_mod2 <- run_model_assessment(mod2)
#> loo_mod <- run_model_assessment(mod2)
#1 problematic observation(s) found.
#Model will be refit 1 times.

#Fitting model 1 out of 1 (leaving out observation 1432)
#Error in stats::model.frame(formula = model_formula, data = structure(list( : 
#                                                                              object 'model_formula' not found
## Debugged and found that that the probl                                                                            
loo_mod <- mclapply(list_mod,
                    FUN=run_model_assessment,
                    k_threshold=0.7,
                    mc.preschedule = FALSE,
                    mc.cores=1)



###
debug(reloo)
reloo(mod2, loo_x, obs = bad_obs)

debug(loo)
mod$formula
k_threshold <- 0.7
loo_obj <- try(loo(mod2, k_threshold = k_threshold))
loo_obj <- try(loo(list_mod[[3]], k_threshold = k_threshold))


reloo(x, loo_x, obs = bad_obs)

log_lik.stanreg(fit_j, newdata = d[omitted, , drop = FALSE], offset = x$offset[omitted], newx = get_x(x)[omitted, , drop = FALSE], stanmat = as.character.stanreg(fit_j))

ll_args.stanreg(object, newdata = newdata, offset = offset, reloo_or_kfold = calling_fun %in% c("kfold", "reloo"), ...)


### check issue

#https://github.com/stan-dev/rstanarm/issues/135
## This suggests:
#Thanks Aki. I think I have a fix for this. The issue arises when computing
#log lik (internally inside the reloo function) when nrow(newdata) is 1 and
#any of the predictors are coded as factor variables in R (the Player
#variable in this case). R is trying to validate the levels of the factor so
#we just need to work around that. I'll merge the fix into the master branch
#tonight or tomorrow once I test it more.

## ------------------------------------------------------------------------
loo2 <- (loo(mod2_mean2016, 
             save_psis = TRUE))
print(loo2)

## ------------------------------------------------------------------------
compare_models(mod)

    
# ls(mod2_mean2016)
# 
# plot(loo(mod2_mean2016, k_threshold = 0.7))
# 
# loo(mod2_mean2016, k_threshold = 0.7)
# 
# loo.stanreg(mod2_mean2016, k_threshold = 0.7)
# 
# reloo(x, loo_x, obs = bad_obs)
# 
# log_lik.stanreg(fit_j, newdata = d[omitted, , drop = FALSE], offset = x$offset[omitted], newx = get_x(x)[omitted, , drop = FALSE], stanmat = as.character.stanreg(fit_j))
# 
# ll_args.stanreg(object, newdata = newdata, offset = offset, reloo_or_kfold = calling_fun %in% c("kfold", "reloo"), ...)
# 
# 
# ## ------------------------------------------------------------------------
# loo2 <- (loo(mod2_mean2016, 
#              save_psis = TRUE))
# print(loo2)
# 
# ## ------------------------------------------------------------------------
# compare_models(mod)

## ------------------------------------------------------------------------
shinystan::launch_shinystan(mod)

## ------------------------------------------------------------------------
#save(list_mod,file="C:\\Users\\rschattman\\Documents\\Research\\climate-drivers\\modeloutput.rdata")

## ------------------------------------------------------------------------
# for(i in 1:n_model){           #use this if you've saved multiple rdata files
# load(file= paste("C:\\Users\\rschattman\\Documents\\Research\\climate-drivers\\model",i,"output.rdata", sep =""))
# }

#use the code below if you've saved one rdata file with all the models in it

load(file=paste("C:\\Users\\rschattman\\Documents\\Research\\climate-drivers\\modeloutput.rdata", sep = ""))


## ------------------------------------------------------------------------
str(list_mod)
str(summary(list_mod))

## ------------------------------------------------------------------------

print(list_mod, digets = 3)
round(apply(rstan:: extract(list_mod$stanfit, pars = "drought") [[1]], 2, median), digits = 3)

loo(x, list_mod, cores = 1)
  #save_psis = FALSE, K_threshold = NULL)

table1 <- data.frame (list_mod = 1, 
                    intercept = 1, 
                    intercept.se = 1, 
                    slope = 1, 
                    slope.se = 1, 
                    r.squared = 1, 
                    p.value = 1)

## ------------------------------------------------------------------------
for(i in 1:11){
  x<-rnorm(11)
  y<-rnorm(11)
  list_mod <- list_mod
  summary(list_mod)
}

for (i in 1:11){
  print(i)
}

table1[i,] <- c(i,
                summary(list_mod)[['coefficients']]['(Intercept)','Estimate'],
                     summary(list_mod)[['coefficients']]['(Intercept)','Std. Error'],
                summary(list_mod)[['coefficients']]['x','Estimate'],
                     summary(list_mod)[['coefficients']]['x','Std. Error'],
                summary(list_mod)[['r.squared']],
                     summary(list_mod)[['coefficients']]['x','Pr(>|t|)'])

write.csv(table1, file = 'table1.csv')

#Fitting model 1 out of 1 (leaving out observation 1432)
#Error in stats::model.frame(formula = model_formula, data = list(Concern_DryDrought = c(4L,  : 
#                                                                                          object 'model_formula' not found
                                                                                        
################################# End of script ######################################
