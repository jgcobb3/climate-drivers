############### Climate perception project ########## 
## SESYNC Research Support
##
##Data used in this project comes from three sources: (1) A survey of NRCS and FSA employees conducted by the USDA Climate Hubs in 2016/2017, (2) Crop idemnity payments made by FSA to farmers for weather-related 
##crop loss between 2013-2016, and (3) mean and standard deviations of drought data over 1, 3, 5, 10, and 15 year periods.
##
##
## DATE CREATED: 09/27/2018
## DATE MODIFIED: 12/03/2018
## AUTHORS: Rachel Schattman, Benoit Parmentier  
## Version: 2
## PROJECT: Climate Percecption
## ISSUE: 
## TO DO:
##
## COMMIT: modifying code
##

## Very good reference:
#http://rpsychologist.com/r-guide-longitudinal-lme-lmer

## Reference for model evaluation:
#https://arxiv.org/pdf/1507.04544.pdf 

###################################################
#

###### Library used

## ------------------------------------------------------------------------
library("MASS")
library("lme4")
library("rstanarm")
library("bayesplot")
library("ggplot2")
library("loo")
library("parallel")
library("coda")
library("rstan")  
library("dplyr")

# citation("rstanarm")
#update.packages(ask = FALSE, checkBuilt = TRUE)

####### Functions used in this script and sourced from other files

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
#script_path <- "/nfs/bparmentier-data/Data/projects/soilsesfeedback-data/scripts"
#modeling_functions <- "bayes_logistic_model_functions_11272018.R"
#source(file.path(script_path,modeling_functions))

#Rachel setup local - Fed computer
#script_path <- "C:/Users/rschattman/Documents/Research/climate-drivers-master/climate-drivers"
#modeling_functions <- "bayes_logistic_model_functions.R"
#source(file.path(script_path,modeling_functions))

#Rachel setup - home computer
script_path <- "C:/Users/rache/Documents/GitHub/climate-drivers"
modeling_functions <- "bayes_logistic_model_functions.R"
source(file.path(script_path,modeling_functions))


#########cd ###################################################################
#####  Parameters and argument set up ########### 

#ARGS 1
#in_dir <- "/nfs/bparmentier-data/Data/projects/soilsesfeedback-data/data"
#in_dir <- "C:/Users/rschattman/Documents/Research/climate-drivers-master/climate-drivers"
in_dir <- "C:/Users/rache/Documents/GitHub/climate-drivers"

#ARGS 2
#out_dir <- "/nfs/bparmentier-data/Data/projects/soilsesfeedback-data/outputs"
#out_dir <- "C:/Users/rschattman/Documents/Research/climate-drivers-master/climate-drivers/output"
#in_dir <- "/nfs/bparmentier-data/Data/projects/soilsesfeedback-data/data"
#in_dir <- "C:/Users/rschattman/Documents/Research/climate-drivers-master/climate-drivers"
in_dir <- "C:/Users/rache/Documents/GitHub/climate-drivers"

#ARGS 2
#out_dir <- "/nfs/bparmentier-data/Data/projects/soilsesfeedback-data/outputs"
#out_dir <- "C:/Users/rschattman/Documents/Research/climate-drivers-master/climate-drivers/output"
out_dir <- "C:/Users/rache/Documents/GitHub/climate-drivers/output"

#ARGS 3:
create_out_dir_param=TRUE #create a new ouput dir if TRUE

#ARGS 7
out_suffix <-"12272018" #output suffix for the files and ouptut folder

#ARGS 8
num_cores <- 2 # number of cores


in_filename <- "NRCS_FSAMergeDataset_w_PDSI2_7_28_18.csv"
model_type <- "bayes_stan"
y_var_name <- "Concern_DryDrought"

################# START SCRIPT ######################################


########################################################

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

dataDR <- read.csv(file.path(in_dir,in_filename), 
                   header = TRUE)

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

## Model formulas used in the 1st analysis:

#mod_noPDSI <- "y_var ~ PercLossDrought + stdiv"
#mod_mean2016 <- "y_var ~ PercLossDrought + PDSI_MEAN_2016"
#mod_mean2014 <- "y_var ~ PercLossDrought + PDSI_MEAN_2014"
#mod_mean2012 <- "y_var ~ PercLossDrought + PDSI_MEAN_2012"
#mod_mean2007 <- "y_var ~ PercLossDrought + PDSI_MEAN_2007"
#mod_mean2002 <- "y_var ~ PercLossDrought + PDSI_MEAN_2002"
#mod_STD2016 <- "y_var ~ PercLossDrought + PDSI_STD_2016"
#mod_STD2014 <- "y_var ~ PercLossDrought + PDSI_STD_2014"
#mod_STD2012 <- "y_var ~ PercLossDrought + PDSI_STD_2012"
#mod_STD2007 <- "y_var ~ PercLossDrought + PDSI_STD_2007"
#mod_STD2002 <-  "y_var ~ PercLossDrought + PDSI_STD_2002"
               
#list_model_formulas <- list(mod_noPDSI,mod_mean2016,mod_mean2014,mod_mean2012,mod_mean2007,mod_mean2002,
#                   mod_STD2016,mod_STD2014,mod_STD2012,mod_STD2007,mod_STD2002)

## Model formulas used in the 2nd analysis:


mod_mean2016b <- "y_var ~ PDSI_MEAN_2016"
mod_mean2014b <- "y_var ~ PDSI_MEAN_2014"
mod_mean2012b <- "y_var ~ PDSI_MEAN_2012"
mod_mean2007b <- "y_var ~ PDSI_MEAN_2007"
mod_mean2002b <- "y_var ~ PDSI_MEAN_2002"
mod_STD2016b <- "y_var ~ PDSI_STD_2016"
mod_STD2014b <- "y_var ~ PDSI_STD_2014"
mod_STD2012b <- "y_var ~ PDSI_STD_2012"
mod_STD2007b <- "y_var ~ PDSI_STD_2007"
mod_STD2002b <-  "y_var ~ PDSI_STD_2002"

list_model_formulasb <- list(mod_mean2016b,mod_mean2014b,mod_mean2012b,mod_mean2007b,mod_mean2002b,
                            mod_STD2016b,mod_STD2014b,mod_STD2012b,mod_STD2007b,mod_STD2002b)

############ PART 2: Run model with option for bayesian ordinal logistic

#mod2 <- run_model_ordinal_logistic(list_model_formulas[[2]],
#                           data = data_subset, 
#                           prior = NULL,
#                           prior_counts = dirichlet(1),
#                           shape = NULL,
#                           chains = 4, 
#                           num_cores = 4, 
#                           seed_val = 1234, 
#                           iter_val = 200)

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

#First set of models         
#list_mod <- lapply(list_model_formulas[1:11],
#                     FUN=run_model_ordinal_logistic,
#                     data = data_subset,
#                     prior = normal(location = 0, scale = NULL, autoscale = TRUE),
#                     prior_counts = dirichlet(1),
#                     shape = NULL,
#                    chains = 4, 
#                     num_cores = 4, 
#                     seed_val = 1234, 
#                     iter_val = 200)

#list_mod[[11]]

#Second set of models
#suggestions for setting priors: https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# assign prior
Norm_prior <- normal(location = .5, scale = c(1,4), autoscale = TRUE) 
#another helpful source: https://rdrr.io/cran/rstanarm/man/priors.html 

#assign prior

### Benoit, there seems to be a problem with the model compiling as a list. I know it is clunky, but I am going to run them all
# seperatly. Yuck.

myprior <- R2(0.5, "mean") #assumes that mode, mean and median of the Beta distribution are equal. Indicated for polr


#list_mod <- lapply(list_model_formulasb[[1:10]],
#              FUN = run_model_ordinal_logistic,
#              data = data_subset, 
#              prior = myprior,
               #shape = NULL, 
               #algorithm = "sampling",
               #adapt_delta = NULL, 
               #do_residuals = TRUE),
#              prior_counts = dirichlet(1),
#              chains = 4, 
#              num_cores = 4, 
#              seed_val = 1234, 
#              iter_val = 200)

##### I couldn't get this mod to recognize anything but NULL priors.
#Mod1 <- lapply(mod_mean2016b,
#                   FUN = run_model_ordinal_logistic,
#                   data = data_subset, 
#                   prior = R2(0.5, "mean"), 
#                   #shape = NULL, 
#                   #algorithm = "sampling",
#                   #adapt_delta = NULL, 
#                   #do_residuals = TRUE),
#                   prior_counts = dirichlet(1),
#                   #prior_PD = FALSE,
#                   chains = 4, 
#                   num_cores = 4, 
#                   seed_val = 1234, 
#                   iter_val = 200)

Mod1 <- stan_polr(mod_mean2016b,
               data = data_subset, 
               prior = R2(0.5, "mean"), 
               shape = NULL, 
               algorithm = "sampling",
               adapt_delta = NULL, 
               do_residuals = TRUE,
               prior_counts = dirichlet(1),
               prior_PD = FALSE)
               #chains = 4, 
               #num_cores = 2, 
               #seed_val = 1234) 
               #iter_val = 200)

Mod2 <- stan_polr(mod_mean2014b,
                  data = data_subset, 
                  prior = R2(0.5, "mean"), 
                  shape = NULL, 
                  algorithm = "sampling",
                  adapt_delta = NULL, 
                  do_residuals = TRUE,
                  prior_counts = dirichlet(1),
                  prior_PD = FALSE)

Mod3 <- stan_polr(mod_mean2012b,
                  data = data_subset, 
                  prior = R2(0.5, "mean"), 
                  shape = NULL, 
                  algorithm = "sampling",
                  adapt_delta = NULL, 
                  do_residuals = TRUE,
                  prior_counts = dirichlet(1),
                  prior_PD = FALSE)

Mod4 <- stan_polr(mod_mean2007b,
                  data = data_subset, 
                  prior = R2(0.5, "mean"), 
                  shape = NULL, 
                  algorithm = "sampling",
                  adapt_delta = NULL, 
                  do_residuals = TRUE,
                  prior_counts = dirichlet(1),
                  prior_PD = FALSE)

Mod5 <- stan_polr(mod_mean2002b,
                  data = data_subset, 
                  prior = R2(0.5, "mean"), 
                  shape = NULL, 
                  algorithm = "sampling",
                  adapt_delta = NULL, 
                  do_residuals = TRUE,
                  prior_counts = dirichlet(1),
                  prior_PD = FALSE)
#chains = 4, 
#num_cores = 2, 
#seed_val = 1234) 
#iter_val = 200)

#save(mod,file= paste("C:\\Users\\rschattman\\Documents\\Research\\climate-drivers\\model",i,"output.rdata", sep ="")) # This save would be useful if you wanted to save each of the 11 models as their own file

mod_outfilename <- paste0("Mod1_",out_suffix,".RData")
save(Mod1, file = file.path(out_dir,mod_outfilename))

mod_outfilename <- paste0("Mod2_",out_suffix,".RData")
save(Mod2, file = file.path(out_dir,mod_outfilename))

mod_outfilename <- paste0("Mod3_",out_suffix,".RData")
save(Mod3, file = file.path(out_dir,mod_outfilename))

mod_outfilename <- paste0("Mod4_",out_suffix,".RData")
save(Mod4, file = file.path(out_dir,mod_outfilename))

mod_outfilename <- paste0("Mod5_",out_suffix,".RData")
save(Mod5, file = file.path(out_dir,mod_outfilename))

############# PART 3: Model assessment ################

# Reference for posterior checks: 
# http://mc-stan.org/rstanarm/reference/pp_check.stanreg.html

prior_summary(Mod1)
pp_check(Mod1, plotfun = "bars", nreps = 500, prob = 0.5)
pp_check(Mod1) #density overlay plot
pp_check(Mod1, "stat")
pp_check(Mod1, "stat_2d")
# Mod1: posterior draws accuratly refelect the actual distrubtion of the data

prior_summary(Mod2)
pp_check(Mod2, plotfun = "bars", nreps = 500, prob = 0.5)
pp_check(Mod2) #density overlay plot
pp_check(Mod2, "stat")
pp_check(Mod2, "stat_2d")
# Mod2: posterior draws accuratly refelect the actual distrubtion of the data

prior_summary(Mod3)
pp_check(Mod3, plotfun = "bars", nreps = 500, prob = 0.5)
pp_check(Mod3) #density overlay plot
pp_check(Mod3, "stat")
pp_check(Mod3, "stat_2d")
# Mod3: posterior draws accuratly refelect the actual distrubtion of the data

prior_summary(Mod4)
pp_check(Mod4, plotfun = "bars", nreps = 500, prob = 0.5)
pp_check(Mod4) #density overlay plot
pp_check(Mod4, "stat")
pp_check(Mod4, "stat_2d")
# Mod3: posterior draws accuratly refelect the actual distrubtion of the data

prior_summary(Mod5)
pp_check(Mod5, plotfun = "bars", nreps = 500, prob = 0.5)
pp_check(Mod5) #density overlay plot
pp_check(Mod5, "stat")
pp_check(Mod5, "stat_2d")
# Mod3: posterior draws accuratly refelect the actual distrubtion of the data

## Check for chain convergence
# good resource: http://mc-stan.org/bayesplot/articles/plotting-mcmc-draws.html 
# another: http://mc-stan.org/bayesplot/articles/visual-mcmc-diagnostics.html 
posterior_Mod1 <- as.array(Mod1)
dim(posterior_Mod1)
dimnames(posterior_Mod1)
log_posterior(Mod1)


rhats <- rhat(list_modb[2])
color_scheme_set("brightblue")
mcmc_rhat(rhats) #rhat values close to 1, so we assume chains have converged.

#visualizing chains
color_scheme_set("red")
mcmc_intervals(posterior, pars = c("PDSI_MEAN_2016", "1|2", "2|3", "3|4"))
mcmc_areas(
  posterior,
  pars = c("PDSI_MEAN_2016", "1|2", "2|3", "3|4"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, #99%
  point_est = "mean")
mcmc_dens_overlay(posterior, pars = c("PDSI_MEAN_2016", "1|2", "2|3", "3|4"))



#list_modb <- lapply(list_model_formulasb,
#                    FUN = run_model_ordinal_logistic,
#                    data = data_subset, 
#                    #prior = Norm_prior,
#                    prior = R2(0.2, "mean"),
#                    #shape = NULL, 
#                    #algorithm = "sampling",
#                    #adapt_delta = NULL, 
#                    #do_residuals = TRUE),
#                    prior_counts = dirichlet(1),
#                    chains = 4, 
#                    num_cores = 4, 
#                    seed_val = 1234, 
#                    iter_val = 200)






############# PART 4: Compare Models ################


#loo_mod2 <- run_model_assessment(mod2)


##Benoit's elegant code

loo_mod <- mclapply(list_modb,
                    FUN = run_model_assessment,
                    k_threshold = 0.7,
                    mc.preschedule = FALSE,
                    mc.cores = 1)

#loo_mod <- lapply(list_mod,
#                  FUN=run_model_assessment,
#                  k_threshold=0.7)

#compare_models(loo_mod[[2]],loo_mod[[3]])

#loo_mod <- mclapply(list_mod,
                  #FUN=run_model_assessment,
                  #k_threshold=0.7,
                  #mc.preschedule = FALSE,
                  #mc.cores=1)


#loo_mod <- lapply(list_mod,
                    #FUN=run_model_assessment,
                    #k_threshold=0.7)

#compare_models(loo_mod[[2]],loo_mod[[3]])

loo1 <- loo(list_modb[[1]]) #data structure doesn't work?
loo2 <- loo(list_modb[[2]])
loo3 <- loo(list_modb[[3]])
loo4 <- loo(list_modb[[4]])
loo5 <- loo(list_modb[[5]])
loo6 <- loo(list_modb[[6]])
loo7 <- loo(list_modb[[7]])
loo8 <- loo(list_modb[[8]])
loo9 <- loo(list_modb[[9]])
loo10 <- loo(list_modb[[10]])

loomod_compare <- compare_models(loo_mod[1:10])

loomod_compare <- compare_models(loos = c(loo_mod[1],
                                 loo_mod[2],
                                 loo_mod[3],
                                 loo_mod[4],
                                 loo_mod[5],
                                 loo_mod[6],
                                 loo_mod[7],
                                 loo_mod[8],
                                 loo_mod[9],
                                 loo_mod[10]))



print(loomod_compare) # this is great, except all rows are labled "mod" and you can't tell which is which!

############# PART 3: Model assessment (Model B) ################
#loo1b <- loo(list_modb[[1]])
#loo2b <- loo(list_modb[[2]])
#loo3b <- loo(list_modb[[3]])
#loo4b <- loo(list_modb[[4]])
#loo5b <- loo(list_modb[[5]])
#loo6b <- loo(list_modb[[6]])
#loo7b <- loo(list_modb[[7]])
#loo8b <- loo(list_modb[[8]])
#loo9b <- loo(list_modb[[9]])
#loo10b <- loo(list_modb[[10]])

#loomodb_compare <- compare_models(loo1b,
#                                 loo2b,
#                                 loo3b,
#                                 loo4b,
#                                 loo5b,
#                                 loo6b,
#                                 loo7b,
#                                 loo8b,
#                                 loo9b,
#                                 loo10b)

#print(loomodb_compare)


# str(list_modb[[2]])  ### prints out a list
names(list_modb[[1]])
list_modb[[1]]$stanfit




############################### Median Absolute Deviation = (MAD)################################ 
# “Bayesian point estimates” — the posterior medians — are similar to 
# maximum likelihood estimates

# diagnose posteriors - Bayesian uncertainty intervals
PI1 <- posterior_interval(list_modb[[1]], prob = 0.95)
summary(residuals(list_modb[[1]])) # not deviance residuals

#check for covariances
cov2cor(vcov(list_modb[[1]])) # covariance of chains... maybe not helful



# Visually check for convergence of MCMC chains
# requires coda package
# x must be an mcmc list
gelman.diag(x, confidence = 0.95, transform=FALSE, autoburnin=TRUE,
            multivariate=TRUE)

str(summary(list_modb))

#Rachel's not elegant (but functional) code
loo_mod2 <- loo(list_mod[[2]],
                cores = 2)
loo_mod3 <- loo(list_mod[[3]],
                cores = 2)
loo_mod4 <- loo(list_mod[[4]],
                cores = 2)
loo_mod5 <- loo(list_mod[[5]],
                cores = 2)
loo_mod6 <- loo(list_mod[[6]],
                cores = 2)
loo_mod7 <- loo(list_mod[[7]],
                cores = 2)
loo_mod8 <- loo(list_mod[[8]],
                cores = 2)
loo_mod9 <- loo(list_mod[[9]],
                cores = 2)
loo_mod10 <- loo(list_mod[[10]],
                 cores = 2)
loo_mod11 <- loo(list_mod[[11]],
                 cores = 2)

print(loo_mod2)
print(loo_mod3)
print(loo_mod4)
print(loo_mod5)
print(loo_mod6)
print(loo_mod7)
print(loo_mod8)
print(loo_mod9)
print(loo_mod10)
print(loo_mod11)

#compare models
compareloo <- compare(x=list(loo_mod2,
              loo_mod3,
              loo_mod4,
              loo_mod5,
              loo_mod6,
              loo_mod7,
              loo_mod8,
              loo_mod9,
              loo_mod10,
              loo_mod11))


############# PART 4: Create a Table ################


#table1 <- data.frame (list_mod = 1, 
 #                   intercept = 1, 
 #                   intercept.se = 1, 
 #                   slope = 1, 
 #                   slope.se = 1, 
 #                   r.squared = 1, 
 #                   p.value = 1)
 

## ------------------------------------------------------------------------
str(list_mod)
str(summary(list_mod))

## ------------------------------------------------------------------------

print(list_mod, digets = 3)

rstan::extract(list_mod[[2]]$stanfit,pars="PercLossDrought")
list_mod[[2]]$stanfit

### Still need to fix this part to extract the coef
#round(apply(rstan:: extract(list_mod$stanfit, pars = "drought") [[1]], 2, median), digits = 3)

#loo(x, list_mod, cores = 1)
  #save_psis = FALSE, K_threshold = NULL)

#table1 <- data.frame (list_mod = 1, 
#                    intercept = 1, 
#                    intercept.se = 1, 
#                    slope = 1, 
#                    slope.se = 1, 
#                    r.squared = 1, 
#                    p.value = 1)

#for(i in 1:11){
#  x<-rnorm(11)
#  y<-rnorm(11)
#  list_mod <- list_mod
#  summary(list_mod)
#}

#for (i in 1:11){
#  print(i)
#}

#table1[i,] <- c(i,
#                summary(list_mod)[['coefficients']]['(Intercept)','Estimate'],
#                     summary(list_mod)[['coefficients']]['(Intercept)','Std. Error'],
#                summary(list_mod)[['coefficients']]['x','Estimate'],
#                     summary(list_mod)[['coefficients']]['x','Std. Error'],
#                summary(list_mod)[['r.squared']],
#                     summary(list_mod)[['coefficients']]['x','Pr(>|t|)'])

#write.csv(table1, file = 'table1.csv')

## Plotting:
##https://mjskay.github.io/tidybayes/articles/tidy-rstanarm.html 

##### Testing the Parellel Regression Assumption of POLR ############
# https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/

sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)),
    'Y>=4' = qlogis(mean(y >= 4)))
}

(s_mean2016 <- with(data_subset, summary(as.numeric(Concern_DryDrought) ~ PDSI_MEAN_2016, fun=sf)))
(s_mean2014 <- with(data_subset, summary(as.numeric(Concern_DryDrought) ~ PDSI_MEAN_2014, fun=sf)))
(s_mean2012 <- with(data_subset, summary(as.numeric(Concern_DryDrought) ~ PDSI_MEAN_2012, fun=sf)))
(s_mean2007 <- with(data_subset, summary(as.numeric(Concern_DryDrought) ~ PDSI_MEAN_2007, fun=sf)))
(s_mean2002 <- with(data_subset, summary(as.numeric(Concern_DryDrought) ~ PDSI_MEAN_2002, fun=sf)))
(s_std2016 <- with(data_subset, summary(as.numeric(Concern_DryDrought) ~ PDSI_STD_2016, fun=sf)))
(s_std2014 <- with(data_subset, summary(as.numeric(Concern_DryDrought) ~ PDSI_STD_2014, fun=sf)))
(s_std2012 <- with(data_subset, summary(as.numeric(Concern_DryDrought) ~ PDSI_STD_2012, fun=sf)))
(s_std2007 <- with(data_subset, summary(as.numeric(Concern_DryDrought) ~ PDSI_STD_2007, fun=sf)))
(s_std2002 <- with(data_subset, summary(as.numeric(Concern_DryDrought) ~ PDSI_STD_2002, fun=sf)))

# The above functions display the linear predicted variables we would get if we regressed Concern (the predicted variable)
# on our predictor variables without the parallel slopes assumption. To evaluate the parallel slops assumption, we now will
# run a series of binary logistic regressesions with varying cutpoints on the dependent variable and checking the equality
# of coefficients across cutpoints.

#PDSI_MEAN_2016 - parallel slopes assumption holds.
glm(I(as.numeric(Concern_DryDrought) >= 2) ~ PDSI_MEAN_2016, family="binomial", data = data_subset)
glm(I(as.numeric(Concern_DryDrought) >= 3) ~ PDSI_MEAN_2016, family="binomial", data = data_subset) # dif between intercept 2/3 = 1.648
glm(I(as.numeric(Concern_DryDrought) >= 4) ~ PDSI_MEAN_2016, family="binomial", data = data_subset)# dif between intercept 3/4 = 1.648

plot(s_mean2016, which=1:4, pch=1:3, xlab='logit', main=' ', xlim=range(s_mean2016[,3:4]))

#PDSI_MEAN_2014 - parallel slopes assumption holds.
glm(I(as.numeric(Concern_DryDrought) >= 2) ~ PDSI_MEAN_2014, family="binomial", data = data_subset)
glm(I(as.numeric(Concern_DryDrought) >= 3) ~ PDSI_MEAN_2014, family="binomial", data = data_subset) # dif between intercept 2/3 = 1.658
glm(I(as.numeric(Concern_DryDrought) >= 4) ~ PDSI_MEAN_2014, family="binomial", data = data_subset)# dif between intercept 3/4 = 1.655

plot(s_mean2014, which=1:4, pch=1:3, xlab='logit', main=' ', xlim=range(s_mean2014[,3:4]))

#PDSI_MEAN_2012 - does the parallel slopes assumption hold? The plot looks similar to the 2014 and 2016 plots, so I would say yes...ish?
glm(I(as.numeric(Concern_DryDrought) >= 2) ~ PDSI_MEAN_2012, family="binomial", data = data_subset)
glm(I(as.numeric(Concern_DryDrought) >= 3) ~ PDSI_MEAN_2012, family="binomial", data = data_subset) # dif between intercept 2/3 = 1.623
glm(I(as.numeric(Concern_DryDrought) >= 4) ~ PDSI_MEAN_2012, family="binomial", data = data_subset)# dif between intercept 3/4 = 1.700

plot(s_mean2012, which=1:4, pch=1:3, xlab='logit', main=' ', xlim=range(s_mean2012[,3:4]))

#PDSI_MEAN_2007 - does the parallel slopes assumption hold? Same as above
glm(I(as.numeric(Concern_DryDrought) >= 2) ~ PDSI_MEAN_2007, family="binomial", data = data_subset)
glm(I(as.numeric(Concern_DryDrought) >= 3) ~ PDSI_MEAN_2007, family="binomial", data = data_subset) # dif between intercept 2/3 = 1.618
glm(I(as.numeric(Concern_DryDrought) >= 4) ~ PDSI_MEAN_2007, family="binomial", data = data_subset)# dif between intercept 3/4 = 1.693

plot(s_mean2007, which=1:4, pch=1:3, xlab='logit', main=' ', xlim=range(s_mean2007[,3:4]))

#PDSI_MEAN_2002 - does the parallel slopes assumption hold? Same as above
glm(I(as.numeric(Concern_DryDrought) >= 2) ~ PDSI_MEAN_2002, family="binomial", data = data_subset)
glm(I(as.numeric(Concern_DryDrought) >= 3) ~ PDSI_MEAN_2002, family="binomial", data = data_subset) # dif between intercept 2/3 = 1.618
glm(I(as.numeric(Concern_DryDrought) >= 4) ~ PDSI_MEAN_2002, family="binomial", data = data_subset)# dif between intercept 3/4 = 1.70

plot(s_mean2002, which=1:4, pch=1:3, xlab='logit', main=' ', xlim=range(s_mean2002[,3:4]))

# All STD model plots are reasonable - I think the parrallet slopes assumptions holds on all of our models.
plot(s_std2016, which=1:4, pch=1:3, xlab='logit', main=' ', xlim=range(s_std2016[,3:4]))
plot(s_std2014, which=1:4, pch=1:3, xlab='logit', main=' ', xlim=range(s_std2014[,3:4]))
plot(s_std2012, which=1:4, pch=1:3, xlab='logit', main=' ', xlim=range(s_std2012[,3:4]))
plot(s_std2007, which=1:4, pch=1:3, xlab='logit', main=' ', xlim=range(s_std2007[,3:4]))
plot(s_std2002, which=1:4, pch=1:3, xlab='logit', main=' ', xlim=range(s_std2002[,3:4]))



################################# End of script ######################################
