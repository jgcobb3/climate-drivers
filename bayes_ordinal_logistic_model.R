############### Climate perception project ########## 
## SESYNC Research Support
##
##Data used in this project comes from three sources: (1) A survey of NRCS and FSA employees conducted by the USDA Climate Hubs in 2016/2017, (2) Crop idemnity payments made by FSA to farmers for weather-related 
##crop loss between 2013-2016, and (3) mean and standard deviations of drought data over 1, 3, 5, 10, and 15 year periods.
##
##
## DATE CREATED: 09/27/2018
## DATE MODIFIED: 8/8/2019
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
#http://mc-stan.org/rstanarm/reference/index.html

## Reference for model evaluation:
#https://arxiv.org/pdf/1507.04544.pdf 

###################################################
#

###### Library used
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
library("tidyr")
library("rstantools")
library("shinystan")
library("StanHeaders")

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
modeling_functions <- "bayes_logistic_model_functions_11272018.R"
#source(file.path(script_path,modeling_functions))

#Rachel setup local - Fed computer
script_path <- "C:/Users/rschattman/Documents/Research/climate-drivers-master/climate-drivers"
#modeling_functions <- "bayes_logistic_model_functions.R"
#source(file.path(script_path,modeling_functions))

#Rachel setup - home computer
#script_path <- "C:/Users/rache/Documents/GitHub/climate-drivers"
modeling_functions <- "bayes_logistic_model_functions.R"
source(file.path(script_path,modeling_functions))


#########cd ###################################################################
#####  Parameters and argument set up ########### 

#ARGS 1
#in_dir <- "/nfs/bparmentier-data/Data/projects/soilsesfeedback-data/data"
in_dir <- "C:/Users/rschattman/Documents/Research/climate-drivers-master/climate-drivers"
#in_dir <- "C:/Users/rache/Documents/GitHub/climate-drivers"

#ARGS 2
#out_dir <- "/nfs/bparmentier-data/Data/projects/soilsesfeedback-data/outputs"
out_dir <- "C:/Users/rschattman/Documents/Research/climate-drivers-master/climate-drivers/output"
#out_dir <- "C:/Users/rache/Documents/GitHub/climate-drivers/output"

#ARGS 3:
create_out_dir_param=TRUE #create a new ouput dir if TRUE

#ARGS 7
out_suffix <-"03022019" #output suffix for the files and ouptut folder

#ARGS 8
num_cores <- 2 # number of cores

Survey <- 
  in_filename <- "NRCS_FSAMergeDataset_w_PDSI2_7_28_18.csv"
in_filename2 <- "Deviation_from_norm_01312019.csv"
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

dataSurvey <- read.csv(file.path(in_dir,in_filename), 
                       header = TRUE)
dataDEV <- read.csv(file.path(in_dir, in_filename2),
                    header = TRUE)
dataDR <- merge(dataSurvey, dataDEV, 
                by = "STDIV")
head(dataDR)

dataDR$y_var <- dataDR[[y_var_name]]

dataDR$stdiv <- factor(dataDR$STDIV)
dataDR$Agency <- factor(dataDR$Agency)
dataDR$y_var <- factor(dataDR$y_var)

#### Setting up models var inputs
x_var_clean <- c("PercLossDrought", 
                 "STDIV", 
                 "Agency", 
                 "PDSI_MEAN_2016",
                 "PDSI_MEAN_2014",
                 "PDSI_MEAN_2012",
                 "PDSI_MEAN_2007",
                 "PDSI_MEAN_2002",
                 #"PDSI_STD_2016",
                 #"PDSI_STD_2014",
                 #"PDSI_STD_2012",
                 #"PDSI_STD_2007",
                 #"PDSI_STD_2002",
                 "PDSI_DEV_2016",
                 "PDSI_DEV_2014",
                 "PDSI_DEV_2012",
                 "PDSI_DEV_2007",
                 "PDSI_DEV_2002")

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
mod_DEV_2016b <- "y_var ~ PDSI_DEV_2016"
mod_DEV_2014b <- "y_var ~ PDSI_DEV_2014"
mod_DEV_2012b <- "y_var ~ PDSI_DEV_2012"
mod_DEV_2007b <- "y_var ~ PDSI_DEV_2007"
mod_DEV_2002b <- "y_var ~ PDSI_DEV_2002"

list_model_formulasb <- list(mod_mean2016b,
                             mod_mean2014b,
                             mod_mean2012b,
                             mod_mean2007b,
                             mod_mean2002b,
                             mod_DEV_2016b,
                             mod_DEV_2014b,
                             mod_DEV_2012b,
                             mod_DEV_2007b,
                             mod_DEV_2002b)

############ PART 2: Run model with option for bayesian ordinal logistic

# Resources on Priors 
# https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# https://rdrr.io/cran/rstanarm/man/priors.html 

# set priors
myprior <- R2(0.5, "mean") #assumes that mode, mean and median of the Beta distribution are equal. Indicated for polr

# set up list mods
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

list_mod <- lapply(list_model_formulasb[1:10],
                   FUN=run_model_ordinal_logistic,
                   data = data_subset,
                   prior = R2(0.5, "mean"),
                   prior_counts = dirichlet(1),
                   #shape = NULL,
                   chains = 4, 
                   num_cores = 4, 
                   seed_val = 1234, 
                   iter_val = 2000)

mod_outfilename <- paste0("list_mod_",out_suffix,".RData")
save(list_mod, file = file.path(out_dir,mod_outfilename))
list_mod[[10]]

# If you want to run them all seperatly, you could use the Mods below

Mod1 <- stan_polr(mod_mean2016b,
                  data = data_subset, 
                  prior = R2(0.5, "mean"), 
                  shape = NULL, 
                  algorithm = "sampling",
                  adapt_delta = NULL, 
                  do_residuals = TRUE,
                  prior_counts = dirichlet(1),
                  prior_PD = FALSE)

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

############# PART 3: MODEL ASSESSMENT ################
# Reference for posterior checks: 
# http://mc-stan.org/rstanarm/reference/pp_check.stanreg.html

prior_summary(list_mod[[10]])
pp_check(list_mod[[10]], plotfun = "bars", nreps = 500, prob = 0.5)
pp_check(list_mod[[10]]) #density overlay plot
pp_check(list_mod[[10]], "stat")
pp_check(list_mod[[10]], "stat_2d")
# Mod10: posterior draws accuratly refelect the actual distrubtion of the data

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
# Mod4: posterior draws accuratly refelect the actual distrubtion of the data

prior_summary(Mod5)
pp_check(Mod5, plotfun = "bars", nreps = 500, prob = 0.5)
pp_check(Mod5) #density overlay plot
pp_check(Mod5, "stat")
pp_check(Mod5, "stat_2d")
# Mod5: posterior draws accuratly refelect the actual distrubtion of the data

## Check for chain convergence
# good resource: http://mc-stan.org/bayesplot/articles/plotting-mcmc-draws.html 
# another: http://mc-stan.org/bayesplot/articles/visual-mcmc-diagnostics.html 

stan_trace(Mod1, inc_warmup=TRUE) 
rhats1 <- rhat(Mod1)
color_scheme_set("brightblue")
mcmc_rhat(rhats1) #rhat values close to 1, so we assume chains have converged.

stan_trace(Mod2, inc_warmup=TRUE) 
rhats2 <- rhat(Mod2)
color_scheme_set("brightblue")
mcmc_rhat(rhats2) #rhat values close to 1, so we assume chains have converged.

Mod6 <- lapply(mod_DEV_2016b,
               FUN = run_model_ordinal_logistic,
               data = data_subset, 
               prior = myprior,
               #shape = NULL, 
               #algorithm = "sampling",
               #adapt_delta = NULL, 
               #do_residuals = TRUE),
               prior_counts = dirichlet(1),
               chains = 4, 
               num_cores = 4, 
               seed_val = 1234, 
               iter_val = 200)

Mod7 <- lapply(mod_DEV_2014b,
               FUN = run_model_ordinal_logistic,
               data = data_subset, 
               prior = myprior, 
               #prior = R2(0.5, "mean"), #assumes that mode, mean and median of the Beta distribution are equal. Indicated for polr
               #shape = NULL, 
               #algorithm = "sampling",
               #adapt_delta = NULL, 
               #do_residuals = TRUE),
               prior_counts = dirichlet(1),
               chains = 4, 
               num_cores = 4, 
               seed_val = 1234, 
               iter_val = 200)

Mod8 <- lapply(mod_DEV_2012b,
               FUN = run_model_ordinal_logistic,
               data = data_subset, 
               prior = myprior,
               #shape = NULL, 
               #algorithm = "sampling",
               #adapt_delta = NULL, 
               #do_residuals = TRUE),
               prior_counts = dirichlet(1),
               chains = 4, 
               num_cores = 4, 
               seed_val = 1234, 
               iter_val = 200)

Mod9 <- lapply(mod_DEV_2007b,
               FUN = run_model_ordinal_logistic,
               data = data_subset, 
               prior = myprior,
               #shape = NULL, 
               #algorithm = "sampling",
               #adapt_delta = NULL, 
               #do_residuals = TRUE),
               prior_counts = dirichlet(1),
               chains = 4, 
               num_cores = 4, 
               seed_val = 1234, 
               iter_val = 200)

Mod10 <- lapply(mod_DEV_2002b,
                FUN = run_model_ordinal_logistic,
                data = data_subset, 
                prior = myprior,
                #prior = R2(0.5, "mean"), #assumes that mode, mean and median of the Beta distribution are equal. Indicated for polr
                #shape = NULL, 
                #algorithm = "sampling",
                #adapt_delta = NULL, 
                #do_residuals = TRUE),
                prior_counts = dirichlet(1),
                chains = 4, 
                num_cores = 4, 
                seed_val = 1234, 
                iter_val = 200)

# Save mods
mod_outfilename <- paste0("list_mod_",out_suffix,".RData")
save(list_mod, 
     file = file.path(out_dir,mod_outfilename))

mod_outfilename <- paste0("Mod10",out_suffix,".RData")
save(Mod10, 
     file = file.path(out_dir,mod_outfilename))

stan_trace(Mod3, inc_warmup=TRUE) 
rhats3 <- rhat(Mod3)
color_scheme_set("brightblue")
mcmc_rhat(rhats3) #rhat values close to 1, so we assume chains have converged.

stan_trace(Mod4, inc_warmup=TRUE) 
rhats4 <- rhat(Mod4)
color_scheme_set("brightblue")
mcmc_rhat(rhats4) #rhat values close to 1, so we assume chains have converged.

stan_trace(Mod5, inc_warmup=TRUE) 
rhats5 <- rhat(Mod5)
color_scheme_set("brightblue")
mcmc_rhat(rhats5) #rhat values close to 1, so we assume chains have converged.


########### Section 4: CREDIBILITY INTERVALS ######################
posterior_interval(Mod1, prob = 0.95)
posterior_interval(Mod2, prob = 0.95)
posterior_interval(Mod3, prob = 0.95)
posterior_interval(Mod4, prob = 0.95)
posterior_interval(Mod5, prob = 0.95)

######## Section 6: Shiny Stan Diagnostics #############################
# much of the code above could be accomplished just by running shiny stan
# but it was good practice to do it
launch_shinystan(Mod1)
launch_shinystan(Mod2)
launch_shinystan(Mod3)
launch_shinystan(Mod4)
launch_shinystan(Mod5)

######### Section 7: COEFFICIENTs #########################################
summary.stanreg(Mod1)
Mod1$coefficients
Mod2$coefficients
Mod3$coefficients
Mod4$coefficients
Mod5$coefficients


############# PART 3: Model assessment ################

loo_mod <- mclapply(list_mod,
                    FUN = run_model_assessment,
                    k_threshold = 0.7,
                    mc.preschedule = FALSE,
                    mc.cores = 1)


mod_outfilename <- paste0("loo_mod_",out_suffix,".RData")
save(loo_mod, 
     file = file.path(out_dir,mod_outfilename))


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

#loo_mod <- mclapply(list_mod,
#FUN=run_model_assessment,
#k_threshold=0.7,
#mc.preschedule = FALSE,
#mc.cores=1)

# check your priors
prior_summary(list_mod[[1]])


# check the structure of the mod 
#str(list_mod[[2]])  
#names(list_mod[[1]])
list_mod[[1]]$stanfit

# Report Condifence Intervales from Posterior draws. Default is 90% CIs.
Post_int <- lapply(list_mod[[1:10]], rstanarm::posterior_interval(list_mod[[1:10]]), prob = .95) #errot: recursive indexing failed at level 3

Post_int1 <- rstanarm::posterior_interval(list_mod[[1]], prob = .95)
Post_int2 <- rstanarm::posterior_interval(list_mod[[2]], prob = .95)
Post_int3 <- rstanarm::posterior_interval(list_mod[[3]], prob = .95)
Post_int4 <- rstanarm::posterior_interval(list_mod[[4]], prob = .95)
Post_int5 <- rstanarm::posterior_interval(list_mod[[5]], prob = .95)
Post_int6 <- rstanarm::posterior_interval(list_mod[[6]], prob = .95)
Post_int7 <- rstanarm::posterior_interval(list_mod[[7]], prob = .95)
Post_int8 <- rstanarm::posterior_interval(list_mod[[8]], prob = .95)
Post_int9 <- rstanarm::posterior_interval(list_mod[[9]], prob = .95)
Post_int10 <- rstanarm::posterior_interval(list_mod[[10]], prob = .95)

PP1<-rstanarm::posterior_predict(list_mod[[1]])
print(list_mod[[1]])
plot(list_mod[[1]])
list_mod[[10]]$stanfit
get_posterior_mean(list_mod[1])
####################################### Extracting info for paper ############################
########## Extract Model Parameters  #neff ration = Effective sample size
list_archive <- list_mod
extract_multinom_mod_information <- function(list_mod){
  
  if(class(list_mod)!="list"){
    loo_mod <- list(list_mod)
  }else{
    list_mod <- list_mod
    rm(list_mod)
  }
  
  names_mod <- paste("mod",1:length(list_mod),sep="")
  model_output_values <- unlist(lapply(list_mod,function(x){x$mean})) #error with X - is this because we only want to subset the last row?
  names(x[log-posterior]$mean) <- names_mod
  se_mean <- unlist(lapply(list_mod,function(x){x$se_mean}))
  names(se_mean_values) <- names_mod
  #adding extraction of odds ratio
  #list_odds <- lapply(list_mod,function(x){exp(coef(x))})
  #names(list_odds) <- names_mod
  #list_formulas <- lapply(list_mod,function(x){summary(x)$formula})
  #list_extract_coef_p_values <- lapply(list_mod,FUN=extract_coef_p_values)
  #names(list_extract_coef_p_values) <- names_mod
  multinom_extract_obj <- list(x[log-posterior]$mean, se_mean_values)               #list_coef,list_extract_coef_p_values,list_odds)
  names(multinom_extract_obj) <- c("Mean of Log Posterior", "SE Mean")
  #"list_coef","list_extract_coef_p_values","list_odds")
  return(multinom_extract_obj)
}

model_summary_table <- extract_multinom_mod_information(list_mod=list_mod)

########### Extract Loo info 
loo_archive <- loo_mod # make a copy that will not be modified


extract_multinom_mod_information <- function(loo_mod){
  
  if(class(loo_mod)!="list"){
    loo_mod <- list(loo_mod)
  }else{
    loo_mod <- loo_mod
    rm(loo_mod)
  }
  
  names_mod <- paste("mod",1:length(loo_mod),sep="")
  LOOIC_values <- unlist(lapply(loo_mod,function(x){x$looic}))
  names(LOOIC_values) <- names_mod
  list_elpd_loo <- unlist(lapply(loo_mod,function(x){x$elpd_loo}))
  names(list_elpd_loo) <- names_mod
  #adding extraction of odds ratio
  #list_odds <- lapply(list_mod,function(x){exp(coef(x))})
  #names(list_odds) <- names_mod
  #list_formulas <- lapply(list_mod,function(x){summary(x)$formula})
  #list_extract_coef_p_values <- lapply(list_mod,FUN=extract_coef_p_values)
  #names(list_extract_coef_p_values) <- names_mod
  multinom_extract_obj <- list(LOOIC_values, list_elpd_loo)               #list_coef,list_extract_coef_p_values,list_odds)
  names(multinom_extract_obj) <- c("LOOIC_values", "elpd_loo")
  #"list_coef","list_extract_coef_p_values","list_odds")
  return(multinom_extract_obj)
}

loo_list <- extract_multinom_mod_information(loo_mod=loo_mod)

#Transform list into dataframe and write into CSV

loo_table <- data.frame(mod = loo_list,
                        LOOIC = LOOIC_values
                        #ESS = list_elpd_loo
                        #loglikelihood=loglikelihood_values,
                        #LR = LR_Chi_values,
                        #LR_p=LR_Chi_p_values
                        #n=n_obs
)
loo_table$LOOIC
#df_val$mod <- rownames(df_val)
rownames(loo_table) <- NULL
loo_list[[i]] <- loo_table
{
  
  loo_all <-do.call(rbind,loo_list)
  out_filename <- file.path(out_dir,paste("loo_table_",out_suffix,".txt",sep=""))
  write.table(df_all,file=out_filename,sep=",",row.names = FALSE)
  
  return(loo_all) 
}


#https://github.com/bparment1/LUCC_yucatan/blob/master/analyses_fire_yucatan_functions.R
#start on line 446



#################################### For the paper #######################################
list_mod[[10]]$stanfit # gives n_eff, Rhat, mean, SD, and posterior 95% CI

loo_mod[[10]]$estimates
list_mod[[10]]$coefficients

############# PART 4: Create a Table ################


#table1 <- data.frame (list_mod = 1, 
#                   intercept = 1, 
#                   intercept.se = 1, 
#                   slope = 1, 
#                   slope.se = 1, 
#                   r.squared = 1, 
#                   p.value = 1)


##### This is where I stopped - Meet with BP?
rstan::extract(list_mod[[2]]$stanfit,pars="PercLossDrought")

### Still need to fix this part to extract the coef
list_mod[[1]]$coefficients
?coefficients
rstanarm::R2(list_mod[[1]])
COEF <- round(apply(rstan::extract(list_mod[[1]]$stanfit, pars = "Dry/Drought"), 2, median), digits = 3)

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



########### Section 9: PLOTS ######################
## Source: http://mc-stan.org/bayesplot/ 
## Step one: create new data frame using posterior draws
install.packages("bayesplot")
library("bayesplot")
install.packages("ggplot2")
library(ggplot2)
library(tidyselect)
library(rstan)
library(StanHeaders)

posterior_M1 <- as.matrix(Mod1)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
mcmc_areas(posterior,
           pars = c("1", "2", "3", "4"),
           prob = 0.8) 

#colnames(y_rep) <- c("1", "2", "3", "4") 
#plot_posterior <- gather(posterior_predict, key = "1", value = "not concerned") 
#head(plot_posterior) 

gplot1 <- ggerrorplot(posterior_M1, x = x_var_clean, 
                      y = c("Mean 1-year", "Mean 3-years", "Mean 5-years", "Mean 10-years", "Mean 15-years"),
                      combine = TRUE, merge = FALSE,
                      desc_stat = "mean_sd",  
                      color = "black",
                      palette = "npg",
                      title = "Level of concern and mean PDSI over 5-time scales",
                      add = "violin", add.params = list(color = "darkgray", fill="Concern_DryDrought"),
                      ylim = c(-6, 12),
                      common.legend = TRUE,
                      legend = "top", top = 12,
                      legend.title = "Level of Concern", 
                      xlab = "level of concern",
                      ylab = "PDSI",
                      orientation = "vertical")+
  #caption = "Level of concern about drought: Not concerned = 1, 
  #Slightly concerned = 2, Concerned = 3, Very concerned = 4")+
  #stat_compare_means(comparisons = my_comparisons) +
  #stat_compare_means(label.y = -5, label.x = 1.5) +
  geom_hline(yintercept= -0.5, linetype="dashed", color = "red", show.legend = TRUE, label_value(labels, multi_line = TRUE))

