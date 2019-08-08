############### Climate perception project ########## 
## SESYNC Research Support
##
##Data used in this project comes from three sources: (1) A survey of NRCS and FSA employees conducted by the USDA Climate Hubs in 2016/2017, (2) Crop idemnity payments made by FSA to farmers for weather-related 
##crop loss between 2013-2016, and (3) mean and standard deviations of drought data over 1, 3, 5, 10, and 15 year periods.
##
##
## DATE CREATED: 09/27/2018
## DATE MODIFIED: 2/22/2019
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
out_suffix <-"0302019" #output suffix for the files and ouptut folder

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



## Model formulas used in the 3rd analysis:


mod_mean2016c <- "y_var ~ PDSI_MEAN_2016 + PDSI_DEV_2016"
mod_mean2014c <- "y_var ~ PDSI_MEAN_2014 + PDSI_DEV_2014"
mod_mean2012c <- "y_var ~ PDSI_MEAN_2012 + PDSI_DEV_2012"
mod_mean2007c <- "y_var ~ PDSI_MEAN_2007 + PDSI_DEV_2007"
mod_mean2002c <- "y_var ~ PDSI_MEAN_2002 + PDSI_DEV_2002"

list_model_formulas_c <- list(mod_mean2016c,
                             mod_mean2014c,
                             mod_mean2012c,
                             mod_mean2007c,
                             mod_mean2002c)
                             
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

list_mod_combo <- lapply(list_model_formulas_c[1:5],
                   FUN=run_model_ordinal_logistic,
                   data = data_subset,
                   prior = R2(0.5, "mean"),
                   prior_counts = dirichlet(1),
                   #shape = NULL,
                   chains = 4, 
                   num_cores = 4, 
                   seed_val = 1234, 
                   iter_val = 2000)

mod_outfilename <- paste0("list_mod_c_",out_suffix,".RData")
save(list_mod, file = file.path(out_dir,mod_outfilename))
list_mod[[10]]
