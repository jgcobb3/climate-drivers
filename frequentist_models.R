############### Climate perception project ########## 
## SESYNC Research Support
##
##Data used in this project comes from three sources: (1) A survey of NRCS and FSA employees conducted by the USDA Climate Hubs in 2016/2017, (2) Crop idemnity payments made by FSA to farmers for weather-related 
##crop loss between 2013-2016, and (3) mean and standard deviations of drought data over 1, 3, 5, 10, and 15 year periods.
##
##
## DATE CREATED: 12/1/2018
## DATE MODIFIED: 
## AUTHORS: Rachel Schattman
## Version: 1
## PROJECT: Climate Percecption
## ISSUE: Run models as frequentist regressions for AGU poster
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

###### Libraries used
library(dplyr)
library(ggpubr)
library(ggplot2)
library(car)
library(gvlma)
require(foreign)
require(MASS)
require(Hmisc)
require(reshape2)

##################


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


#Rachel setup local - Fed computer
script_path <- "C:/Users/rschattman/Documents/Research/climate-drivers-master/climate-drivers"
modeling_functions <- "bayes_logistic_model_functions.R"
source(file.path(script_path,modeling_functions))

#Rachel setup - home computer
script_path <- "C:/Users/rache/Documents/GitHub/climate-drivers"
modeling_functions <- "bayes_logistic_model_functions.R"
source(file.path(script_path,modeling_functions))

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
out_suffix <-"_12012018" #output suffix for the files and ouptut folder

#ARGS 8
num_cores <- 2 # number of cores


in_filename <- "NRCS_FSAMergeDataset_w_PDSI2_7_28_18.csv"
y_var_name <- "Concern_DryDrought"



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


#Model formulas used in the Frequentist Analysis:


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

#################### run polr ##########################################
M1b <- polr(y_var ~ PDSI_MEAN_2016, data = data_subset, Hess = TRUE)
summary(M1b)
(ctable <- coef(summary(M1b))) #store table
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 ## calculate and store p values
(ctable <- cbind(ctable, "p value" = p)) ## combined table
(ci <- confint(M1b)) # default method gives profiled CIs. If the 95% CI does not cross 0, the parameter estimate is statistically significant. 

M2b <- polr(y_var ~ PDSI_MEAN_2014, data = data_subset, Hess = TRUE)
summary(M2b)
(ctable <- coef(summary(M2b))) #store table
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 ## calculate and store p values
(ctable <- cbind(ctable, "p value" = p)) ## combined table
(ci <- confint(M2b)) # default method gives profiled CIs. If the 95% CI does not cross 0, the parameter estimate is statistically significant. 

M3b <- polr(y_var ~ PDSI_MEAN_2012, data = data_subset, Hess = TRUE)
summary(M3b)
(ctable <- coef(summary(M3b))) #store table
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 ## calculate and store p values
(ctable <- cbind(ctable, "p value" = p)) ## combined table
(ci <- confint(M3b)) # default method gives profiled CIs. If the 95% CI does not cross 0, the parameter estimate is statistically significant. 

M4b <- polr(y_var ~ PDSI_MEAN_2007, data = data_subset, Hess = TRUE)
summary(M4b)
(ctable <- coef(summary(M4b))) #store table
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 ## calculate and store p values
(ctable <- cbind(ctable, "p value" = p)) ## combined table
(ci <- confint(M4b)) # default method gives profiled CIs. If the 95% CI does not cross 0, the parameter estimate is statistically significant. 

M5b <- polr(y_var ~ PDSI_MEAN_2002, data = data_subset, Hess = TRUE)
summary(M5b)
(ctable <- coef(summary(M5b))) #store table
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 ## calculate and store p values
(ctable <- cbind(ctable, "p value" = p)) ## combined table
(ci <- confint(M5b)) # default method gives profiled CIs. If the 95% CI does not cross 0, the parameter estimate is statistically significant. 

M6b <- polr(y_var ~ PDSI_STD_2016, data = data_subset, Hess = TRUE)
summary(M6b)
(ctable <- coef(summary(M6b))) #store table
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 ## calculate and store p values
(ctable <- cbind(ctable, "p value" = p)) ## combined table
(ci <- confint(M6b)) # default method gives profiled CIs. If the 95% CI does not cross 0, the parameter estimate is statistically significant. 

M7b <- polr(y_var ~ PDSI_STD_2014, data = data_subset, Hess = TRUE)
summary(M7b)
(ctable <- coef(summary(M7b))) #store table
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 ## calculate and store p values
(ctable <- cbind(ctable, "p value" = p)) ## combined table
(ci <- confint(M7b)) # default method gives profiled CIs. If the 95% CI does not cross 0, the parameter estimate is statistically significant. 

M8b <- polr(y_var ~ PDSI_STD_2012, data = data_subset, Hess = TRUE)
summary(M8b)
(ctable <- coef(summary(M8b))) #store table
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 ## calculate and store p values
(ctable <- cbind(ctable, "p value" = p)) ## combined table
(ci <- confint(M8b)) # default method gives profiled CIs. If the 95% CI does not cross 0, the parameter estimate is statistically significant. 

M9b <- polr(y_var ~ PDSI_STD_2007, data = data_subset, Hess = TRUE)
summary(M9b)
(ctable <- coef(summary(M9b))) #store table
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 ## calculate and store p values
(ctable <- cbind(ctable, "p value" = p)) ## combined table
(ci <- confint(M9b)) # default method gives profiled CIs. If the 95% CI does not cross 0, the parameter estimate is statistically significant. 

M10b <- polr(y_var ~ PDSI_STD_2002, data = data_subset, Hess = TRUE)
summary(M10b)
(ctable <- coef(summary(M10b))) #store table
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 ## calculate and store p values
(ctable <- cbind(ctable, "p value" = p)) ## combined table
(ci <- confint(M10b)) # default method gives profiled CIs. If the 95% CI does not cross 0, the parameter estimate is statistically significant. 

#save(mod,file= paste("C:\\Users\\rschattman\\Documents\\Research\\climate-drivers\\model",i,"output.rdata", sep ="")) # This save would be useful if you wanted to save each of the 11 models as their own file

mod_outfilename <- paste0("M1b",out_suffix,".RData")

save(Mod1b, 
     file = file.path(out_dir,mod_outfilename))
