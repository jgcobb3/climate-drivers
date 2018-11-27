############################## Bayesian modeling with STAN #################### 
##
## This is an example script for Bayesian modeling using STAN to estimate parameters.
##
##
## DATE CREATED: 10/01/2018
## DATE MODIFIED: 11/16/2018
## AUTHORS: Benoit Parmeniter
## Version: 1
## PROJECT: Training for Bayesian modeling
## ISSUE: 
## TO DO:
##
## COMMIT: editing script  to look at the interpretation of outputs
##

## Very good reference:
#http://rpsychologist.com/r-guide-longitudinal-lme-lmer


###### Library used

###### Functions used in this script and sourced from other files

require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
library(rstan)
library(gdata)
library(bayesplot)
library(MASS)
library(lme4)
library(rstanarm)
library(bayesplot)
library(ggplot2)
library(loo)
library(parallel)

###### Functions used in this script and sourced from other files

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

#Benoit setup
script_path <- "/nfs/bparmentier-data/Data/projects/bayesian_methods/scripts"

#functions <- "_functions_11142018d.R"
#source(file.path(script_path,functions))

#########cd ###################################################################
#####  Parameters and argument set up ########### 

#ARGS 1
in_dir <- "/nfs/bparmentier-data/Data/projects/bayesian_methods/data"

#ARGS 2
out_dir <- "/nfs/bparmentier-data/Data/projects/bayesian_methods/outputs"
#ARGS 5:
create_out_dir_param=TRUE #create a new ouput dir if TRUE
#ARGS 7
out_suffix <-"bayesian_ordinal_logistic_rstan_11142018" #output suffix for the files and ouptut folder
#ARGS 8
num_cores <- 2 # number of cores
#ARGS 9
#date_param <- "1982.01.01;1982.12.31" #start date, end date

in_filename <- "Crop_Data_modified.csv"

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

#data_df <- read.table(file.path(in_dir,in_filename),
#                      sep=",",
#                      header=T,
#                     stringsAsFactors = F)

#https://ourcodingclub.github.io/2018/04/17/stan-intro.html

#https://www.analyticsvidhya.com/blog/2016/02/multinomial-ordinal-logistic-regression/
#https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/

#dat <- read.dta("http://www.ats.ucla.edu/stat/data/ologit.dta")
dat <- read.dta("https://stats.idre.ucla.edu/stat/data/ologit.dta")
head(dat)

## one at a time, table apply, pared, and public
lapply(dat[, c("apply", "pared", "public")], table)

#table(apply ~ pared + public,data=dat)

ggplot(dat, aes(x = apply, y = gpa)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  facet_grid(pared ~ public, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

###################################################################
### PART 2: First analyze dataset using ordinal logistic model #########

model_formula <- "apply ~ pared + public + gpa"

#Use proportional odds logistic regression (polr)
mod_polr <- polr(model_formula, data = dat, Hess=TRUE)
summary(mod_polr)

#https://stats.stackexchange.com/questions/89474/interpretation-of-ordinal-logistic-regression

summary(mod_polr)

#0.61594+2.2039

### Checking proportional assumption

#We will use summary.formula from the Hmisc package. It computes the
#mean by classes given a continuous variable

s0 <- summary(apply ~ pared + public + gpa, data=dat)
s0 <- xtabs(as.numeric(apply) ~ pared + public + gpa, data=dat)

s0 <- summary(as.numeric(apply) ~ pared + public + gpa, data=dat)

## let's no use the qlogis function
### we need to use a continuous var so we transform the factor to numeric
mean(as.numeric(dat$apply) >= 1) #mean is 1, because dat$apply is always greater or equal to 1!
#1
mean(as.numeric(dat$apply) >= 2) #mean is 1, because dat$apply is always greater or equal to 1!
#0.45, there are 45% of values greater or equal to 2
mean(as.numeric(dat$apply) >= 3) #mean is 1, because dat$apply is always greater or equal to 1!
#0.1, there are 10% of values greater or equal to 2, which is a probablity that can be used in qlogis

qlogis(1)
qlogis(0.45)

y_test <- dat[dat$pared==1,]$apply
mean(as.numeric(y_test) >= 2) #mean is 1, because dat$apply is always greater or equal to 1!
#0.6825397
qlogis(0.6825397)
#> qlogis(0.6825397)
#[1] 0.7654679
y_test <- dat[dat$pared==0,]$apply
mean(as.numeric(y_test) >= 2) #mean is 1, because dat$apply is always greater or equal to 1!
#0.6825397
qlogis(mean(as.numeric(y_test) >= 2))
#This is the intercept of the model

#The inverse cumulative distribution function (quantile function) of the logistic distribution is a generalization of the logit function. Its derivative is called the quantile density function. They are defined as follows:
  
#{\displaystyle Q(p;\mu ,s)=\mu +s\,\ln \left({\frac {p}{1-p}}\right).} Q(p;\mu ,s)=\mu +s\,\ln \left({\frac  {p}{1-p}}\right).

length(y_test)

sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}

s <- summary(as.numeric(apply) ~ pared + public + gpa, fun=sf,data=dat)

(s <- with(dat, summary(as.numeric(apply) ~ pared + public + gpa, fun=sf)))
s1 <- summary(as.numeric(apply) ~ pared + public + gpa, fun=sf,data=dat)

#### Now use GLM to test the proportionality assumption:
I(as.numeric(dat$apply) >= 2)
glm(I(as.numeric(apply) >= 2) ~ pared, family="binomial", data = dat)
glm((as.numeric(apply) >= 2) ~ pared, family="binomial", data = dat)


#Looking at the intercept for this model (-0.3783), we see that it matches the predicted value in the cell for pared equal
#to “no” in the column for Y>=1, the value below it, 
#for pared equals “yes” is equal to the intercept plus the coefficient for pared (i.e. -0.3783 + 1.1438 = 0.765).

#From	Subject	Received	Size	Categories	
#redminesys@sesync.org	[Research Support - Support #29437] SESYNC Meeting Planner Invitation	Tue 2:50 PM	124 KB		
                       
                       
                       
library(nnet)
mlm <- multinom(apply ~ pared + public + gpa, data=dat)

M1 <- logLik(mod_polr)
M2 <- logLik(mlm)
(G <- -2*(M1[1] - M2[1]))
#> (G <- -2*(M1[1] - M2[1]))
#[1] 3.030909
#> pchisq(G,3,lower.tail = FALSE)
#[1] 0.3868842


#Then we calculate -2 times the difference between log likelihoods to obtain a likelihood ratio
#test statistic and save as G. Finally we calculate a p-value using the pchisq function, 
#which tells us the area under a chi-square distribution with 3 degrees of freedom beyond 3.68. 
#The p-value is quite high which indicates the proportional odds model fits as well as the 
#more complex multinomial logit model. Hosmer and Lemeshow tell us this test is not 
#“completely statistically correct.” Nevertheless it does “provide some evidence of model adequacy.” (page 304)
#For questions or clarifications regarding this article, contact the UVa Library 

###################################################################
### PART 3: First analyze dataset using ordinal logistic model #########

#data,prior = NULL 
#prior_counts = dirichlet(1)
#shape = NULL
#chains = 4
#num_cores = NUL
#seed_val = 100
#iter_val = 200

num_cores <-4
seed_val <- 100

mod_stan_polr <- try(stan_polr(formula=model_formula,
                     data = dat, 
                     prior = NULL, 
                     prior_counts = dirichlet(1),
                     shape = NULL,
                     chains = 4, 
                     cores = num_cores, 
                     seed = 100, 
                     iter = 200))

mod_stan_polr$coefficients
mod_polr$coefficients

##############################   End of script ##################################