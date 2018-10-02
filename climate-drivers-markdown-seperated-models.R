## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
	echo = TRUE,
	message = TRUE,
	warning = TRUE
)

## ------------------------------------------------------------------------
library(MASS)
library(lme4)
library(rstanarm)
library("bayesplot")
library("ggplot2")
library("loo")

## ------------------------------------------------------------------------
dataDR <- read.csv("/nfs/bparmentier-data/Data/projects/soilsesfeedback-data/data/NRCS_FSAMergeDataset_w_PDSI2_7_28_18.csv", header = TRUE)

## ------------------------------------------------------------------------
y_var_name <- "Concern_DryDrought"

dataDR$y_var <- dataDR[[y_var_name]]

dataDR$stdiv <- factor(dataDR$stdiv)
dataDR$Agency <- factor(dataDR$Agency)
dataDR$y_var <- factor(dataDR$y_var)
dataDR$PDSI_MEAN_2016 <- factor(dataDR$PDSI_MEAN_2016)
dataDR$PDSI_MEAN_2014 <- factor(dataDR$PDSI_MEAN_2014)
dataDR$PDSI_MEAN_2012 <- factor(dataDR$PDSI_MEAN_2012)
dataDR$PDSI_MEAN_2007 <- factor(dataDR$PDSI_MEAN_2007)
dataDR$PDSI_MEAN_2002 <- factor(dataDR$PDSI_MEAN_2002)
dataDR$PDSI_STD_2016 <- factor(dataDR$PDSI_STD_2016)
dataDR$PDSI_STD_2014 <- factor(dataDR$PDSI_STD_2014)
dataDR$PDSI_STD_2012 <- factor(dataDR$PDSI_STD_2012)
dataDR$PDSI_STD_2007 <- factor(dataDR$PDSI_STD_2007)
dataDR$PDSI_STD_2002 <- factor(dataDR$PDSI_STD_2002)

## ------------------------------------------------------------------------
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


## ------------------------------------------------------------------------
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
               

## ------------------------------------------------------------------------

### This should be a loop or a function:


run_model_ordinal_logistic <- function(i,list_param){
  model_type <- "bayes_stan"
  
  
}

model_type <- "bayes_stan"
mod_noPDSI <- "y_var ~ PercLossDrought + stdiv"

mod1_noPDSI <- try(stan_polr(mod_noPDSI, 
                         data = data_subset, 
                         #prior = R2(0.25), 
                         prior = NULL,
                         prior_counts = dirichlet(1),
                         shape = NULL,
                         chains = 4, 
                         #cores = CORES, 
                         seed = 1234, 
                         iter = 200))

#Got this error
#Error in qr.solve(decomposition, Q) : singular matrix 'a' in solve

mod2_mean2016 <- try(stan_polr(mod_mean2016, 
                         data = data_subset, 
                         #prior = R2(0.25), 
                         prior = NULL,
                         prior_counts = dirichlet(1),
                         shape = NULL,
                         chains = 4, 
                         #cores = CORES, 
                         seed = 1234, 
                         iter = 200))
#took about 10min for mod2_mean2016

mod3_mean2014 <- try(stan_polr(mod_mean2014, 
                         data = data_subset, 
                         #prior = R2(0.25), 
                         prior = NULL,
                         prior_counts = dirichlet(1),
                         shape = NULL,
                         chains = 4, 
                         #cores = CORES, 
                         seed = 1234, 
                         iter = 200))

#took about 10min for mod3_mean2014

mod4_mean2012 <- try(stan_polr(mod_mean2012, 
                         data = data_subset, 
                         #prior = R2(0.25), 
                         prior = NULL,
                         prior_counts = dirichlet(1),
                         shape = NULL,
                         chains = 4, 
                         #cores = CORES, 
                         seed = 1234, 
                         iter = 200))

mod5_mean2007 <- try(stan_polr(mod_mean2007, 
                         data = data_subset, 
                         #prior = R2(0.25), 
                         prior = NULL,
                         prior_counts = dirichlet(1),
                         shape = NULL,
                         chains = 4, 
                         #cores = CORES, 
                         seed = 1234, 
                         iter = 200))

mod6_mean2002 <- try(stan_polr(mod_mean2002, 
                         data = data_subset, 
                         #prior = R2(0.25), 
                         prior = NULL,
                         prior_counts = dirichlet(1),
                         shape = NULL,
                         chains = 4, 
                         #cores = CORES, 
                         seed = 1234, 
                         iter = 200))

mod7_STD2016 <- try(stan_polr(mod_STD2016, 
                         data = data_subset, 
                         #prior = R2(0.25), 
                         prior = NULL,
                         prior_counts = dirichlet(1),
                         shape = NULL,
                         chains = 4, 
                         #cores = CORES, 
                         seed = 1234, 
                         iter = 200))

mod8_STD2014 <- try(stan_polr(mod_STD2014, 
                         data = data_subset, 
                         #prior = R2(0.25), 
                         prior = NULL,
                         prior_counts = dirichlet(1),
                         shape = NULL,
                         chains = 4, 
                         #cores = CORES, 
                         seed = 1234, 
                         iter = 200))

mod9_STD2012 <- try(stan_polr(mod_STD2012, 
                         data = data_subset, 
                         #prior = R2(0.25), 
                         prior = NULL,
                         prior_counts = dirichlet(1),
                         shape = NULL,
                         chains = 4, 
                         #cores = CORES, 
                         seed = 1234, 
                         iter = 200))

mod10_STD2007 <- try(stan_polr(mod_STD2007, 
                         data = data_subset, 
                         #prior = R2(0.25), 
                         prior = NULL,
                         prior_counts = dirichlet(1),
                         shape = NULL,
                         chains = 4, 
                         #cores = CORES, 
                         seed = 1234, 
                         iter = 200))

mod11_STD2002 <- try(stan_polr(mod_STD2002, 
                         data = data_subset, 
                         #prior = R2(0.25), 
                         prior = NULL,
                         prior_counts = dirichlet(1),
                         shape = NULL,
                         chains = 4, 
                         #cores = CORES, 
                         seed = 1234, 
                         iter = 200))

  print(paste("i=",i))
  list_mod[[i]] <- mod   
  summary(list_mod[[1]]) 
  #save(mod,file= paste("C:\\Users\\rschattman\\Documents\\Research\\climate-drivers\\model",i,"output.rdata", sep ="")) # This save would be useful if you wanted to save each of the 11 models as their own file
  
save(data1, file = "data.RData")


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
loo_mod2 <- try(loo(mod2_mean2016, k_threshold = 0.7))
loo_mod3 <- try(loo(mod3_mean2014,k_threshold = 0.7))
loo_mod4 <- try(loo(mod4_mean2012, k_threshold = 0.7))
#loo_mod5 <- try(loo(mod5_mean2007, k_threshold = 0.7))
loo_mod6 <- try(loo(mod6_mean2002, k_threshold = 0.7))
loo_mod7 <- try(loo(mod7_STD2016, k_threshold = 0.7))
loo_mod8 <- try(loo(mod8_STD2014))                    #no observations with pareto_k > 0.7
loo_mod9 <- try(loo(mod9_STD2012))                    #no observations with pareto_k > 0.7
loo_mod10 <- try(loo(mod10_STD2007, k_threshold = 0.7))
loo_mod11 <- try(loo(mod11_STD2002, k_threshold = 0.7))


## ------------------------------------------------------------------------
ls(mod2_mean2016)

plot(loo(mod2_mean2016, k_threshold = 0.7))

loo(mod2_mean2016, k_threshold = 0.7)

loo.stanreg(mod2_mean2016, k_threshold = 0.7)

reloo(x, loo_x, obs = bad_obs)

log_lik.stanreg(fit_j, newdata = d[omitted, , drop = FALSE], offset = x$offset[omitted], newx = get_x(x)[omitted, , drop = FALSE], stanmat = as.character.stanreg(fit_j))

ll_args.stanreg(object, newdata = newdata, offset = offset, reloo_or_kfold = calling_fun %in% c("kfold", "reloo"), ...)


## ------------------------------------------------------------------------
loo2 <- (loo(mod2_mean2016, 
             save_psis = TRUE))
print(loo2)

## ------------------------------------------------------------------------
compare_models(mod)

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

