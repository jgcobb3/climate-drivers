###########   Climate Drivers Project @ SESYNC CYBERINFRASTRUCTURE COURSE     ##########
###########     Rachel Schattman, Sarah Wiener, John Cobb, Sarah Champion     ##########
###########                       July 2018, Annapolis MD      



#############################         Libraries            #############################

library(MASS)
library(lme4)

#################                     Load Data                          ###############

in_dir <- '/nfs/climatedrivers-data'
in_filename <- 'NRCS_FSAMergeDataset_w_PDSI2.csv'


dataDR <- read.table(file.path(in_dir, in_filename), 
                     sep=",", 
                     header = TRUE)

#####################              Regression models                 ###################

# Development of regression models using 2016-2017 FSA/NRCS Surveys + drought indices


#####################            Variable descriptions              #####################
# dependent variable is y_var. 
# 'Concern_DryDrought' which is a respondent concern about longer
# dry periods and drought, measured on a 4 point scale where 1 = not concerned, 2 =
# slightly concerned, 3 = concerned, 4 = very concerned

# indedependent variable is 'PercLossDrought' which is the 
# average percent loss due to drought in a state averaged over the years 2013-2016 
# and normalized for acres of farmland in the state

# control for 'stdiv', a combination of the statecode and the climate division
# within that state 

# Independent variables (timescales): mean drought descriptions are as follows 
# 1 year (PDSI_MEAN_2016)
# 3 year (PDSI_MEAN_2013)
# 5 year (PDSI_MEAN_2011)
# 10 year (PDSI_MEAN_2006)
# 15 year (PDSI_MEAN_2001)

# Independent variables (timescales): standard deviation drought are as follows: 
# 1 year (PDSI_STD_2016)
# 3 year (PDSI_STD_2013)
# 5 year (PDSI_STD_2011)
# 10 year (PDSI_STD_2006)
# 15 year (PDSI_STD_2001)

###########################     Preliminary data managmement    #####################

# assign the dependent variable



y_var_name <- "Concern_DryDrought"

dataDR$y_var <- dataDR[[y_var_name]]

# make sure that variables are categorical

dataDR$stdiv <- factor(dataDR$stdiv)
dataDR$Agency <- factor(dataDR$Agency)
dataDR$y_var <- factor(dataDR$y_var)
dataDR$PDSI_MEAN_2016 <- factor(dataDR$PDSI_MEAN_2016)
dataDR$PDSI_MEAN_2013 <- factor(dataDR$PDSI_MEAN_2013)
dataDR$PDSI_MEAN_2011 <- factor(dataDR$PDSI_MEAN_2011)
dataDR$PDSI_MEAN_2006 <- factor(dataDR$PDSI_MEAN_2006)
dataDR$PDSI_MEAN_2001 <- factor(dataDR$PDSI_MEAN_2001)
dataDR$PDSI_STD_2016 <- factor(dataDR$PDSI_STD_2016)
dataDR$PDSI_STD_2013 <- factor(dataDR$PDSI_STD_2013)
dataDR$PDSI_STD_2011 <- factor(dataDR$PDSI_STD_2011)
dataDR$PDSI_STD_2006 <- factor(dataDR$PDSI_STD_2006)
dataDR$PDSI_STD_2001 <- factor(dataDR$PDSI_STD_2001)

## remove observations with NAs from the 3 variables selected 
## Number of observations will be reduced

x_var_clean <- c("PercLossDrought", "stdiv", "Agency", 
                 "PDSI_MEAN_2016",
                 "PDSI_MEAN_2013",
                 "PDSI_MEAN_2011",
                 "PDSI_MEAN_2006",
                 "PDSI_MEAN_2001",
                 "PDSI_STD_2016",
                 "PDSI_STD_2013",
                 "PDSI_STD_2011",
                 "PDSI_STD_2006",
                 "PDSI_STD_2001")

y_var_clean <- y_var_name

variables_used <- c(y_var_clean, x_var_clean)


#########

data_subset <- dataDR [,variables_used]
data_subset$y_var <- factor(data_subset[[y_var_name]])

data_subset <- na.omit(data_subset)

data_subset$y_var <- factor(data_subset[[y_var_name]])

#Set up model list
list_models <- c("y_var ~ PercLossDrought + Agency",
                "y_var ~ PercLossDrought + Agency + PDSI_MEAN_2016", 
                "y_var ~ PercLossDrought + Agency + PDSI_MEAN_2013",
                "y_var ~ PercLossDrought + Agency + PDSI_MEAN_2011",
                "y_var ~ PercLossDrought + Agency + PDSI_MEAN_2006",
                "y_var ~ PercLossDrought + Agency + PDSI_MEAN_2001",
                "y_var ~ PercLossDrought + Agency + PDSI_MEAN_2016",
                "y_var ~ PercLossDrought + Agency + PDSI_STD_2016",
                "y_var ~ PercLossDrought + Agency + PDSI_STD_2013",
                "y_var ~ PercLossDrought + Agency + PDSI_STD_2011",
                "y_var ~ PercLossDrought + Agency + PDSI_STD_2006",
                "y_var ~ PercLossDrought + Agency + PDSI_STD_2001")

# input parameters for future function

n_model <- length(list_models)
n_model <- 2
list_mod <- vector("list", length=n_model)
model_type <- "glm"


# Loop 

for(i in 1:n_model){
  formula_model <- list_models[i]
  
  variables_used <- strsplit(formula_model,'\\+')
  variables_used <- unlist(strsplit(variables_used[[1]],'\\~'))
  variables_used <- unlist(lapply(variables_used,function(x){sub(" ","",x)}))
  variables_used <- unlist(lapply(variables_used,function(x){gsub(" ","",x)}))
  
  variables_used
  
  data_subset <- dataDR [,variables_used]
  data_subset <- na.omit(data_subset)
  
  #data_subset$y_var <- factor(data_subset[[y_var_name]])
  
  
  if(model_type == 'glm'){
    data_subset$y_var <- as.numeric(data_subset$y_var)
    
    mod <- try(glm(formula_model,
               family = poisson,
               data = data_subset))
  }
  if(model_type == "ordinal_model"){
    mod <- try(polr(formula_model,
                data = data_subset,
                weights = rep(1, nrow(data_subset)),
                Hess = TRUE,
                start = rep(1,4)))
  }
    list_mod[[i]] <- mod 
}
  
list_mod 
  
### The polr model below was our first attempt, but we could not debug it
  mod <- polr(formula_model,
              data = data_subset,
              weights = rep(1, nrow(data_subset)),
              Hess = TRUE,
              start = rep(1,4))


formula_model <- list_models[1]

data_subset$y_var <- as.numeric(data_subset[[y_var_name]])

mod <- glm(formula_model,
           family = poisson,
           data = data_subset)

mod

### The polr model below was our first attempt, but we could not debug it

