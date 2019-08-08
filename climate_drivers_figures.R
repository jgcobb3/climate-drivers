############### Climate perception project ########## 
## SESYNC Research Support
##
##Data used in this project comes from three sources: (1) A survey of NRCS and FSA employees conducted by the USDA Climate Hubs in 2016/2017, (2) Crop idemnity payments made by FSA to farmers for weather-related 
##crop loss between 2013-2016, and (3) mean and standard deviations of drought data over 1, 3, 5, 10, and 15 year periods.
##
##
## DATE CREATED: 08/08/2019
## DATE MODIFIED: 
## AUTHORS: Rachel Schattman, Benoit Parmentier  
## Version: 1
## PROJECT: Climate Percecption
## ISSUE: 
## TO DO:



######## Load packages 
library(margins)


######## Good resource
https://osf.io/gyfj7/download


####### Create box plot
marginal_effects(list_mod[[1]], "belief", categorical = TRUE)
