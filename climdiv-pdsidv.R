# Get data from drought website by climate division and calculate means and sd for 1,3,5,10,15 yrs.
# https://www.ncdc.noaa.gov/cag/national/time-series/110/pdsi/1/6/1895-2018?base_prd=true&firstbaseyear=1901&lastbaseyear=2000
# ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/drought-readme.txt

library(dplyr)
library(stringr)
library(reshape2)

df <- read.csv("data/climdiv-pdsidv.csv",
                colClasses = c(DIVISION.NUMBER='character')) %>%
      mutate(STDIV = str_c(STATE.CODE, "0", DIVISION.NUMBER)) %>%
      select(STDIV, YEAR, ends_with('VALUE')) %>%
      filter(df$YEAR >= 1968) %>%
      melt(id.vars = c('STDIV', 'YEAR'), value.name = 'PDSI')

