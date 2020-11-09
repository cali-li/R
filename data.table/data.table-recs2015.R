#
# This script contains data.table usage for dataset recs2015.
#
# Author: Cali Li
# Date: 11/27/2019

#80: ---------------------------------------------------------------------------

# load packages: ---------------------------------------------------------------
library(data.table)
library(dplyr)

# load data
recs = fread('recs2015_public_v4.csv')

# compute proportion based on nweight
prop_nweight = recs[, uatyp := ifelse(UATYP10 == "R",
                                      "R_nweight", "U_nweight")] %>%
  .[, .(DOEID, uatyp, NWEIGHT, DIVISION, INTERNET)] %>%
  .[, nweight_internet := ifelse(INTERNET == 1, NWEIGHT, 0)] %>%
  .[, .(sum_nweight_internet = sum(nweight_internet), 
        sum_nweight = sum(NWEIGHT)), by = .(uatyp, DIVISION)] %>%
  .[, prop := sum_nweight_internet / sum_nweight] %>%
  .[, .(uatyp, division = DIVISION, prop)] %>%
  melt(id = 1:2) %>%
  dcast(division ~ uatyp)

# compute proportion based on brrwt*
prop_brrwt = recs[,uatyp := ifelse(UATYP10 == "R", "R_brrwt", "U_brrwt")] %>%
  .[, .(DOEID, uatyp, DIVISION, INTERNET, .SD),
    .SDcols = paste("BRRWT", 1:96, sep = "")] %>%
  melt(id = 1:4) %>%
  .[, .(sum_brrwt_internet = sum(value*INTERNET), 
        sum_brrwt = sum(value)), by = .(uatyp, DIVISION, variable)] %>%
  .[, prop := sum_brrwt_internet / sum_brrwt] %>%
  .[, .(uatyp, division = DIVISION, variable, prop)] %>%
  dcast(division + variable ~ uatyp) %>%
  .[prop_nweight, on = 'division'] %>%
  
  # compute se and CI based on brrwt*
  .[, se := sqrt(sum((abs(R_nweight - U_nweight) - abs(R_brrwt-U_brrwt))^2)
                 *4/96), by = division] %>%
  .[, `:=`(disparsity = round(abs(R_nweight - U_nweight), 3),
           lwr = round(abs(R_nweight - U_nweight) - se, 3),
           upr = round(abs(R_nweight - U_nweight) + se, 3))] %>%
  
  # output a table
  .[, .(Division = replace(unique(division), 1 : 10, 
                           c('New England', 'Middle Atlantic',
                             'East North Central', 'West North Central',
                             'South Atlantic', 'East South Central',
                             'West South Central', 'Mountain North',
                             'Mountain South', 'Pacific')),
        Disparsity = unique(disparsity),
        Lwr = unique(lwr),
        Upr = unique(upr))] %>%
  .[order(-Disparsity)] %>%
  knitr::kable(align = 'c')