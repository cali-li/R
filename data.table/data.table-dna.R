#
# This script contains data.table usage & parallel computation & future package
## for dataset DNA methylation.
#
# Author: Cali Li
# Date: 11/31/2019

#80: --------------------------------------------------------------------------

# load packages: --------------------------------------------------------------
library(dplyr)
library(tidyr)
library(data.table)

# question b
## load data into R
data = fread('total_data.txt')
data = data[grep('^ch', ID_REF), 
            which(unlist(lapply(data, function(x) !all(is.na(x))))),
            with = FALSE] %>%
  melt(id=1)

# question c
## From the information, we can get that GSM4105187-GSM4105193 are samples for 
## Crohn's disease; while GSM4105194-GSM4105198 are samples for no Crohn's 
## disease.
data = data[, `:=`(sample_group = ifelse(variable %in% unique(variable)[1:7],
                                         "Crohn", "noCrohn"))]

# question d
## cmpute t-stats
t_stats = dcast(data, ID_REF+variable~sample_group) %>%
  .[, .(mean = mean(Crohn, na.rm = TRUE) - mean(noCrohn, na.rm = TRUE),
           std_Crohn = sd(Crohn, na.rm = TRUE),
           std_noCrohn = sd(noCrohn, na.rm = TRUE)),
    by = .(ID_REF)] %>%
  .[, .(ID_REF,
        t_stats = round(mean / (sqrt((6 * (std_Crohn) ^ 2 + 
                                     4 * (std_noCrohn) ^ 2) / 10) *
                               sqrt(1/7 + 1/5)),3))]
    
# question e
data_e = data[, `:=`(probe_group = substr(ID_REF, 0, 5))]

# question f
## compute proportion based on significance
data_f = data_e[t_stats, on = 'ID_REF'] %>%
  .[, p := 2 * pt(t_stats, df = 10)] %>%
  .[, `:=`(sig = 1L * (p < 0.05))] %>%
  .[, .(prop = sum(sig) / .N), by = probe_group]

# question g
t_stat = function(data, type, permute=TRUE) {
  if (permute) {
    data = data[, sample_group1 := sample_group[sample(1:.N, 
                                                       replace = FALSE)]]
  }
  ## Compute t-stats
  data_1 = data[sample_group1 == 'Crohn', .(ccount = .N, cmean = mean(value),
                                           x1 = sum(value^2) - 
                                             .N * ((mean(value))^2)),
                by = ID_REF]
  data_2 = data[sample_group1 == 'noCrohn', .(ncount = .N, 
                                              nmean = mean(value),
                                              x2 = sum(value^2) - 
                                                .N * ((mean(value))^2)),
                by = ID_REF]
  data_t = data_1[data_2, on = 'ID_REF'] %>%
    .[, .(t_stats = (cmean - nmean) / sqrt((x1 + x2)/(ccount + ncount - 2))
          / sqrt(1 / ccount + 1 / ncount)), by = ID_REF] %>%
    .[, `:=`(probe_group = substr(ID_REF, 0, 5))]
  
  ## compute three different computation methods
  if (type == "two_tailed") {
    t = data_t[, p := qt(1-0.05/2, df = 10)] %>%
      .[, `:=`(sig = 1L * (abs(t_stats) > p))] %>%
      .[, .(prop = sum(abs(sig)) / .N), by = probe_group]
  }
  if (type == "greater") {
    t = data_t[, p := qt(1-0.05, df = 10)] %>%
      .[, `:=`(sig = 1L * (t_stats > p))] %>%
      .[, .(prop = sum(sig) / .N), by = probe_group]
  }
  if (type == "lesser") {
    t = data_t[, p := qt(0.05, df = 10)] %>%
      .[, `:=`(sig = 1L * (t_stats < p))] %>%
      .[, .(prop = sum(sig) / .N), by = probe_group]
  }
  return(t)
}

# question h
## compute on the original data
t_stat(data, type = "two_tailed")

## define a new function stat_h
stat_h = function(n) {
  t = t_stat(data, type = "two_tailed")
  return(t)
}
p = function(n) {
  mean(n$prop) / (sd(n$prop)/sqrt(22))
}

## compute time over 1000 permutation
system.time({
  test_h = lapply(1:1000, stat_h)
  p_h = lapply(test_h, p)
})

# question j
## mclapply
## define a new function stat_j using greater method.
stat_j = function(n) {
  t = t_stat(data, type = "greater")
  return(t)
}

## load package and compute time over 1000 permutation
library(parallel)
system.time({
  test_j = mclapply(1:1000, stat_j)
  p_j = mclapply(test_j, p)
})

# question i
## future
## define a new function stat_i using lesser method.
stat_i = function(n) {
  t = t_stat(data, type = "lesser")
  return(t)
}

## load package and compute time over 1000 permutation
library(future)
plan(multisession)
system.time({
  test_h = lapply(1:1000, stat_i)
  p_j = lapply(test_j, p)
})