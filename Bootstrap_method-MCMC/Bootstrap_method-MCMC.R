#
# Updated: 11/8/2019
# Author: Cali Li
# Application for Bootstrap method and MCMC.
#
# Question 1.a
theta_compute = function(x,y,alpha = 0.05) {
  
  # Input should be vectors.
  stopifnot(is.vector(x) & is.vector(y))
  
  # Compute theta
  theta_exp = mean(x) / mean(y)
  
  # Compute sigma
  theta = c(((sum(x)-x)/(length(x)-1))/mean(y),
            mean(x)/((sum(y)-y)/(length(y)-1)))

  # Confidence intervial
  #se = sd(theta)
  se = sqrt(sum((as.numeric(theta)-mean(theta))^2)*(length(theta)-1)
            /length(theta))
  upper = theta_exp + qnorm(1-alpha/2)*se
  lower = theta_exp + qnorm(alpha/2)*se
  result = round(c(lower,upper),3)
  dim(result) = c(2,1)
  return(result)
}

# Question 1.b -----------------------------------------------------------------
theta_compute2 = function(x,y,alpha = 0.05) {
  
  # Input should be vectors.
  stopifnot(is.vector(x) & is.vector(y))
  
  # Number of bootstrap samples
  B = 1e3
  boot_samples_x = sample(x, length(x) * B, replace=TRUE)
  boot_samples_y = sample(y, length(y) * B, replace=TRUE)
  dim(boot_samples_x) = c(length(x), B)
  dim(boot_samples_y) = c(length(y), B)
  boot_x = colMeans(boot_samples_x)
  boot_y = colMeans(boot_samples_y)
  #boot_x = apply(boot_samples_x, 2, function(t) unname(mean(t)))
  #boot_y = apply(boot_samples_y, 2, function(t) unname(mean(t)))
  boot_theta = boot_x / boot_y
  
  # The percentile method
  upr_1 = quantile(boot_theta, 1-alpha/2)
  lwr_1 = quantile(boot_theta, alpha/2)
  
  # The basic bootstrap method
  upr_2 = 2*mean(x)/mean(y) - lwr_1
  lwr_2 = 2*mean(x)/mean(y) - upr_1
  
  # The normal approximation with bootstrap standard error
  std = sd(boot_theta)
  upr_3 = mean(x)/mean(y) + qnorm(1-alpha/2)*std
  lwr_3 = mean(x)/mean(y) - qnorm(1-alpha/2)*std
  
  # Output
  est = round(c(lwr_1, upr_1, lwr_2, upr_2, lwr_3, upr_3),3)
  dim(est) = c(2,3)
  return(est)
}

# Question 1.c -----------------------------------------------------------------
library(datasets)
data(ToothGrowth)

# Compute the CI of the rate
x = ToothGrowth[which(ToothGrowth$supp == 'OJ'),]$len
y = ToothGrowth[which(ToothGrowth$supp == 'VC'),]$len

# Tidy table
cap_title = '**Table 1.** *Estimation for the rate OJ to VC.*'
cap = paste(cap_title)
row = c('lwr_dose_as_0.5', 'upr_dose_as_0.5',
        'lwr_dose_as_1.0', 'upr_dose_as_1.0',
        'lwr_dose_as_2.0', 'upr_dose_as_2.0')
compute = round(c(mean(x[1:10])/mean(y[1:10]), mean(x[1:10])/mean(y[1:10]),
            mean(x[11:20])/mean(y[11:20]), mean(x[11:20])/mean(y[11:20]),
            mean(x[21:30])/mean(y[21:30]), mean(x[21:30])/mean(y[21:30])), 3)
compute1 = rbind(theta_compute(x[1:10],y[1:10]),
                 theta_compute(x[11:20],y[11:20]),
                 theta_compute(x[21:30],y[21:30]))
compute2 = rbind(theta_compute2(x[1:10],y[1:10]),
                theta_compute2(x[11:20],y[11:20]),
                theta_compute2(x[21:30],y[21:30]))
compute = cbind(row,compute,compute1,compute2)
knitr::kable(as.data.frame(compute), caption = cap,
             col.names = c(' ','point estimate','jackknife','percentile method',
                           'basic bootstrap','normal approx'))

# Question 2.a -----------------------------------------------------------------
theta_compute_matrix = function(x,y,alpha = 0.05) {
  
  # cols represent Monte Carlo samples.
  stopifnot(ncol(x) == ncol(y))
  
  theta_exp = colMeans(x) / colMeans(y)
  theta_ci_x = ((colSums(x) - x) / (nrow(x) - 1)) / colMeans(y)
  theta_ci_y = colMeans(x) / ((colSums(y) - y) / (nrow(y) - 1))
  dim(theta_ci_x) = c(nrow(x), ncol(x))
  dim(theta_ci_y) = c(nrow(y), ncol(y))
  theta_ci = rbind(theta_ci_x, theta_ci_y)
  
  se = sqrt(colSums((theta_ci-colMeans(theta_ci))^2)*(nrow(theta_ci)-1)
            /nrow(theta_ci))
  
  #se = sd(theta_ci)
  upper = theta_exp + qnorm(1-alpha/2)*se/sqrt(nrow(theta_ci))
  lower = theta_exp + qnorm(alpha/2)*se/sqrt(nrow(theta_ci))
  
  result = c(lower,upper)
  dim(result) = c(ncol(x),2)
  return(result)
}

# Question 2.b -----------------------------------------------------------------
theta_compute2_matrix = function(x,y,alpha = 0.05) {
  
  # cols represent Monte Carlo samples.
  stopifnot(ncol(x) == ncol(y))
  
  nrowx = nrow(x)
  nrowy = nrow(y)
  ncoly = ncol(y)
  
  theta = colMeans(x) / colMeans(y)
  
  x = as.vector(x)
  y = as.vector(y)
  
  B = 1e3
  boot_samples_x = sample(x, length(x) * B, replace=TRUE)
  boot_samples_y = sample(y, length(y) * B, replace=TRUE)
  dim(boot_samples_x) = c(nrowx, ncoly*B)
  dim(boot_samples_y) = c(nrowy, ncoly*B)
  boot_theta = colMeans(boot_samples_x) / colMeans(boot_samples_y)
  dim(boot_theta) = c(B,ncoly)
  
  # Transpose the vector boot_theta and compute quantiles
  upr_1 = apply(boot_theta,2,function(x) quantile(x,1-alpha/2))
  lwr_1 = apply(boot_theta,2,function(x) quantile(x,alpha/2))
  
  # The basic bootstrap method
  upr_2 = 2*theta - lwr_1
  lwr_2 = 2*theta - upr_1
  
  # The normal approximation with bootstrap standard error
  #se=sd(boot_theta)
  se=sqrt(B/(B-1)*colMeans(boot_theta^2)-(colMeans(boot_theta))^2)
  upr_3 = theta + qnorm(1-alpha/2)*se
  lwr_3 = theta - qnorm(1-alpha/2)*se
  
  # Output
  est = c(lwr_1, upr_1, lwr_2, upr_2, lwr_3, upr_3)
  dim(est) = c(ncoly, 2*3)
  return(est)
}

# Question 2.c -----------------------------------------------------------------
# Simulate parameters & data
# Use Normal distribution here since that is one of the most useful distribution
n = 1e2
x = rexp(n)
y = rexp(n)
dim(x) = c(10,10)
dim(y) = c(10,10)

# Estimate value for the jackknife CI
target = colMeans(x) / colMeans(y)

# Coverage probilities for the jackknife CI
result_1 = theta_compute_matrix(x,y)
cvrg_prob_1 = mean({result_1[,1] < target} & {result_1[,2] > target})
print(cvrg_prob_1)

# Coverage probilities for the quantile CI
result_2 = theta_compute2_matrix(x,y)
cvrg_prob_2 = mean({result_2[,1] < target} & {result_2[,2] > target})
print(cvrg_prob_2)

# Coverage probilities for the bootstrap CI
cvrg_prob_3 = mean({result_2[,3] < target} & {result_2[,4] > target})
print(cvrg_prob_3)

# Coverage probilities for the normal CI
cvrg_prob_4 = mean({result_2[,5] < target} & {result_2[,6] > target})
print(cvrg_prob_4)

# They both have probilities equal to 1.

# Average length for the jackknife CI
mean(result_1[,2]-result_1[,1])
# Average length for the quantile CI
mean(result_2[,2]-result_2[,1])
# Average length for the bootstrap CI
mean(result_2[,4]-result_2[,3])
# Average length for the normal CI
mean(result_2[,6]-result_2[,5])

# Average CI length for the jackknife method is 1.72, which is the biggest.
# That means the CI is too vague and compared to other methods 
# it has less information.
# The quantile method and the bootstrap method have the same interval length
# as 1.66, since these two methods have similar way to compute intervals.
# The normal assumption method has the bigger average intervals as 1.67.

# Average shape of the jackknife CI
mean((result_1[,2]-target)/(target-result_1[,1]))
# Average shape of the quantile CI
mean((result_2[,2]-target)/(target-result_2[,1]))
# Average shape of the bootstrap CI
mean((result_2[,4]-target)/(target-result_2[,3]))
# Average shape of the normal CI
mean((result_2[,6]-target)/(target-result_2[,5]))

# The jackknife method and the normal assumption method can give us
# the symmetric CI shape since their average shape are both 1.
# The quantile method has a worse skewed trend as the average shape is 6.68.
# That means the CI has more space above the point estimate.
# The bootstrap method is also bad
# as the average shape is 0.81, which means it still has a skewness.