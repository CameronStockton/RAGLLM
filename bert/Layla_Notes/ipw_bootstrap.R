# install packages and load library for sas7bdat files;
# install.packages("sas7bdat")
# install.packages("geepack")
library(sas7bdat)
library(geepack)
library(boot)

## Load data
nhefs <- read.sas7bdat("CHANGE DIRECTORY/nhefs.sas7bdat")
nhefs <- nhefs[which(!is.na(nhefs$wt82_71)),]

#####################
# IPW Bootstrapping #
#####################

# function to calculate difference in means
ipw <- function(data, indices) {
  d <- data[indices, ]
  # estimating denominator of IP weights
  ipw.denom <- glm(qsmk ~ sex + race + age + weakheart + smokeintensity + 
                     asthma + bronch, data=d, family=binomial())
  d$predict_d <- predict(ipw.denom, d, type='response')
  # estimating numerator of IP weights
  ipw.num <- glm(qsmk ~ 1, data=d, family=binomial())
  d$predict_n <- predict(ipw.num, d, type='response')
  # calculating weights
  d$sw <- ifelse(d$qsmk==1, d$predict_n/d$predict_d, (1-d$predict_n)/(1-d$predict_d))
  # msm
  msm <- geeglm(wt82_71 ~ qsmk, data=d, weights=sw, id=seqn, corstr="independence")
  return(msm$coefficients[2])
}

# bootstrap
set.seed(1)
results <- boot(data=nhefs, statistic=ipw, R=100)

# generating confidence intervals
se <- sd(results$t)
mean <- results$t0
ll <- mean - qnorm(0.975)*se
ul <- mean + qnorm(0.975)*se
c(mean, ll, ul)
