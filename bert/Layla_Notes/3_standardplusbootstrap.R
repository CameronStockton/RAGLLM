#install.packages("boot") # install package if required

library("readxl")
library("boot")
nhefs <- read_excel("CHANGE DIRECTORY/nhefs.xlsx")

# function to calculate difference in means
standardization <- function(data, indices) {
  # create a dataset with 3 copies of each subject
  d <- data[indices,] # 1st copy: equal to original one`
  d$interv <- -1
  d0 <- d # 2nd copy: treatment set to 0, outcome to missing
  d0$interv <- 0
  d0$qsmk <- 0
  d0$wt82_71 <- NA
  d1 <- d # 3rd copy: treatment set to 1, outcome to missing
  d1$interv <- 1
  d1$qsmk <- 1
  d1$wt82_71 <- NA
  d.onesample <- rbind(d, d0, d1) # combining datasets

  # linear model to estimate mean outcome conditional on treatment and confounders
  # parameters are estimated using original observations only (interv= -1)
  # parameter estimates are used to predict mean outcome for observations with set 
  # treatment (interv=0 and interv=1)
  fit <- glm(wt82_71 ~ qsmk + sex + race + age + I(age*age) + 
               as.factor(education) + smokeintensity + 
               I(smokeintensity*smokeintensity) + smokeyrs + I(smokeyrs*smokeyrs) +
               as.factor(exercise) + as.factor(active) + wt71 + I(wt71*wt71), 
             data=d.onesample)
  
  d.onesample$predicted_meanY <- predict(fit, d.onesample)
  
  # estimate mean outcome in each of the groups interv=-1, interv=0, and interv=1
  return(c(mean(d.onesample$predicted_meanY[d.onesample$interv==-1]),
    mean(d.onesample$predicted_meanY[d.onesample$interv==0]),
    mean(d.onesample$predicted_meanY[d.onesample$interv==1]),
    mean(d.onesample$predicted_meanY[d.onesample$interv==1])-
      mean(d.onesample$predicted_meanY[d.onesample$interv==0])))
}

# bootstrap
set.seed(543540)
results <- boot(data=nhefs, statistic=standardization, R=500)

# generating confidence intervals
se <- c(sd(results$t[,1]), sd(results$t[,2]), 
        sd(results$t[,3]), sd(results$t[,4]))
mean <- c(mean(results$t[,1]), mean(results$t[,2]), 
          mean(results$t[,3]), mean(results$t[,4]))
ll <- mean - qnorm(0.975)*se
ul <- mean + qnorm(0.975)*se

bootstrap <- data.frame(cbind(c("Observed", "No Treatment", "Treatment", 
                                "Treatment - No Treatment"), mean, se, ll, ul))
bootstrap
