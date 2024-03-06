library("readxl")
nhefs <- read_excel("CHANGE DIRECTORY/nhefs.xlsx")
summary(nhefs$price82)

# for simplicity, ignore subjects with missing outcome or missing instrument
nhefs.iv <- nhefs[which(!is.na(nhefs$wt82) & !is.na(nhefs$price82)),]
nhefs.iv$highprice <- ifelse(nhefs.iv$price82>=1.5, 1, 0)

t.test(wt82_71 ~ highprice, data=nhefs.iv)

prop.table(table(nhefs.iv[,c("highprice", "qsmk")]),1)


######################################################################
# Estimating the average causal effect using the standard IV estimator 
# via two-stage-least-squares regression
######################################################################

#install.packages ("sem") # install package if required
library(sem) 

model1 <- tsls(wt82_71 ~ qsmk, ~ highprice, data = nhefs.iv)
summary(model1)
confint(model1) 


######################################################################
# Estimating the average causal using the standard IV estimator
# with alternative proposed instruments
######################################################################

summary(tsls(wt82_71 ~ qsmk, ~ ifelse(price82 >= 1.6, 1, 0), data = nhefs.iv))
summary(tsls(wt82_71 ~ qsmk, ~ ifelse(price82 >= 1.7, 1, 0), data = nhefs.iv))
summary(tsls(wt82_71 ~ qsmk, ~ ifelse(price82 >= 1.8, 1, 0), data = nhefs.iv))
summary(tsls(wt82_71 ~ qsmk, ~ ifelse(price82 >= 1.9, 1, 0), data = nhefs.iv))


######################################################################
# Estimating the average causal using the standard IV estimator
# Conditional on baseline covariates
######################################################################

model2 <- tsls(wt82_71 ~ qsmk + sex + race + age + smokeintensity + smokeyrs + 
                 as.factor(exercise) + as.factor(active) + wt71,
               ~ highprice + sex + race + age + smokeintensity + smokeyrs + as.factor(exercise) +
                 as.factor(active) + wt71, data = nhefs.iv)
summary(model2)