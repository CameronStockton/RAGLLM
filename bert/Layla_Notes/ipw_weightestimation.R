library("readxl")
nhefs <- read_excel("CHANGE DIRECTORY/nhefs.xlsx")
nhefs.nmv <- nhefs[which(!is.na(nhefs$wt82)),] 
nhefs.nmv$age50 <- ifelse(nhefs.nmv$age>50, 1, 0)

##########################################################
# If QSMK had been conditionally randomized by age group 
##########################################################
  
# analysis without models
#table(nhefs.nmv$age50, nhefs.nmv$qsmk)
prop.table(table(nhefs.nmv$age50, nhefs.nmv$qsmk),1)

nhefs.nmv$w <- ifelse(nhefs.nmv$age50==1, 
                      ifelse(nhefs.nmv$qsmk==1, 1/0.3333, 1/(1-0.3333)),
                      ifelse(nhefs.nmv$qsmk==1, 1/0.2250, 1/(1-0.2250)))

summary(nhefs.nmv$w)

weighted.mean(nhefs.nmv$wt82_71[nhefs.nmv$qsmk==0], w=nhefs.nmv$w[nhefs.nmv$qsmk==0])
weighted.mean(nhefs.nmv$wt82_71[nhefs.nmv$qsmk==1], w=nhefs.nmv$w[nhefs.nmv$qsmk==1])

# estimation of ip weights with a logistic model
fit <- glm(qsmk ~ age50, family=binomial(), data=nhefs.nmv)
summary(fit)
nhefs.nmv$p.qsmk <- predict(fit, type="response") 

table(nhefs.nmv$p.qsmk, nhefs.nmv$age50)

nhefs.nmv$w2 <- ifelse(nhefs.nmv$qsmk==1, 1/nhefs.nmv$p.qsmk, 1/(1-nhefs.nmv$p.qsmk))
summary(nhefs.nmv$w2)

weighted.mean(nhefs.nmv$wt82_71[nhefs.nmv$qsmk==0], w=nhefs.nmv$w2[nhefs.nmv$qsmk==0])
weighted.mean(nhefs.nmv$wt82_71[nhefs.nmv$qsmk==1], w=nhefs.nmv$w2[nhefs.nmv$qsmk==1])

# no association between age50 and qsmk in pseudo-population
xtabs(nhefs.nmv$w2 ~ nhefs.nmv$age50 + nhefs.nmv$qsmk)
prop.table(xtabs(nhefs.nmv$w2 ~ nhefs.nmv$age50 + nhefs.nmv$qsmk),1)

##########################################################
# Observational study: adjusting for several covariates 
##########################################################
  
# estimation of ip weights with a logistic model
fit2 <- glm(qsmk ~ sex + race + age + I(age*age) + as.factor(education)
           + smokeintensity + I(smokeintensity*smokeintensity)
           + smokeyrs + I(smokeyrs*smokeyrs) + as.factor(exercise)
           + as.factor(active) + wt71 + I(wt71*wt71), family=binomial(),
           data=nhefs.nmv)
summary(fit2)
nhefs.nmv$p.qsmk2 <- predict(fit2, nhefs.nmv, type="response")

nhefs.nmv$w3 <- ifelse(nhefs.nmv$qsmk==1, 1/nhefs.nmv$p.qsmk2, 1/(1-nhefs.nmv$p.qsmk2))
summary(nhefs.nmv$w3)

weighted.mean(nhefs.nmv$wt82_71[nhefs.nmv$qsmk==0], w=nhefs.nmv$w3[nhefs.nmv$qsmk==0])
weighted.mean(nhefs.nmv$wt82_71[nhefs.nmv$qsmk==1], w=nhefs.nmv$w3[nhefs.nmv$qsmk==1])

# "check" for positivity
table(nhefs.nmv$age[nhefs.nmv$race == 0 & nhefs.nmv$sex == 1], 
      nhefs.nmv$qsmk[nhefs.nmv$race == 0 & nhefs.nmv$sex == 1])