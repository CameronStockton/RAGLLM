library("readxl")
nhefs <- read_excel("CHANGE DIRECTORY/nhefs.xlsx")
nhefs.nmv <- nhefs[which(!is.na(nhefs$wt82)),] # provisionally ignore subjects with missing values for weight in 1982
nhefs.nmv$age50 <- ifelse(nhefs.nmv$age>50, 1, 0)

table(nhefs.nmv$sex)
table(nhefs.nmv$age50)

# PS: nonparametric estimation without models
table(nhefs.nmv$age50[nhefs.nmv$sex==0], nhefs.nmv$qsmk[nhefs.nmv$sex==0])
prop.table(table(nhefs.nmv$age50[nhefs.nmv$sex==0], nhefs.nmv$qsmk[nhefs.nmv$sex==0]),1)

table(nhefs.nmv$age50[nhefs.nmv$sex==1], nhefs.nmv$qsmk[nhefs.nmv$sex==1])
prop.table(table(nhefs.nmv$age50[nhefs.nmv$sex==1], nhefs.nmv$qsmk[nhefs.nmv$sex==1]),1)


# PS: nonparametric estimation with models
fit.nonpara <- glm(qsmk~sex+age50+I(sex*age50), data=nhefs.nmv, family=binomial())
summary(fit.nonpara)

nhefs.nmv$p.qsmk1 <- predict(fit.nonpara, type="response")
head(cbind(nhefs.nmv$seqn, nhefs.nmv$sex, nhefs.nmv$age50, nhefs.nmv$p.qsmk1))


# PS: One example of parametric estimation with 2 covariates
fit.para1 <- glm(qsmk~sex+age50, data=nhefs.nmv, family=binomial())
summary(fit.para1)

nhefs.nmv$p.qsmk2 <- predict(fit.para1, type="response")
head(cbind(nhefs.nmv$seqn, nhefs.nmv$sex, nhefs.nmv$age50, nhefs.nmv$p.qsmk2))


# PS: Parametric estimation with many covariates
fit.para2 <- glm(qsmk ~ sex + race + age + I(age*age) + as.factor(education) + smokeintensity
                 + I(smokeintensity*smokeintensity) + smokeyrs + I(smokeyrs*smokeyrs)
                 + as.factor(exercise) + as.factor(active) + wt71 + I(wt71*wt71), 
                 data=nhefs.nmv, family=binomial())
summary(fit.para2)


nhefs.nmv$p.qsmk3 <- predict(fit.para2, type="response")
head(cbind(nhefs.nmv$seqn, nhefs.nmv$sex, nhefs.nmv$age, nhefs.nmv$p.qsmk3))

summary(nhefs.nmv$p.qsmk3)


# Stratification on PS (second example) 
  
## calculation of deciles
nhefs.nmv$ps.dec <- cut(nhefs.nmv$p.qsmk3, 
                        breaks=c(quantile(nhefs.nmv$p.qsmk3, probs=seq(0,1,0.1))),
                        labels=seq(1:10),
                        include.lowest=TRUE)

#install.packages("psych") # install package if required
library("psych")
describeBy(nhefs.nmv$p.qsmk3, nhefs.nmv$ps.dec)

## stratification on PS deciles
for (deciles in c(1:10)) {
  print(t.test(wt82_71~qsmk, data=nhefs.nmv[which(nhefs.nmv$ps.dec==deciles),]))
}

## regression on PS deciles
fit.ps.dec <- glm(wt82_71~qsmk+as.factor(ps.dec), data=nhefs.nmv)
summary(fit.ps.dec)

## regression on continuous PS
fit.ps.cont <- glm(wt82_71~qsmk+p.qsmk3+I(p.qsmk3*p.qsmk3), data=nhefs.nmv)
summary(fit.ps.cont)