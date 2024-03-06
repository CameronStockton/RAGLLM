library("readxl")
nhefs <- read_excel("CHANGE DIRECTORY/nhefs.xlsx")
nhefs.gest <- nhefs[which(!is.na(nhefs$wt82)),] # provisionally ignore subjects with missing values for weight in 1982

####################################################
# G-estimation: Checking one possible value of psi #
####################################################

nhefs.gest$psi <- 3.46
nhefs.gest$Hpsi <- nhefs.gest$wt82_71 - nhefs.gest$psi*nhefs.gest$qsmk

fit <- glm(qsmk ~ sex + race + age + I(age*age) + as.factor(education)
           + smokeintensity + I(smokeintensity*smokeintensity) + smokeyrs
           + I(smokeyrs*smokeyrs) + as.factor(exercise) + as.factor(active)
           + wt71 + I(wt71*wt71) + Hpsi, family=binomial, data=nhefs.gest)
summary(fit)


##########################################################
# G-estimation: Checking multiple possible values of psi #
##########################################################

grid <- seq(from = 2.5,to = 4.5, by = 0.1)
j = 0
Hpsi.coefs <- cbind(rep(NA,length(grid)), rep(NA, length(grid)))
colnames(Hpsi.coefs) <- c("Estimate", "p-value")

for (i in grid){
  psi = i
  j = j+1
  nhefs.gest$Hpsi <- nhefs.gest$wt82_71 - psi * nhefs.gest$qsmk 
  
  gest.fit <- glm(qsmk ~ sex + race + age + I(age*age) + as.factor(education)
             + smokeintensity + I(smokeintensity*smokeintensity) + smokeyrs
             + I(smokeyrs*smokeyrs) + as.factor(exercise) + as.factor(active)
             + wt71 + I(wt71*wt71) + Hpsi, family=binomial, data=nhefs.gest)
  Hpsi.coefs[j,1] <- summary(gest.fit)$coefficients["Hpsi", "Estimate"]
  Hpsi.coefs[j,2] <- summary(gest.fit)$coefficients["Hpsi", "Pr(>|z|)"]
}

rownames(Hpsi.coefs) <- grid
Hpsi.coefs

####################################################
# G-estimation + IPW for selection bias adjustment #
####################################################

nhefs$c <- ifelse(is.na(nhefs$wt82), 1, 0)

# estimation of denominator of censoring weights
cw.denom <- glm(c==0 ~ sex + race + age + I(age*age) + as.factor(education)
                + smokeintensity + I(smokeintensity*smokeintensity) + smokeyrs
                + I(smokeyrs*smokeyrs) + as.factor(exercise) + as.factor(active)
                + wt71 + I(wt71*wt71) + qsmk, family=binomial, data=nhefs)

nhefs.c <- nhefs[which(!is.na(nhefs$wt82)),]
nhefs.c$pd.c <- predict(cw.denom, nhefs.c, type="response")

nhefs.c$wc <- 1/(nhefs.c$pd.c)
summary(nhefs.c$wc)


#######################################
# G-estimation: Closed-form estimator #
#######################################

logit.est <- glm(qsmk ~ sex + race + age + I(age*age) + as.factor(education)
                 + smokeintensity + I(smokeintensity*smokeintensity) + smokeyrs
                 + I(smokeyrs*smokeyrs) + as.factor(exercise) + as.factor(active)
                 + wt71 + I(wt71*wt71), family=binomial(), data=nhefs.c, weight=wc)
summary(logit.est)
nhefs.c$pqsmk <- predict(logit.est, nhefs.c, type = "response")
summary(nhefs.c$pqsmk)

# solve sum(w_c * H(psi) * (qsmk - E[qsmk | L]))  = 0
# for a single psi and H(psi) = wt82_71 - psi * qsmk
# this can be solved as psi = sum( w_c * wt82_71 * (qsmk - pqsmk)) / sum(w_c * qsmk * (qsmk - pqsmk))

with(nhefs.c, sum(wc*wt82_71*(qsmk - pqsmk)) / sum(wc*qsmk*(qsmk - pqsmk)))

# finding the approximate 95% CI
#install.packages("geepack")
library("geepack")
grid <- seq(from = 2.5,to = 4.5, by = 0.1)
j = 0
Hpsi.coefs <- cbind(rep(NA,length(grid)), rep(NA, length(grid)))
colnames(Hpsi.coefs) <- c("Estimate", "p-value")

for (i in grid){
  psi = i
  j = j+1
  nhefs.c$Hpsi <- nhefs.c$wt82_71 - psi * nhefs.c$qsmk 
  
  gest.fit <- geeglm(qsmk ~ sex + race + age + I(age*age) + as.factor(education)
                  + smokeintensity + I(smokeintensity*smokeintensity) + smokeyrs
                  + I(smokeyrs*smokeyrs) + as.factor(exercise) + as.factor(active)
                  + wt71 + I(wt71*wt71) + Hpsi, family=binomial, data=nhefs.c,
                  weights=wc, id=seqn, corstr="independence")
  Hpsi.coefs[j,1] <- summary(gest.fit)$coefficients["Hpsi", "Estimate"]
  Hpsi.coefs[j,2] <- summary(gest.fit)$coefficients["Hpsi", "Pr(>|W|)"]
}

rownames(Hpsi.coefs) <- grid
Hpsi.coefs
