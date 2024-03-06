library(sas7bdat)
nhefs.gest <- read.sas7bdat("CHANGE DIRECTORY/nhefs.sas7bdat")

##########################################################
# G-estimation: Checking multiple possible values of psi #
##########################################################

grid <- seq(from = 2.0, to = 4.5, by = 0.01)
j = 0
Hpsi.coefs1 <- cbind(rep(NA,length(grid)), rep(NA,length(grid)), rep(NA, length(grid)))
colnames(Hpsi.coefs1) <- c("psi", "Estimate", "p-value")

for (i in grid){
  psi = i
  j = j+1
  nhefs.gest$Hpsi <- nhefs.gest$wt82_71 - psi * nhefs.gest$qsmk 
  
  gest.fit <- glm(qsmk ~ sex + race + age + smokeintensity + asthma + weakheart + Hpsi, family=binomial, data=nhefs.gest)
  
  Hpsi.coefs1[j,1] <- i
  Hpsi.coefs1[j,2] <- summary(gest.fit)$coefficients["Hpsi", "Estimate"]
  Hpsi.coefs1[j,3] <- summary(gest.fit)$coefficients["Hpsi", "Pr(>|z|)"]
}

rownames(Hpsi.coefs1) <- grid
Hpsi.coefs1

#for an example of how to incorporate IP weights to adjust for selection bias into the analysis, see posted code walkthrough video