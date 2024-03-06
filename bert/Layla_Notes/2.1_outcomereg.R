library("readxl")

nhefs <- read_excel("nhefs.xlsx")
nhefs_nmv <- nhefs[which(!is.na(nhefs$wt82)),] # provisionally ignore subjects with missing values for weight in 1982

table(nhefs_nmv$sex, nhefs_nmv$qsmk)
prop.table(table(nhefs_nmv$sex, nhefs_nmv$qsmk), 1)

# Men: stratified analysis without models
summary(nhefs_nmv[which(nhefs_nmv$sex==0 & nhefs_nmv$qsmk==0),]$wt82_71) # non-quitters
summary(nhefs_nmv[which(nhefs_nmv$sex==0 & nhefs_nmv$qsmk==1),]$wt82_71) # quitters

t.test(wt82_71~qsmk, data=nhefs_nmv[which(nhefs_nmv$sex==0),])

# Women: stratified analysis without models
summary(nhefs_nmv[which(nhefs_nmv$sex==1 & nhefs_nmv$qsmk==0),]$wt82_71) # non-quitters
summary(nhefs_nmv[which(nhefs_nmv$sex==1 & nhefs_nmv$qsmk==1),]$wt82_71) # quitters

t.test(wt82_71~qsmk, data=nhefs_nmv[which(nhefs_nmv$sex==1),])

# Stratified analysis with models

## saturated model
sat.model <- glm(wt82_71 ~ qsmk + sex + qsmk*sex, data=nhefs_nmv)
summary(sat.model)

predicted.wt82_71 <- data.frame(c(1,0,1,0),c(1,1,0,0))
names(predicted.wt82_71) <- c("qsmk", "sex")
predicted.wt82_71$sat.model <- predict(sat.model, predicted.wt82_71, type="response")
predicted.wt82_71

## nonsaturated model
nonsat.model <- glm(wt82_71 ~ qsmk + sex, data=nhefs_nmv)
summary(nonsat.model)
predicted.wt82_71$nonsat.model <- predict(nonsat.model, predicted.wt82_71, type="response")
predicted.wt82_71