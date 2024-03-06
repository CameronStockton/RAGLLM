library("readxl")
nhefs <- read_excel("CHANGE DIRECTORY/nhefs.xlsx")

# formatting datasets
nhefs.surv <- nhefs[!is.na(nhefs$education),]
nhefs.surv$survtime <- ifelse(nhefs.surv$death==0, 120, 
                              (nhefs.surv$yrdth-83)*12+nhefs.surv$modth-1) # yrdth ranges from 83 to 92;
nhefs.surv <- nhefs.surv[c("qsmk", "seqn", "sex", "race", "age", "education", 
                           "smokeintensity","smokeyrs", "exercise", "active", 
                           "wt71", "survtime", "death", "smkintensity82_71")]

#install.packages("boot")
library("boot")


######################################################################
# Estimation of survival curves via IP weighted hazards model
# with bootstrapping for variance estimation
######################################################################

survival.ipw.boot <- function(data, indices) {
  d <- data[indices,]
  
  # estimation of denominator of ip weights
  p.denom <- glm(qsmk ~ sex + race + age + I(age*age) + as.factor(education)
                 + smokeintensity + I(smokeintensity*smokeintensity)
                 + smokeyrs + I(smokeyrs*smokeyrs) + as.factor(exercise)
                 + as.factor(active) + wt71 + I(wt71*wt71), 
                 data=d, family=binomial())
  d$pd.qsmk <- predict(p.denom, d, type="response")
  
  # estimation of numerator of ip weights
  p.num <- glm(qsmk ~ 1, data=d, family=binomial())
  d$pn.qsmk <- predict(p.num, d, type="response")
  
  # computation of estimated weights
  d$sw.a <- ifelse(d$qsmk==1, d$pn.qsmk/d$pd.qsmk,
                   (1-d$pn.qsmk)/(1-d$pd.qsmk))
  
  # creation of person-month data
  d$count <- as.numeric(rownames(d))
  d.ipw <- data.frame(count=rep(d$count, times=d$survtime))
  d.ipw <- merge(d.ipw, d, by="count")  
  d.ipw$time <- sequence(rle(d.ipw$count)$lengths)-1
  d.ipw$event <- ifelse(d.ipw$time==d.ipw$survtime-1 & 
                          d.ipw$death==1, 1, 0)
  d.ipw$timesq <- d.ipw$time^2
  
  # fit of weighted hazards model
  options(warn=-1)
  ipw.model <- glm(event==0 ~ qsmk + I(qsmk*time) + time + timesq, 
                   family=binomial(), weight=sw.a, data=d.ipw) 
  options(warn=0)
  
  # creation of survival curves
  d.ipw.qsmk0 <- data.frame(cbind(seq(0, 119),0,(seq(0, 119))^2))
  d.ipw.qsmk1 <- data.frame(cbind(seq(0, 119),1,(seq(0, 119))^2))
  
  colnames(d.ipw.qsmk0) <- c("time", "qsmk", "timesq")
  colnames(d.ipw.qsmk1) <- c("time", "qsmk", "timesq")
  
  # assignment of estimated (1-hazard) to each person-month */
  d.ipw.qsmk0$p.noevent0 <- predict(ipw.model, d.ipw.qsmk0, type="response")
  d.ipw.qsmk1$p.noevent1 <- predict(ipw.model, d.ipw.qsmk1, type="response")
  
  # computation of survival for each person-month
  d.ipw.qsmk0$surv0 <- cumprod(d.ipw.qsmk0$p.noevent0)
  d.ipw.qsmk1$surv1 <- cumprod(d.ipw.qsmk1$p.noevent1)  
  
  # some data management to plot estimated survival curves
  d.ipw.graph <- merge(d.ipw.qsmk0, d.ipw.qsmk1, by=c("time", "timesq"))
  d.ipw.graph$survdiff <- d.ipw.graph$surv1-d.ipw.graph$surv0
  d.ipw.graph <- d.ipw.graph[order(d.ipw.graph$time),]
  return(d.ipw.graph$survdiff) 
}

survival.ipw.results <- boot(data=nhefs.surv, statistic=survival.ipw.boot, R=100)

survival.ipw.boot.results <- data.frame(cbind(original=survival.ipw.results$t0, 
                                              t(survival.ipw.results$t)))

ipw.boot.graph <- data.frame(cbind(time=seq(0,119), mean=survival.ipw.boot.results$original),
                             ll=(apply((survival.ipw.boot.results)[,-1], 1, quantile, probs=0.025)),
                             ul=(apply((survival.ipw.boot.results)[,-1], 1, quantile, probs=0.975)))

# plot
#install.packages("ggplot2")
library("ggplot2")
ggplot(ipw.boot.graph, aes(x=time, y=surv)) + 
  geom_line(aes(y = mean, colour = "A=1 - A=0")) + 
  geom_line(aes(y = ll, colour = "2.5 percentile")) + 
  geom_line(aes(y = ul, color = "97.5 percentile")) +
  xlab("Months") + 
  scale_x_continuous(limits = c(0, 120), breaks=seq(0,120,12)) +
  scale_y_continuous(limits = c(-0.1, 0.1), breaks=seq(-0.1,0.1,0.05)) +
  ylab("Survival difference") + 
  ggtitle("Survival difference from IP weighted hazards model") + 
  labs(colour="A:") +
  theme_bw() + 
  theme(legend.position="bottom")


######################################################################
# Estimating of survival curves via g-formula
# bootstrapping for variance estimation
######################################################################

#install.packages("dplyr")
library("dplyr")

survival.std.boot <- function(data, indices) {
  d <- data[indices,]
  
  # expanding dataset
  d$count <- as.numeric(rownames(d))
  d.gf <- data.frame(count=rep(d$count, times=d$survtime))
  d.gf <- merge(d.gf, d, by="count")  
  d.gf$time <- sequence(rle(d.gf$count)$lengths)-1
  d.gf$event <- ifelse(d.gf$time==d.gf$survtime-1 & 
                          d.gf$death==1, 1, 0)
  d.gf$timesq <- d.gf$time^2  
  
  # fit of hazards model with covariates
  gf.model <- glm(event==0 ~ qsmk + I(qsmk*time) + I(qsmk*timesq)
                  + time + timesq + sex + race + age + I(age*age)
                  + as.factor(education) + smokeintensity 
                  + I(smokeintensity*smokeintensity) + smkintensity82_71 
                  + smokeyrs + I(smokeyrs*smokeyrs) + as.factor(exercise) 
                  + as.factor(active) + wt71 + I(wt71*wt71), 
                  data=d.gf, family=binomial())
  
  # creation of dataset with all time points for each individual under 
  # each treatment level
  d.gf.pred <- data.frame(count=rep(d$count, times=120))
  d.gf.pred <- merge(d.gf.pred, d, by="count")    
  d.gf.pred$time <- rep(seq(0, 119), nrow(d))
  d.gf.pred$timesq <- d.gf.pred$time^2
  
  gf.qsmk0 <- gf.qsmk1 <- d.gf.pred
  gf.qsmk0$qsmk <- 0
  gf.qsmk1$qsmk <- 1

  # assignment of estimated (1-hazard) to each person-month */
  gf.qsmk0$p.noevent0 <- predict(gf.model, gf.qsmk0, type="response")
  gf.qsmk1$p.noevent1 <- predict(gf.model, gf.qsmk1, type="response")  

  # computation of survival for each person-month
  gf.qsmk0.surv <- gf.qsmk0 %>% group_by(count) %>% mutate(surv0 = cumprod(p.noevent0))
  gf.qsmk1.surv <- gf.qsmk1 %>% group_by(count) %>% mutate(surv1 = cumprod(p.noevent1))
  
  gf.surv0 <- aggregate(gf.qsmk0.surv, by=list(gf.qsmk0.surv$time), FUN=mean)[c("qsmk", "time", "surv0")]
  gf.surv1 <- aggregate(gf.qsmk1.surv, by=list(gf.qsmk1.surv$time), FUN=mean)[c("qsmk", "time", "surv1")]
  
  # some data management to plot estimated survival curves
  gf.graph <- merge(gf.surv0, gf.surv1, by=c("time"))
  gf.graph$survdiff <- gf.graph$surv1-gf.graph$surv0
  gf.graph <- gf.graph[order(gf.graph$time),]
  return(gf.graph$survdiff) 
}

survival.std.results <- boot(data=nhefs.surv, statistic=survival.std.boot, R=5)

survival.std.boot.results <- data.frame(cbind(original=survival.std.results$t0, 
                                              t(survival.std.results$t)))

std.boot.graph <- data.frame(cbind(time=seq(0,119), mean=survival.std.boot.results$original),
                             ll=(apply((survival.std.boot.results)[,-1], 1, quantile, probs=0.025)),
                             ul=(apply((survival.std.boot.results)[,-1], 1, quantile, probs=0.975)))

# plot
#install.packages("ggplot2")
library("ggplot2")
ggplot(std.boot.graph, aes(x=time, y=surv)) + 
  geom_line(aes(y = mean, colour = "A=1 - A=0")) + 
  geom_line(aes(y = ll, colour = "2.5 percentile")) + 
  geom_line(aes(y = ul, color = "97.5 percentile")) +
  xlab("Months") + 
  scale_x_continuous(limits = c(0, 120), breaks=seq(0,120,12)) +
  scale_y_continuous(limits = c(-0.1, 0.1), breaks=seq(-0.1,0.1,0.05)) +
  ylab("Survival difference") + 
  ggtitle("Survival difference from g-formula") + 
  labs(colour="A:") +
  theme_bw() + 
  theme(legend.position="bottom")
