rm(list = ls())
setwd("/Users/mmart/OneDrive/Desktop/buzzer_github")
xdata=read.csv(file="data_ic.csv", header=T, sep=",", stringsAsFactors = T)

source("diagnostic_fcns.R")
xx.fe.re=fe.re.tab(fe.model= "ic_errors ~ condition * delay * (dias_total + box_errors) + age", 
                   re="(1|dog)", data=xdata)

test.data=xx.fe.re$data
xx.fe.re$summary[1:30]

resp.mat=cbind(ic_errors=xdata$ic_errors,
               other_errors=xdata$other_errors)

test.data$z.delay=as.vector(scale(test.data$delay))
test.data$z.dias=as.vector(scale(test.data$dias_total))
test.data$z.box=as.vector(scale(test.data$box_errors))
test.data$z.age=as.vector(scale(test.data$age))
test.data$condition.partnerdelay=test.data$condition.partnerdelay-mean(test.data$condition.partnerdelay)
test.data$condition.pressingdelay=test.data$condition.pressingdelay-mean(test.data$condition.pressingdelay)


##we first fit a poisson
library(lme4)
contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000))
full.p=glmer(ic_errors ~ condition * z.delay * (z.dias + z.box) + z.age +
                    (1+ (condition.partnerdelay + condition.pressingdelay) * z.delay|dog),
                  data=test.data, family=poisson, control=contr)

full.p.s=glmer(ic_errors ~ condition * z.delay * (z.dias + z.box) + z.age +
               (1+ (condition.partnerdelay + condition.pressingdelay) * z.delay||dog),
             data=test.data, family=poisson, control=contr)

full.b=glmer(resp.mat ~ condition * z.delay * (z.dias + z.box) + z.age + 
               (1 + (condition.partnerdelay + condition.pressingdelay) * z.delay |dog),
             data=test.data, family=binomial, control=contr)
full.b.s=glmer(resp.mat ~ condition * z.delay * (z.dias + z.box) + z.age + 
               (1 + (condition.partnerdelay + condition.pressingdelay) * z.delay ||dog),
             data=test.data, family=binomial, control=contr)
##check dispersion
source("diagnostic_fcns.r")
overdisp.test(full.p)
overdisp.test(full.p.s)
overdisp.test(full.b)
overdisp.test(full.b.s)
##none of them is overdispersed, if something, binomial is underdispersed (conservative)
##chekc aic to chose one model
AIC(full.p)
AIC(full.p.s)
AIC(full.b)
AIC(full.b.s)##binomial is better, specifically this one (but AIC penalise df, so let´s check a loglik too)
logLik(full.b)
logLik(full.b.s)
##it is not a big difference. are the corr identifiable?
summary(full.b)$varcor ##yes, they are. I am choosing to keep full.b then

null.b=glmer(resp.mat ~ condition * z.delay + z.age + 
                 (1 + (condition.partnerdelay + condition.pressingdelay) * z.delay |dog),
               data=test.data, family=binomial, control=contr)
as.data.frame(anova(null.b,full.b, method="Chisq"))
##is not significant. we reduce the model
tests.full=as.data.frame(drop1(full.b, test = "Chisq"))
tests.full

full.b1=glmer(resp.mat ~ condition:z.delay:z.dias + (condition +z.box +z.delay)^2+ z.age + 
               (1 + (condition.partnerdelay + condition.pressingdelay) * z.delay |dog),
             data=test.data, family=binomial, control=contr)
tests.full1=as.data.frame(drop1(full.b1, test = "Chisq"))
tests.full1
          
full.b2=glmer(resp.mat ~ (condition +z.box +z.delay+z.dias)^2+ z.age + 
                (1 + (condition.partnerdelay + condition.pressingdelay) * z.delay |dog),
              data=test.data, family=binomial, control=contr)
tests.full2=as.data.frame(drop1(full.b2, test = "Chisq"))
tests.full2

full.b3=glmer(resp.mat ~ condition* (z.box + z.dias) + (z.box +z.delay +z.dias)^2 + z.age +
                (1 + (condition.partnerdelay + condition.pressingdelay) * z.delay |dog),
              data=test.data, family=binomial, control=contr)
tests.full3=as.data.frame(drop1(full.b3, test = "Chisq"))
tests.full3

full.b4=glmer(resp.mat ~ condition*z.box + (z.box +z.delay +z.dias)^2 + z.age +
                (1 + (condition.partnerdelay + condition.pressingdelay) * z.delay |dog),
              data=test.data, family=binomial, control=contr)
tests.full4=as.data.frame(drop1(full.b4, test = "Chisq"))
tests.full4

full.b5=glmer(resp.mat ~ z.age + z.box * (condition + z.delay) + z.delay*z.dias +
                (1 + (condition.partnerdelay + condition.pressingdelay) * z.delay |dog),
              data=test.data, family=binomial, control=contr)
tests.full5=as.data.frame(drop1(full.b5, test = "Chisq"))
tests.full5

full.b6=glmer(resp.mat ~ z.age + z.box * (condition + z.delay) + z.dias +
                (1 + (condition.partnerdelay + condition.pressingdelay) * z.delay |dog),
              data=test.data, family=binomial, control=contr)
tests.full6=as.data.frame(drop1(full.b6, test = "Chisq"))
tests.full6

full.b7=glmer(resp.mat ~ z.age + z.box * z.delay + condition + z.dias +
                (1 + (condition.partnerdelay + condition.pressingdelay) * z.delay |dog),
              data=test.data, family=binomial, control=contr)
tests.full7=as.data.frame(drop1(full.b7, test = "Chisq"))
tests.full7

##this model is our model. now p values. use drop1 because afex/mixed cannot work with resp.mat

full7pvalue=drop1(full.b7, .~., test="Chisq")

##_____________assumptions
#Check for overdispersion
overdisp.test(full.b7)##little underdispersed but ok


#Check for collinearity
xx=lm(ic_errors ~ z.age + z.box + z.delay + condition + z.dias, data = test.data)
library(car)
vif(xx) #No collinearity issues

source("glmm_stability.r")
m.stab=glmm.model.stab(model.res=full.b7, contr=contr)
m.stab.plot(m.stab$summary[, -1])

##confidence intervals
source("boot_glmm.r")
boot.res=boot.glmm.pred(model.res=full.b7, excl.warnings=T,
                        nboots=1000, para=T)
##blups
ranef.diagn.plot(full.b7) #fair

