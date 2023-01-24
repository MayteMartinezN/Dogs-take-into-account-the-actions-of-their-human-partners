rm(list = ls())
setwd("/Users/mmart/OneDrive/Desktop/buzzer_github")

ydata=read.csv(file="data_coordination.csv", header=T, sep=",", stringsAsFactors = T)
##take data only for variable delays
xdata = ydata [ which(ydata$session>0), ]

##check which random slopes we need
source("diagnostic_fcns.r")

xx.fe.re=fe.re.tab(fe.model= "Success ~ condition * condition_order * trial * delay + age *
                   condition * (delay + trial)", re="(1|dog)", data=xdata)

xx.fe.re$summary[1:34]

#dummy coding, centering and z transform

t.data=xx.fe.re$data
t.data$condition.buzzerdelay=t.data$condition.buzzerdelay-mean(t.data$condition.buzzerdelay)
t.data$condition.partnerdelay=t.data$condition.partnerdelay-mean(t.data$condition.partnerdelay)
t.data$condition.pressingdelay=t.data$condition.pressingdelay-mean(t.data$condition.pressingdelay)
t.data$z.condition_order=as.vector(scale(t.data$condition_order))
t.data$z.trial=as.vector(scale(t.data$trial))
t.data$z.delay=as.vector(scale(t.data$delay))
t.data$z.age=as.vector(scale(t.data$age))

##fitting the first full model

library(lme4)
contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000))
full=glmer(Success ~ condition * z.condition_order * z.trial * z.delay +
             z.age * condition * (z.delay + z.trial) + (1 + 
            (condition.buzzerdelay + condition.partnerdelay + condition.pressingdelay + z.condition_order) *
              z.delay * z.trial |dog), data=t.data, family=binomial, control=contr)
full.s=glmer(Success ~ condition * z.condition_order * z.trial * z.delay +
             z.age * condition * (z.delay + z.trial) + (1 + 
                                                          (condition.buzzerdelay + condition.partnerdelay + condition.pressingdelay + z.condition_order) *
                                                        z.delay * z.trial ||dog), data=t.data, family=binomial, control=contr)
#compare loglik and df
logLik(full.s)
logLik(full)

##we keep the simplest one
null.s=glmer(Success ~  z.condition_order * z.trial * z.delay +
               z.age  * (z.delay + z.trial) + (1 + 
                                                            (condition.buzzerdelay + condition.partnerdelay + condition.pressingdelay + z.condition_order) *
                                                            z.delay * z.trial ||dog), data=t.data, family=binomial, control=contr)
as.data.frame(anova(null.s, full.s, test="Chisq"))
##test the five-way interaction:
tests.full.s=as.data.frame(drop1(full.s, test="Chisq"))
tests.full.s

##we start simplifying the model by removing the non-significant higher order interactions
full.s1=glmer(Success ~ (condition + z.condition_order + z.trial + z.delay)^3 +
               z.age * condition * (z.delay + z.trial) + 
                (1 + (condition.buzzerdelay + condition.partnerdelay + condition.pressingdelay + z.condition_order) *
                z.delay * z.trial ||dog), data=t.data, family=binomial, control=contr)

tests.full.s1=as.data.frame(drop1(full.s1, test="Chisq"))
tests.full.s1

full.s2=glmer(Success ~ (condition + z.condition_order + z.trial + z.delay)^3 +
                z.age * condition * z.trial + z.age * z.delay +
                (1 + (condition.buzzerdelay + condition.partnerdelay + condition.pressingdelay + z.condition_order) *
                   z.delay * z.trial ||dog), data=t.data, family=binomial, control=contr)

tests.full.s2=as.data.frame(drop1(full.s2, test="Chisq"))
tests.full.s2

full.s3=glmer(Success ~ condition * z.condition_order * (z.trial + z.delay) +
                z.condition_order*z.trial*z.delay+
                (condition + z.condition_order + z.trial + z.delay)^3 +
                z.age * condition * z.trial + z.age * z.delay +
                (1 + (condition.buzzerdelay + condition.partnerdelay + condition.pressingdelay + z.condition_order) *
                   z.delay * z.trial ||dog), data=t.data, family=binomial, control=contr)
tests.full.s3=as.data.frame(drop1(full.s3, test="Chisq"))
tests.full.s3

full.s4=glmer(Success ~ condition * z.condition_order * (z.trial + z.delay) +
                z.age * condition * z.trial + z.age * z.delay +
                (1 + (condition.buzzerdelay + condition.partnerdelay + condition.pressingdelay + z.condition_order) *
                   z.delay * z.trial ||dog), data=t.data, family=binomial, control=contr)
tests.full.s4=as.data.frame(drop1(full.s4, test="Chisq"))
tests.full.s4

full.s5=glmer(Success ~ (condition + z.condition_order+z.trial+z.delay)^2 + 
                condition:z.condition_order:z.trial+
                z.age * condition * z.trial + z.age * z.delay +
                (1 + (condition.buzzerdelay + condition.partnerdelay + condition.pressingdelay + z.condition_order) *
                   z.delay * z.trial ||dog), data=t.data, family=binomial, control=contr)
tests.full.s5=as.data.frame(drop1(full.s5, test="Chisq"))
tests.full.s5


full.s5=glmer(Success ~ (condition + z.condition_order+z.trial+z.delay)^2 + 
                condition:z.condition_order:z.trial+
                z.age * (condition + z.trial+z.delay) +
                (1 + (condition.buzzerdelay + condition.partnerdelay + condition.pressingdelay + z.condition_order) *
                   z.delay * z.trial ||dog), data=t.data, family=binomial, control=contr)
tests.full.s5=as.data.frame(drop1(full.s5, test="Chisq"))
tests.full.s5

full.s6=glmer(Success ~ condition * (z.condition_order + z.trial + z.delay+z.age)+
                condition:z.condition_order:z.trial+ z.condition_order*z.delay+
                z.age * (z.trial + z.delay)+
                (1 + (condition.buzzerdelay + condition.partnerdelay + condition.pressingdelay + z.condition_order) *
                   z.delay * z.trial ||dog), data=t.data, family=binomial, control=contr)
tests.full.s6=as.data.frame(drop1(full.s6, test="Chisq"))
tests.full.s6

full.s6=glmer(Success ~ condition * (z.condition_order + z.trial + z.delay+z.age)+
                condition:z.condition_order:z.trial+ z.condition_order*z.delay+
                z.age * (z.trial + z.delay)+
                (1 + (condition.buzzerdelay + condition.partnerdelay + condition.pressingdelay + z.condition_order) *
                   z.delay * z.trial ||dog), data=t.data, family=binomial, control=contr)
tests.full.s6=as.data.frame(drop1(full.s6, test="Chisq"))
tests.full.s6


full.s7=glmer(Success ~ condition * (z.condition_order + z.trial + z.delay+z.age)+
                condition:z.condition_order:z.trial+ z.condition_order*z.delay+
                z.age * (z.delay)+
                (1 + (condition.buzzerdelay + condition.partnerdelay + condition.pressingdelay + z.condition_order) *
                   z.delay * z.trial ||dog), data=t.data, family=binomial, control=contr)
tests.full.s7=as.data.frame(drop1(full.s7, test="Chisq"))
tests.full.s7

full.s8=glmer(Success ~ condition * (z.condition_order + z.trial + z.delay+z.age)+
                z.condition_order*z.delay+
                z.age * (z.delay)+
                (1 + (condition.buzzerdelay + condition.partnerdelay + condition.pressingdelay + z.condition_order) *
                   z.delay * z.trial ||dog), data=t.data, family=binomial, control=contr)
tests.full.s8=as.data.frame(drop1(full.s8, test="Chisq"))
tests.full.s8

full.s9=glmer(Success ~ condition * (z.condition_order + z.trial + z.delay+z.age)+
                z.age:z.delay+
                (1 + (condition.buzzerdelay + condition.partnerdelay + condition.pressingdelay + z.condition_order) *
                   z.delay * z.trial ||dog), data=t.data, family=binomial, control=contr)
tests.full.s9=as.data.frame(drop1(full.s9, test="Chisq"))
tests.full.s9

full.s10=glmer(Success ~ condition * (z.condition_order + z.trial+z.age)+
                z.age*z.delay+
                (1 + (condition.buzzerdelay + condition.partnerdelay + condition.pressingdelay + z.condition_order) *
                   z.delay * z.trial ||dog), data=t.data, family=binomial, control=contr)
tests.full.s10=as.data.frame(drop1(full.s10, test="Chisq"))
tests.full.s10

full.s11=glmer(Success ~ condition * (z.condition_order + z.trial)+
                 z.age*z.delay+
                 (1 + (condition.buzzerdelay + condition.partnerdelay + condition.pressingdelay + z.condition_order) *
                    z.delay * z.trial ||dog), data=t.data, family=binomial, control=contr)
tests.full.s11=as.data.frame(drop1(full.s11, test="Chisq"))
tests.full.s11

full.s12=glmer(Success ~ condition * (z.condition_order + z.trial)+
                 z.age + z.delay+
                 (1 + (condition.buzzerdelay + condition.partnerdelay + condition.pressingdelay + z.condition_order) *
                    z.delay * z.trial ||dog), data=t.data, family=binomial, control=contr)
tests.full.s12=as.data.frame(drop1(full.s12, test="Chisq"))
tests.full.s12

##so this is the final model, full.s12

##all the lrt and pvalue (can be done also with drop1, same result)
library(afex)
model_lrt=mixed(Success ~ condition * (z.condition_order + z.trial)+
                      z.age +
                      (1 + (condition.buzzerdelay + condition.partnerdelay + condition.pressingdelay + z.condition_order) *
                         z.delay * z.trial ||dog), data=t.data, family=binomial, control=contr, method="LRT")
  

source("diagnostic_fcns.r")
ranef.diagn.plot(full.s12) ##BLUPS
t.data$dum.success = ifelse(t.data$Success == 'y', 1, 0)
xx=lm(dum.success ~ condition + z.delay + z.condition_order + z.trial, data=t.data)
library(car)
vif(xx) ##collinearity

source("glmm_stability.r") ##model stability
m.stab=glmm.model.stab(model.res=full.s12, contr=contr)
m.stab.plot(m.stab$summary[, -1])

source("boot_glmm.r")##confidence intervals
boot.res=boot.glmm.pred(model.res=full.s12, excl.warnings=T,
                        nboots=1000, para=T)


______________________________