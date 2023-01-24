##who presses first in the succesfull trials

rm(list = ls())
setwd("/Users/mmart/OneDrive/Desktop/buzzer_github")
xdata=read.csv(file="data_coordination.csv", header=T, sep=",", stringsAsFactors = T)
##take data only for variable delays
library(dplyr)
firstdata=filter(xdata, condition != "a_nv")
firstdata=filter(firstdata, Success == "y")
firstdata=filter(firstdata, press_first != " ")
firstdata=filter(firstdata, session > 0) ##only variable delays

source("diagnostic_fcns.r")
xx.fe.re=fe.re.tab(fe.model= "press_first ~ condition*delay*trial*condition_order+age", 
                   re="(1|dog)", data=firstdata)
##xx.fe.re$summary[1:30]
t.data=xx.fe.re$data

t.data$condition.pressingdelay=t.data$condition.pressingdelay-mean(t.data$condition.pressingdelay)
t.data$condition.partnerdelay=t.data$condition.partnerdelay-mean(t.data$condition.partnerdelay
)
t.data$z.condition_order=as.vector(scale(t.data$condition_order))
t.data$z.delay=as.vector(scale(t.data$delay))
t.data$z.trial=as.vector(scale(t.data$trial))
t.data$z.age=as.vector(scale(t.data$age))



###i am gonna do a model to see what affects whether they press first or not. I will keep it simple, just conition* delay + trial + condition order + age???
library(lme4)
contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000))

full_first=glmer (press_first ~ condition+z.trial+z.age +
                    (1+ condition.partnerdelay + condition.pressingdelay +z.trial|dog),
                  data=t.data, family=binomial, control=contr)

full_first_s=glmer (press_first ~ condition+z.trial+z.age +
                      (1+ condition.partnerdelay + condition.pressingdelay +z.trial||dog),
                    data=t.data, family=binomial, control=contr)
logLik(full_first)
logLik(full_first_s)

##I keep the correlations because they are identifiable and not so much diff in complexity

null_first=glmer (press_first ~ 1 +
                    (1+ condition.partnerdelay + condition.pressingdelay + z.trial|dog),
                  data=t.data, family=binomial, control=contr)

as.data.frame(anova(null_first,full_first, method="Chisq"))

##not significant

round((summary(full_first)$coefficients),3)

##pvalues
library(afex)
mixed_full_first=mixed(press_first ~ 1 + condition + z.trial + z.age +
                         (1+ condition.partnerdelay + condition.pressingdelay + z.trial|dog),
                       data=t.data, family=binomial, control=contr, method="LRT")

##assumptions of the model
ranef.diagn.plot(full_first)
##BLUPS are fine but little bit big for binomial (individual variation?)

##COLLINEARITY is fine, all around 1
t.data$dum_press_first = ifelse(t.data$press_first == 'dog', 1, 0)
xx=lm(dum_press_first ~ condition + z.trial + z.age, data=t.data)
library(car)
vif(xx)

##stability?
source("glmm_stability.r")
m.stab=glmm.model.stab(model.res=full_first, contr=contr)
m.stab.plot(m.stab$summary[, -1])

##model is stable

##confidence intervals
source("boot_glmm.r")
boot.res=boot.glmm.pred(model.res=full_first, excl.warnings=T,
                        nboots=1000, para=T)
##because blups are wide, check random effects

summary(full_first)$varcor #some of them big compared to estimates

##percentages and ranges
library(dplyr)
t.data$dumm.press_first=ifelse(t.data$press_first=="dog", 1, 0)
df<- group_by(t.data, condition, dog)
df<-summarise(df, sd=sd(dumm.press_first, na.rm=TRUE), avg=mean(dumm.press_first))
df2<- group_by(df, dog)
df2<-summarise(df2, sd=sd(avg, na.rm=TRUE), avg=mean(avg))

## binomial by dog to see who does what

xx=as.data.frame(table(t.data$dog))
trials=as.vector(xx$Freq)
xx=as.data.frame(table(t.data$dog, t.data$press_first))
library(dplyr)
xx=filter(xx, Var2 == "dog")
dog_first=as.vector(xx$Freq)
dog=as.vector(xx$Var1)
binomialdata=data.frame(dog,dog_first,trials)
binomialdata$prop=round((binomialdata$dog_first/binomialdata$trials),3)

##repeat this with each dog from binomialdata
a=binom.test(61, 117, p=0.5, alternative="two.sided", conf.level = 0.95 )
round(a$p.value,3)

