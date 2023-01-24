rm(list = ls())
setwd("/Users/mmart/OneDrive/Desktop/buzzer_github")
xdata=read.csv(file="data_coordination.csv", header=T, sep=",", stringsAsFactors = T)
##take data only for variable delays
library(dplyr)
ydata=filter(xdata, condition != "a_nv") ##discard nv condition

xdata=ydata[!is.na(ydata$prop_gaze_before15), ] ##celan the NA
ydata=filter(xdata, dog_press == "y")##only trials in which dog pressess

hist(ydata$prop_gaze_before15) ##check the vs
range(ydata$prop_gaze_before15, na.rm=T) #0   1
#glmmTMB cannot handle 0's and 1's so the response has to be transformed

ydata$tr.prop_gaze_before=(ydata$prop_gaze_before15*(nrow(ydata)-1) + 0.5)/nrow(ydata)
hist(ydata$tr.prop_gaze_before)
range(ydata$tr.prop_gaze_before, na.rm=T) ##looks good

#Identify random slopes
source("diagnostic_fcns.r")##this function cleans NA, dummy code factors, and inlcudes only relevant variables
 ##in new dataset (xx.fe.re$data, function $summary counts different values by subject to know if we need random slopes)
xx.fe.re=fe.re.tab(fe.model="prop_gaze_before15 ~ condition*(condition_order + trial_2 + Success)",
                   re="(1|dog)", 
                   other.vars = "tr.prop_gaze_before", data=ydata)
nrow(ydata)
nrow(xx.fe.re$data) ##same data because I cleaned NV and NA before

xx.fe.re$summary ##which random slopes I need
test.data = xx.fe.re$data #new data for model, now we z transform and dummy code
test.data$z.trial=as.vector(scale(test.data$trial_2))
test.data$z.order=as.vector(scale(test.data$condition_order))
test.data$condition.partnerdelay=test.data$condition.partnerdelay-mean(test.data$condition.partnerdelay)
test.data$condition.pressingdelay=test.data$condition.pressingdelay-mean(test.data$condition.pressingdelay)
test.data$Success.y=test.data$Success.-mean(test.data$Success.)



#Full model
library(glmmTMB)
full = glmmTMB(tr.prop_gaze_before ~ condition*(z.order + z.trial + Success)+
                 (1 + (condition.paressingdelay+condition.partnerdelay)*(z.trial + Success.y) + z.order|dog),
               family = beta_family(link="logit"), data = test.data)
full$sdr$pdHess #FALSE -> model didn't converge
summary(full)$varcor
##try without correlation between random slopes and random intercept)

fulls = glmmTMB(tr.prop_gaze_before ~ condition*(z.order + z.trial + Success)+
                  (1 + (condition.paressingdelay+condition.partnerdelay)*(z.trial + Success.y) + z.order||dog),
                family = beta_family(link="logit"), data = test.data)
fulls$sdr$pdHess #converged

null= glmmTMB(tr.prop_gaze_before ~ condition*(z.order + z.trial)+
                (1 + (condition.paressingdelay+condition.partnerdelay)*(z.trial + Success.y) + z.order||dog),
              family = beta_family(link="logit"), data = test.data)

as.data.frame(anova(null,fulls, method="Chisq"))##full-null model comparison
overdisp.test(fulls)##model is not overdisperse

tests.fulls=as.data.frame(drop1(fulls, test = "Chisq"))
tests.fulls ##we can reduce the model
##here I remove the non significant interactions
fullred = glmmTMB(tr.prop_gaze_before ~ condition*( z.trial + Success)+ z.order+
                    (1 + (condition.paressingdelay+condition.partnerdelay)*(z.trial + Success.y) + z.order||dog),
                  family = beta_family(link="logit"), data = test.data)
fullred$sdr$pdHess #converged

tests.red=as.data.frame(drop1(fullred, test = "Chisq"))
tests.red

fullred1 = glmmTMB(tr.prop_gaze_before ~ condition*(Success)+ z.order+ z.trial +
                     (1 + (condition.paressingdelay+condition.partnerdelay)*(z.trial + Success.y) + z.order||dog),
                   family = beta_family(link="logit"), data = test.data)
fullred1$sdr$pdHess #converged

tests.red1=as.data.frame(drop1(fullred1, test = "Chisq"))
tests.red1


fullred2 = glmmTMB(tr.prop_gaze_before ~ condition + Success + z.order+ z.trial +
                     (1 + (condition.paressingdelay+condition.partnerdelay)*(z.trial + Success.y) + z.order||dog),
                   family = beta_family(link="logit"), data = test.data)
fullred2$sdr$pdHess #converged

tests.red2=as.data.frame(drop1(fullred2, test = "Chisq"))
tests.red2

##tests.red2 is the model
##emmeans to see where the difference is
library(emmeans)
emmeans(fullred2, "condition")

##_____________assumptions
#Check for overdispersion
overdisp.test(fullred2)

#The model is not overdispersed

#Check for collinearity
xx=lm(tr.prop_gaze_before ~ condition + Success + z.order+ z.trial, data = test.data)
library(car)
vif(xx) #No collinearity issues


source("glmmTMB_stability.r")
full.stab=glmmTMB.stab(model.res=fullred2, para=T, data=test.data)
round(full.stab$summary[, -1], 3)
m.stab.plot(full.stab$summary[, -1])

##everything super stable

##ci
source("boot_glmmTMB.r")
full.boot=boot.glmmTMB(model.res=fullred2, data=test.data,
                       excl.non.conv=F, nboots=1000, para=T, resol=100,
                       level=0.95, use=NULL, contr=NULL, n.cores=c("all-1", "all"))
##################now let´s plot

library(dplyr)
df<- group_by(test.data, condition, Success, dog)
df<-summarise(df, sd=sd(tr.prop_gaze_before, na.rm=TRUE), avg=mean(tr.prop_gaze_before, na.rm=TRUE))
df$Condition=recode(df$condition, buzzerdelay="Delayed-button", partnerdelay="Delayed-partner", pressingdelay="Delayed-action",
                    .default=levels(df))
df$Success=recode(df$Success, y="Yes", n="No",
                  .default=levels(df))

library(ggplot2)
library(ggpubr)
plot1= ggplot(df, aes(x=Condition, y=avg, fill=Success, shape=Success)) +
  geom_boxplot(size=1, outlier.shape = NA) +
  geom_point(size=2, position = position_jitterdodge(), alpha=0.5)+
  theme_pubr() +
  theme(legend.position="right", axis.text=element_text(size=12), axis.title=element_text(size=12),
        plot.title=element_text(size=15, hjust=0.5)) +
  stat_summary(show.legend = FALSE, fun=mean, geom="point", shape=20, size=6, color="red", position=position_dodge(width=0.75))+
  xlab("") +
  scale_fill_manual(values= c("white", "grey"))+
  scale_color_manual(values= c("white", "grey"))+
  scale_shape_manual(values=c(15, 17))+
  ylab("Proportion of time\nwith head oriented towards human") 
#+ labs(color='Is the partner familiar?') 
plot1

##DESCRIPTIVE DATA for pressing yes/no
test.data$bin_gaze_before = ifelse(test.data$prop_gaze_before15 > 0, 1, 0)
df<- group_by(test.data, condition, Success, dog)
df<-summarise(df, sd=sd(bin_gaze_before, na.rm=TRUE), avg=mean(bin_gaze_before, na.rm=TRUE))
df2<- group_by(df, condition, Success)
df2<-summarise(df2, sd=sd(avg, na.rm=TRUE), avg=mean(avg, na.rm=TRUE))
View(df2)

##descriptives for proportion

df<- group_by(test.data, condition, Success, dog)
df<-summarise(df, sd=sd(prop_gaze_before15, na.rm=TRUE), avg=mean(prop_gaze_before15, na.rm=TRUE))
df2<- group_by(df, condition, Success)
df2<-summarise(df2, sd=sd(avg, na.rm=TRUE), avg=mean(avg, na.rm=TRUE))
View(df2)

##now let´s do something with pressing=NO
rm(list = ls())
setwd("/Users/mmart/OneDrive/Desktop/buzzer_github")
xdata=read.csv(file="data_coordination.csv", header=T, sep=",", stringsAsFactors = T)
##take data only for variable delays
library(dplyr)
ydata=filter(xdata, condition != "a_nv") ##discard nv condition

xdata=ydata[!is.na(ydata$prop_gaze_before15), ] ##celan the NA
ydata=filter(xdata, dog_press == "n")

hist(ydata$dog_see)

##descriptive data for the dog sees when human press yes/no
library(dplyr)

xdata=ydata[!is.na(ydata$dog_see), ]
ydata=filter(xdata, errortype == "nopress")
##what i am going to do is to recode and calculate total cases
ydata$dog_see2=as.factor(ydata$dog_see)
df=ydata %>% count(condition, dog_see2, sort = TRUE)

##we are going to start with looking before
ydata$dum.dogsee = ifelse(ydata$dog_see == 1, 0, 1)
df<- group_by(ydata, condition, dog)
df<-summarise(df, sd=sd(dum.dogsee, na.rm=TRUE), avg=mean(dum.dogsee, na.rm=TRUE))
df<- group_by(df, condition)
df<-summarise(df, sd=sd(avg, na.rm=TRUE), avg=mean(avg, na.rm=TRUE))

