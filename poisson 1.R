library(readxl)
library(MASS)
library(leaps)
library(tidyverse)
library(caret)
set.seed(123)
dat = read_xlsx("C:/Projects/FL/Creative/CLEAR III_Publication data_2018.1.30.xlsx",sheet = "Demographics and volumes")
dat$tobacco_use[is.na(dat$tobacco_use)] = 0
dat$cocaine_use[is.na(dat$cocaine_use)] = 0
dat$anticoagulated_at_registration[is.na(dat$anticoagulated_at_registration)] = 0
dat$on_hrt_at_registration[is.na(dat$on_hrt_at_registration)] = 0
dat$hispanic_latino[is.na(dat$hispanic_latino)] = 0
dat$on_antiplatelet_at_registration[is.na(dat$on_antiplatelet_at_registration)] = 0
dat$noncompliant_antihypertensive[is.na(dat$noncompliant_antihypertensive)] = 0
dat$noncompliant_hyperlipidemia[is.na(dat$noncompliant_hyperlipidemia)] = 0

#dat$randomization_systolic
#dat$dct_ivh_volume_rc
#dat$dct_ich_volume_rc

#sum(dat$tobacco_use)

dat = dat[,-(1:3)]

for(i in 1:47){
  print(sum(is.na(dat[,i])))
  print(i)
}

dat1 = dat[,-(17:31)]
dat1 = dat1[,-c(16,22)]


#dat1 = dat[,c(1:8)]
#dat1 = data.frame(dat1,dat$hemisphere,dat$randomization_systolic,dat$dct_ich_volume_rc,dat$dct_ivh_volume_rc,dat$total_doses)
#train.control <- trainControl(method = "cv", number = 10)
# Train the model
#step.model <- train(total_doses ~., data = dat1,
#                    method = "leapBackward", 
#                    tuneGrid = data.frame(nvmax = 1:5),
#                    trControl = train.control
#)
#step.model$results
#summary(step.model$finalModel)

full = glm(total_doses~., data = dat1, family = poisson(link = "log"))
stepAIC(full,direction = "both",trace = F)

mod1 = glm(total_doses~ treatment + time_last_to_first_dose + VP_shunt_reported, data = dat1, family = poisson(link = "log"))
summary(mod1)

sum(mod1$residuals^2)/sqrt(500)



tr = which(dat$treatment == 1)
tr2 = which(dat$treatment == 2)

silo1_1 = sample(tr,310,replace = F)
silo1_2 = sample(tr2,20,replace = F)

dat_1 = dat[c(silo1_1,silo1_2),]

mod1 = glm(TOTALNIH~., data = dat_1, family = poisson(link = "log"))
mod1$coefficients
summary(mod1)

sum(mod1$residuals^2)

silo2_1 = sample(tr,124,replace = F)
silo2_2 = sample(tr2,202,replace = F)

dat_2 = dat[c(silo2_1,silo2_2),]
mod2 = glm(TOTALNIH~., data = dat_2, family = poisson(link = "log"))
mod2$coefficients
summary(mod2)

sum(mod2$residuals^2)
