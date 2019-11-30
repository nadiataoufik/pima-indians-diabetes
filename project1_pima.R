#GOAL:predict wether a patient is diabetic or not (categorical)
Pima.te
# DATA ACQUISATION
Pima=read.csv("C:/Users/nadiataoufik/Documents/RPROGRAM/pima.csv")
library(caTools)
Pima$Outcome
sample.split(Pima$Outcome,SplitRatio = 0.8)->split
subset(Pima,split==T)->train
subset(Pima,split==F)->test
nrow(train)
nrow(test)
glm(Outcome~. ,data = train,family = "binomial")->model_log
summary(model_log)
#let s optimize the model using the null deviance and residual deviance and AIC (should as minimum as possible )

Pima$Insulin
glm(Outcome~.-SkinThickness ,data = train,family = "binomial")->model_log
summary(model_log)
# my AIC VALUE HAS BEEN reduced , so the skin thickness is not significant variable
glm(Outcome~.-SkinThickness ,data = train,family = "binomial")->model_log
summary(model_log)
# predict the value and check the accuracy of our model
predict(model_log,newdata = test,type = "response")->res

res
test
# let s calculate the cofusion matrix 
library(ROCR)
table(actual=test$Outcome,predicted=res>0.5)
(35+90)/(35+90+19+10)
# increase my accuracy by choosing the right threshhold  using ROCR  
# first step we will store our predicted values for the training in res 
predict(model_log,train,type = "response")->res
#import the library for the ROCR package
library(ROCR)
# define the ROCR PREDICTION AND performance 
prediction(res,train$Outcome)->ROCRPRED
performance(ROCRPRED,"tpr","fpr")->ROCRPERF
plot(ROCRPERF,colorize=TRUE,print.cutoffs.at=seq(0.1,by0.1))
plot(ROCRPERF, colorize=T ,print.cutoffs.at=seq(0.1,by=0.1))
table(actual=test$Outcome,predicted=res>0.3)
(76+42)/(76+24+12+42)

