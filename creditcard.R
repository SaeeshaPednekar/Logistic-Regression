creditcard<-read.csv("F:/Excelr/Assignments/dataset/Logistic regression/creditcard.csv")
View(creditcard)
creditcard<-creditcard[-1]
str(creditcard)
sum(is.na(creditcard))
?labels
library(plyr)
creditcard$card<-as.numeric(revalue(creditcard$card,c("no"="0","yes"="1")))
creditcard$owner<-as.numeric(revalue(creditcard$owner,c("no"="0","yes"="1")))
creditcard$selfemp<-as.numeric(revalue(creditcard$selfemp,c("no"="0","yes"="1")))
creditcard<-creditcard
attach(creditcard)
str(creditcard)



#exploratory data analysis

boxplot(creditcard$reports)$out
boxplot(creditcard$age)$out
boxplot(creditcard$income)$out
boxplot(creditcard$share)$out
boxplot(creditcard$expenditure)$out
boxplot(creditcard$dependents)$out
boxplot(creditcard$months)$out
boxplot(creditcard$majorcards)$out
boxplot(creditcard$X)$out
boxplot(creditcard$owner)$out

logit<-glm(creditcard$card~.,data = creditcard,family = "binomial")
summary(logit)
logit$fitted.values



logit1<-glm(card~age+income+expenditure+owner+selfemp+dependents+months+active,data=creditcard,family = "binomial")
summary(logit1)

#prediction based on test data
pred <- predict(logit1,creditcard,type = "response")
pred
#confusion matrix based on test data
conf <- table(pred>0.5,creditcard$card)
conf
#accuracy
accu <- sum(diag(conf)/sum(conf))
accu


logit2<-glm(card~age+income+share+expenditure+owner+selfemp+dependents+months+majorcards+active,data=creditcard,family = "binomial")
summary(logit2)
pred2 <- predict(logit2,creditcard,type = "response")
pred2
#confusion matrix based on test data
conf2 <- table(pred2>0.5,creditcard$card)
conf2
#accuracy
accu2 <- sum(diag(conf2)/sum(conf2))
accu2

#logit1 model
pred_values <- NULL
yes_no <- NULL
pred_values <- ifelse(pred>=0.5,1,0)
yes_no <- ifelse(pred>=0.5,"yes","no")



# Creating new column to store the above values
creditcard[,"prob"] <- pred
creditcard[,"pred_values"] <- pred_values
creditcard[,"yes_no"] <- yes_no

View(creditcard[,c(1,13:15)])
table(creditcard$card,creditcard$pred_values)

library(ROCR)
rocrpred<-prediction(pred,creditcard$card)
rocrperf<-performance(rocrpred,'tpr','fpr')
?ROCR::performance()
str(rocrperf)

plot(rocrperf,colorize=T)
## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)
summary(rocr_cutoff$cut_off)
