
library(caTools)
library(corpcor)
library(car)
library(psych)

library(dplyr)


bank<-read.csv(choose.files())
str(bank)
summary(bank)
sum(is.na(bank))
colnames(bank)
attach(bank)

hist(age)
boxplot(age)

hist(balance)
boxplot(balance)

hist(day)
boxplot(day)

hist(duration)
boxplot(duration)

hist(campaign)
boxplot(campaign)

hist(pdays)
boxplot(pdays)

hist(previous)
boxplot(previous)

#Splitting the data into train and test with 0.7 split ratio

split<- sample.split(y,SplitRatio = 0.7)
train_data<-subset(bank,split==TRUE)
test_data<-subset(bank,split==FALSE)



#model1 with all variables
model<-glm(train_data$y~.,train_data,family = "binomial")
summary(model)

#insignificant variables age, pdays, previous 
#removing insignificant variables from the train data and creating new data set train_data_1

train_data1<-train_data[,-c(1,5,15,14)]

model1<-glm(train_data1$y~.,train_data1,family = "binomial")
summary(model1)

test_data11<-test_data[,-c(1,5,15,14)]#significant test data
prob <- predict(model1,test_data11,type="response")
prob
summary(model)

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,test_data11$y)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 70.62





# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
test_data11[,"prob"] <- prob
test_data11[,"pred_values"] <- pred_values
test_data11[,"yes_no"] <- yes_no

View(test_data11[,c(13:16)])

table(test_data11$y,test_data11$pred_values)
# Calculate the below metrics
# precision | recall | True Positive Rate | False Positive Rate | Specificity | Sensitivity
# from the above table - 59


# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic
install.packages("ROCR")
library(ROCR)
rocrpred<-prediction(prob,test_data11$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
?ROCR::performance()
str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained

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
