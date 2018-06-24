
#A population of women who were at least 21 years old, of Pima Indian heritage 
#and living near Phoenix, Arizona, was tested for diabetes according to 
#World Health Organization criteria. The data was collected by the US National Institute 
#of Diabetes and Digestive and Kidney Diseases.

#The goal is develop a logistic regression model to predict  diabetes.
################################################################################################3


#** Install all Packages if packages are not already installed **
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("corrplot")
#install.packages("stringr")
#install.packages("ade4")
#install.packages("MASS")
#install.packages("car")
#install.packages("e1071')
#install.packages("caret")
#install.packages("cowplot")
#install.packages("caTools")
#install.packages("ROCR")
#install.packages("lift")
#install.packages("stringr")
#install.packages("plyr")
#install.packages("AUC")

#loading all required libraries
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(ROCR)
library(ade4)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(stringr)
library(MASS)
library(car)
library(lift)
library(stringr)
library(plyr)
library(AUC)

## define working directory using the command setwd to the directory where your input files are located
##** Clear environment variables **
rm(list=ls())
# Loading data file

pima_indian_diabetes<-read.csv("pima_indian_diabetes.csv")

summary(pima_indian_diabetes)

# scaling of continuous variables

pima_indian_diabetes$No_Times_Pregnant<- scale(pima_indian_diabetes$No_Times_Pregnant)
pima_indian_diabetes$Plasma_Glucose<- scale(pima_indian_diabetes$Plasma_Glucose)
pima_indian_diabetes$Diastolic_BP<- scale(pima_indian_diabetes$Diastolic_BP)
pima_indian_diabetes$Triceps<- scale(pima_indian_diabetes$Triceps)
pima_indian_diabetes$Insulin<- scale(pima_indian_diabetes$Insulin)
pima_indian_diabetes$BMI<- scale(pima_indian_diabetes$BMI)
pima_indian_diabetes$Age<- scale(pima_indian_diabetes$Age)

# Splitting data into training and testing data sets

set.seed(100)

indices <- sample.split(pima_indian_diabetes$Diabetes, SplitRatio = 0.7)

train <- pima_indian_diabetes[indices,]

test <- pima_indian_diabetes[!(indices),]

# Creating model using training data

model_1 = glm(Diabetes ~ ., data = train, family = "binomial")

summary(model_1)

# Variable selection using stepwise AIC algorithm

library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2)

# Checking VIF

vif(model_2)

# No variable with VIF greater than 2. Also, all variables have low p value. Hence, there is no need for further variable selection
test_pred = predict(model_1, type = "response",newdata = test[,-8])

summary(test_pred)

#MODEL EVALUATION

pred<-prediction(test_pred,test$Diabetes)
eva<-performance(pred,"sens","spec")
evaA<-performance(pred,'acc')

plot(evaA)
sensitivity <- eva@y.values[[1]]
cutoff <- eva@alpha.values[[1]]
specificity<- eva@x.values[[1]]
accuracy<-evaA@y.values[[1]]

#plot for acccuracy, sensitivity and specificity for various values of cutoffs

plot(cutoff,sensitivity,col="red")
lines(cutoff,specificity,col="green")
lines(cutoff,accuracy,col="blue")
abline(v =0.31)
legend("bottomright", legend=c("sensitivity","accuracy","specificity"),
       col=c("red","blue","green"), lty=1:2, cex=0.8)
#making a dataframe for different values of acccuracy, sensitivity and specificity for various values of cutoffs 
matrix<-data.frame(cbind(sensitivity,specificity,accuracy,cutoff))
matrix$cutoff<-round(matrix$cutoff,2)
#Lets find cofusion matrix for this model
test_cutoff <- factor(ifelse(test_pred >=0.31, "Yes", "No"))
test_actual <- factor(ifelse(test$Diabetes==1, "Yes", "No"))
conf_final <- confusionMatrix(test_cutoff , test_actual, positive = "Yes")
conf_final
#          Reference
#Prediction  No Yes
#        No  58  11
#       Yes  21  28
#Accuracy : 0.7288 
#Sensitivity : 0.7179          
#Specificity : 0.7342 

#KS STATISTICS FOR THE MODEL
perf<-performance(pred,'tpr','fpr')
ks<-max(perf@y.values[[1]]-perf@x.values[[1]])
#KS STATISTICS FOR THE MODEL IS 0.51

#gain and lift chart
gain<-performance(pred,'tpr','rpp')
deciles<-performance(pred,'rpp')
#ks chart
k_stat_matrix<-data.frame(10*(deciles@y.values[[1]]),10*(perf@y.values[[1]]-perf@x.values[[1]]))
colnames(k_stat_matrix)[1]<-"deciles"
colnames(k_stat_matrix)[2]<-"k_statistic"
k_stat_matrix$k_statistic<-round(k_stat_matrix$k_statistic,2)
k_stat_matrix[which(k_stat_matrix$k_statistic==4.2),]
plot(k_stat_matrix)
abline(h=5.1,v=3.5)
#ks statistic lies withinin first 4 deciles 
plot(gain)
plot(perf)
plotLift(test_pred,test$Diabetes)
plot(roc(test_pred,factor(test$Diabetes)))
auc(roc(test_pred,factor(test$Diabetes)))
# area under ROC CURVE is 0.83