---
title: 'Prediction Assignment Writeup'
author: "Fredy Gomez"
date: "Agust 03, 2020"
output: html_document
---

```{r setup, include=TRUE}
knitr::opts_chunk$set(echo = TRUE)
```

# Prediction-Assignment Writeup


# Packages
  install.packages("caret")
  install.packages("randomForest")
  install.packages("rpart")

  library("caret")
  library("randomForest")
  library("rpart")

#Set seed 
  
  set.seed(20000)


# Data
  trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"   
  testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv" 


# Cleaning
  training <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))  
  testing <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))

#Delete variables with near zero variance
  training<-training[,colSums(is.na(training)) == 0]
  testing <-testing[,colSums(is.na(testing)) == 0]

#Delete columns that are not predictors, after of review
  training   <-training[,-c(1:7)]
  testing <-testing[,-c(1:7)]

#Data after Cleaning
  dim(training)
## [1] 19622    53


# Crossvalidation


#In order to get out-of-sample errors, split training data in (75%) and testing (25%) data) subsets:
  Train <- createDataPartition(y=training$classe, p=0.75, list=FALSE)    
  NTraining <- training[Train, ]
  NTesting <- training[-Train, ]  
  dim(NTraining)

## [1] 14718    53
  dim(NTesting)
## [1] 4904   53


# Models
#Decision Tree
#Fit model on data
  fitD <- rpart(classe ~ ., data=NTraining, method="class")

#Model to predict class in validation set
  predictionDT <- predict(fitD, NTesting, type = "class")

  install.packages("e1071")
  library(e1071)

#Estimate the errors of the prediction algorithm in the Decision Tree model
  confusionMatrix(NTesting$classe, predictionDT)

  
  Confusion Matrix and Statistics
 
          Reference
  Prediction    A    B    C    D    E
  A 1226   42   38   59   30
  B  177  561   84   77   50
  C   14   79  686   48   28
  D   78   36  116  523   51
  E   36   59  115   42  649
  
  Overall Statistics
  
  Accuracy : 0.7433          
  95% CI : (0.7308, 0.7555)
  No Information Rate : 0.3122          
  P-Value [Acc > NIR] : < 2.2e-16       
  
  Kappa : 0.6743          
  
  Mcnemar's Test P-Value : < 2.2e-16       

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E
Sensitivity            0.8008   0.7220   0.6603   0.6983   0.8032
Specificity            0.9499   0.9060   0.9563   0.9324   0.9385
Pos Pred Value         0.8789   0.5911   0.8023   0.6505   0.7203
Neg Pred Value         0.9131   0.9454   0.9128   0.9449   0.9603
Prevalence             0.3122   0.1584   0.2119   0.1527   0.1648
Detection Rate         0.2500   0.1144   0.1399   0.1066   0.1323
Detection Prevalence   0.2845   0.1935   0.1743   0.1639   0.1837
Balanced Accuracy      0.8753   0.8140   0.8083   0.8153   0.8708

  #RANDOM FOREST

#Fit model on NTraining data
  fitRF <- randomForest(classe ~ ., data=NTraining, method="class")

#Model to predict class in validation set
  predictionR <- predict(fitRF, NTesting, type = "class")

#Estimate the errors of the prediction algorithm in the Random Forest
  confusionMatrix(NTesting$classe, predictionR)

## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1395    0    0    0    0
##          B    3  944    2    0    0
##          C    0    8  847    0    0
##          D    0    0    6  797    1
##          E    0    0    0    1  900
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9957          
##                  95% CI : (0.9935, 0.9973)
##     No Information Rate : 0.2851          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9946          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9979   0.9916   0.9906   0.9987   0.9989
## Specificity            1.0000   0.9987   0.9980   0.9983   0.9998
## Pos Pred Value         1.0000   0.9947   0.9906   0.9913   0.9989
## Neg Pred Value         0.9991   0.9980   0.9980   0.9998   0.9998
## Prevalence             0.2851   0.1941   0.1743   0.1627   0.1837
## Detection Rate         0.2845   0.1925   0.1727   0.1625   0.1835
## Detection Prevalence   0.2845   0.1935   0.1743   0.1639   0.1837
## Balanced Accuracy      0.9989   0.9952   0.9943   0.9985   0.9993


#Test the model to predict 20 different test cases

# Perform prediction
  predictSmission <- predict(fitRF, testing, type="class")
  predictSmission
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E