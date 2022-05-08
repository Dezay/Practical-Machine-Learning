---
title: "Final Project"
output:   
  html_document:
    keep_md: true
---



#I-) Essential Packages

```r
IscaretInstalled <- require("caret")
```

```
## Loading required package: caret
```

```
## Warning: package 'caret' was built under R version 4.1.3
```

```
## Loading required package: ggplot2
```

```
## Warning: package 'ggplot2' was built under R version 4.1.3
```

```
## Loading required package: lattice
```

```r
if(!IscaretInstalled){
    install.packages("caret")
    library("caret")
    }

IsrandomForestInstalled <- require("randomForest")
```

```
## Loading required package: randomForest
```

```
## Warning: package 'randomForest' was built under R version 4.1.3
```

```
## randomForest 4.7-1
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```
## 
## Attaching package: 'randomForest'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     margin
```

```r
if(!IsrandomForestInstalled){
    install.packages("randomForest")
    library("randomForest")
    }

IsRpartInstalled <- require("rpart")        
```

```
## Loading required package: rpart
```

```r
if(!IsRpartInstalled){
    install.packages("rpart")
    library("rpart")
    }

#set seed for reproducability
set.seed(20000)
```

#II-) Data Processing
Load the data

```r
trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"   
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv" 
```

#III-)Cleaning data 
Load data to memory.

```r
training <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))  
testing <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))

#Remove variables with near zero variance
training<-training[,colSums(is.na(training)) == 0]
testing <-testing[,colSums(is.na(testing)) == 0]

#Remove columns that are not predictors, which are the the seven first columns
training   <-training[,-c(1:7)]
testing <-testing[,-c(1:7)]

# The data after cleaning
dim(training)
```

```
## [1] 19622    53
```

```r
dim(testing)
```

```
## [1] 20 53
```
#IV-) Cross-validation
In order to get out-of-sample errors, split the training data in training (75%) and testing (25%) data) subsets:

```r
inTrain <- createDataPartition(y=training$classe, p=0.75, list=FALSE)    
NEOTraining <- training[inTrain, ]
NEOTesting <- training[-inTrain, ]  
dim(NEOTraining)
```

```
## [1] 14718    53
```

```r
dim(NEOTesting) 
```

```
## [1] 4904   53
```

#V-) Prediction Models

DECISION TREE
Fit model on NEOTraining data

```r
fitDT <- rpart(classe ~ ., data=NEOTraining, method="class")
```
Use model to predict class in validation set (NEOTesting)

```r
predictionDT <- predict(fitDT, NEOTesting, type = "class")
```
Estimate the errors of the prediction algorithm in the Decision Tree model

```r
NEOTesting$classe <- as.factor(NEOTesting$classe)
confusionMatrix(predictionDT,NEOTesting$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1226  177   14   78   36
##          B   42  561   79   36   59
##          C   38   84  686  116  115
##          D   59   77   48  523   42
##          E   30   50   28   51  649
## 
## Overall Statistics
##                                           
##                Accuracy : 0.7433          
##                  95% CI : (0.7308, 0.7555)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.6743          
##                                           
##  Mcnemar's Test P-Value : < 2.2e-16       
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.8789   0.5911   0.8023   0.6505   0.7203
## Specificity            0.9131   0.9454   0.9128   0.9449   0.9603
## Pos Pred Value         0.8008   0.7220   0.6603   0.6983   0.8032
## Neg Pred Value         0.9499   0.9060   0.9563   0.9324   0.9385
## Prevalence             0.2845   0.1935   0.1743   0.1639   0.1837
## Detection Rate         0.2500   0.1144   0.1399   0.1066   0.1323
## Detection Prevalence   0.3122   0.1584   0.2119   0.1527   0.1648
## Balanced Accuracy      0.8960   0.7683   0.8576   0.7977   0.8403
```

RANDOM FOREST
Fit model on NEOTraining data

```r
NEOTraining$classe <- as.factor(NEOTraining$classe)
fitRF <- randomForest(classe ~ ., data=NEOTraining, method="class")
```
Use model to predict class in validation set (NEOTesting)

```r
predictionRF <- predict(fitRF, NEOTesting, type = "class")
```
Estimate the errors of the prediction algorithm in the Random Forest

```r
confusionMatrix(NEOTesting$classe, predictionRF)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1393    1    0    0    1
##          B    3  945    1    0    0
##          C    0    4  851    0    0
##          D    0    0    8  795    1
##          E    0    0    0    2  899
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9957          
##                  95% CI : (0.9935, 0.9973)
##     No Information Rate : 0.2847          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9946          
##                                           
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9979   0.9947   0.9895   0.9975   0.9978
## Specificity            0.9994   0.9990   0.9990   0.9978   0.9995
## Pos Pred Value         0.9986   0.9958   0.9953   0.9888   0.9978
## Neg Pred Value         0.9991   0.9987   0.9978   0.9995   0.9995
## Prevalence             0.2847   0.1937   0.1754   0.1625   0.1837
## Detection Rate         0.2841   0.1927   0.1735   0.1621   0.1833
## Detection Prevalence   0.2845   0.1935   0.1743   0.1639   0.1837
## Balanced Accuracy      0.9986   0.9969   0.9943   0.9976   0.9986
```

#VI-) TEST THE MODEL TO PREDICT 20 DIFFERENT TEST CASES
# Perform prediction

```r
predictSubmission <- predict(fitRF, testing, type="class")
predictSubmission
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```
