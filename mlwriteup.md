Practical Machine Learning: Assignment Writeup
========================================================
##Executive summary

 In this project, the goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. 
 
The goal of your project is to predict the manner in which they did the exercise

#Data Preparation

```r
#Data loading
ppmlTrain=read.csv("pml-training.csv", header=T, na.strings=c("NA", "#DIV/0!"))
ppmlTest=read.csv("pml-testing.csv", header=T, na.string=c("NA", "#DIV/0!"))
```

-all variables with at least one "NA" were excluded from the analysis. 
-Variables related to time and user information were excluded 
-Same variables were mainteined in the test data set to be used for predicting the 20 test cases


```r
## NA exclusion for all the variables
NONAppmlTrain=ppmlTrain[, apply(ppmlTrain, 2, function(x) !any(is.na(x)))] 
```


```r
## variables with user information, time and undefined
cleanedppmlTrain=NONAppmlTrain[,-c(1:8)]
```

```r
#20 test cases
cleanedppmltest=ppmlTest[,names(cleanedppmlTrain[,-52])]
```


#Partition and predicting


```r
library(randomForest)
library(caret)
Train=createDataPartition(y=cleanedppmlTrain$classe, p=0.75,list=F)
training=cleanedppmlTrain[Train,] 
test=cleanedppmlTrain[-Train,] 
```
#Conclusion

Random forest trees were generated for the training dataset using cross-validation. The algorithm was examnined with the partitioned test set to determine the accuracy and estimated error of prediction. 


```r
library(e1071)
library(randomForest)
library(caret)

fc=trainControl(method="cv", number=5, allowParallel=T, verbose=T)
rffit=train(classe~.,data=training, method="rf", trControl=fc, verbose=F)
```

```
## + Fold1: mtry= 2 
## - Fold1: mtry= 2 
## + Fold1: mtry=26 
## - Fold1: mtry=26 
## + Fold1: mtry=51 
## - Fold1: mtry=51 
## + Fold2: mtry= 2 
## - Fold2: mtry= 2 
## + Fold2: mtry=26 
## - Fold2: mtry=26 
## + Fold2: mtry=51 
## - Fold2: mtry=51 
## + Fold3: mtry= 2 
## - Fold3: mtry= 2 
## + Fold3: mtry=26 
## - Fold3: mtry=26 
## + Fold3: mtry=51 
## - Fold3: mtry=51 
## + Fold4: mtry= 2 
## - Fold4: mtry= 2 
## + Fold4: mtry=26 
## - Fold4: mtry=26 
## + Fold4: mtry=51 
## - Fold4: mtry=51 
## + Fold5: mtry= 2 
## - Fold5: mtry= 2 
## + Fold5: mtry=26 
## - Fold5: mtry=26 
## + Fold5: mtry=51 
## - Fold5: mtry=51 
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 26 on full training set
```



```r
predictrf=predict(rffit, newdata=test)
confusionMatrix(predictrf, test$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1394    4    0    0    0
##          B    1  942    8    0    0
##          C    0    3  847    9    2
##          D    0    0    0  792    2
##          E    0    0    0    3  897
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9935          
##                  95% CI : (0.9908, 0.9955)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9917          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9993   0.9926   0.9906   0.9851   0.9956
## Specificity            0.9989   0.9977   0.9965   0.9995   0.9993
## Pos Pred Value         0.9971   0.9905   0.9837   0.9975   0.9967
## Neg Pred Value         0.9997   0.9982   0.9980   0.9971   0.9990
## Prevalence             0.2845   0.1935   0.1743   0.1639   0.1837
## Detection Rate         0.2843   0.1921   0.1727   0.1615   0.1829
## Detection Prevalence   0.2851   0.1939   0.1756   0.1619   0.1835
## Balanced Accuracy      0.9991   0.9952   0.9936   0.9923   0.9974
```


```r
predof20=predict(rffit, newdata=cleanedppmltest)
# prediction of the 20 cases
predof20
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```

```r
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(predof20)
```
