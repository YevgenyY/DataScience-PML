# Q1
## 1. Subset the data to a training set and testing set based on the Case variable in the data set.
## 2. Set the seed to 125 and fit a CART model with the rpart method using all predictor variables and 
##    default caret settings.
## 3. In the final model what would be the final model prediction for cases with the following variable 
## values:

## a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2
## b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100
## c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100
## d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

training <- subset(segmentationOriginal, segmentationOriginal$Case=="Train")
testing <- subset(segmentationOriginal, segmentationOriginal$Case=="Test")
dim(training); dim(testing)

set.seed(125)
modFit <- train(Class ~., method = "rpart", data=training)
modFit$finalModel

### print plot
library(rattle);library(rpart.plot);
fancyRpartPlot(modFit$finalModel)

## Answer:
### a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2 => PS
### b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100 => WS
### c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100 => PS
### d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2 => not possible to predict


# Q2
## If K is small in a K-fold cross validation is the bias in the estimate of out-of-sample (test set) 
## accuracy smaller or bigger? If K is small is the variance in the estimate of out-of-sample (test set) 
## accuracy smaller or bigger. Is K large or small in leave one out cross validation?

## Answer: The bias is larger and the variance is smaller. Under leave one out cross validation K is 
## equal to the sample size.

# Q3
## These data contain information on 572 different Italian olive oils from multiple regions in Italy. 
## Fit a classification tree where Area is the outcome variable. Then predict the value of area for the 
## following data frame using the tree command with all defaults
## What is the resulting prediction? Is the resulting prediction strange? Why or why not?
library(pgmm)
data(olive)
olive = olive[,-1]
newdata = as.data.frame(t(colMeans(olive)))
names(olive)
training <- olive

modFit <- train(Area ~., method = "rpart", data=training)
modFit$finalModel
fancyRpartPlot(modFit$finalModel)
predict(modFit, newdata)

# Q4
## Load the South Africa Heart Disease Data and create training and test sets with the 
## following code:
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

## Then set the seed to 13234 and fit a logistic regression model (method="glm", be sure to 
## specify family="binomial") with Coronary Heart Disease (chd) as the outcome and age at onset, 
## current alcohol consumption, obesity levels, cumulative tabacco, type-A behavior, and low 
## density lipoprotein cholesterol as predictors. Calculate the misclassification rate for your 
## model using this function and a prediction on the "response" scale:

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

# What is the misclassification rate on the training set? What is the misclassification rate on 
# the test set?

modFit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, 
                data=trainSA, method="glm", family="binomial")
modFit$finalModel

predTrain <- predict(modFit, trainSA)
missClass(trainSA$chd, predTrain)

predTest <- predict(modFit, testSA)
missClass(testSA$chd, predTest)

# Q5
## Load the vowel.train and vowel.test data sets:
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
## Set the variable y to be a factor variable in both the training and test set. Then set the seed 
## to 33833. Fit a random forest predictor relating the factor variable y to the remaining variables. 
## Read about variable importance in random forests here: 
## http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#ooberr The caret package uses by 
## default the Gini importance.

## Calculate the variable importance using the varImp function in the caret package. What is the 
## order of variable importance?

vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)

modFit <- train(y ~ ., method="rf", data = vowel.train, prox=TRUE)
modFit$finalModel

varImp(modFit)

