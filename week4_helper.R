# Regularized regression
## Basic idea
## 1. Fit a regression model
## 2. Penalize o(or shrink) large coefficients

## Pros
## - Can help with the bias/variance tradeoff
## - Can help with model selection

## Cons:
## - May be computationally demanding on large data sets
## - Does not perform as well as random forests and boosting

## example - prostate cancer
library(ElemStatLearn); data(prostate);
str(prostate)

small = prostate[1:5,]
lm(lpsa ~., data = small)

# Combining predictors
## Key ideas
## - you can combine classifiers by averaging/voting
## - combining classifiers improves accuracy
## - combining classifiers reduces interpretability
## - boosting, bagging and random forests are variants on this theme

library(ISLR); data(Wage); library(ggplot2); library(caret);
Wage <- subset(Wage, select=-c(logwage));

inBuild <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
validation <- Wage[-inBuild,]; buildData <- Wage[inBuild,]
inTrain <- createDataPartition(y=buildData$wage, p=0.7, list=FALSE)
training <- buildData[inTrain,]; testing <- buildData[-inTrain,]

dim(training); dim(testing); dim(validation)

## build two different models
mod1 <- train(wage ~., method="glm", data=training)
mod2 <- train(wage ~., method="rf", data=training, inControl=trainControl(method="cv"), number=1)

pred1 <- predict(mod1, testing); pred2 <- predict(mod2, testing)
qplot(pred1, pred2, colour=wage, data=testing)

## Fit a model that combines predictors (stacking)
predDF <- data.frame(pred1,pred2,wage=testing$wage)
combModFit <- train(wage ~., method="gam", data=predDF)
combPred <- predict(combModFit, predDF)

## testing errors
sqrt(sum(pred1-testing$wage)^2)
sqrt(sum(pred2-testing$wage)^2)
sqrt(sum(combPred-testing$wage)^2)


## predict on validation data set
pred1V <- predict(mod1, validation); pred2V <- predict(mod2, validation)
predVDF <- data.frame(pred1V, pred2V, wage=testing$wage)
combPredV <- predict(combModFit, predVDF)

## Forecasting google data
library(quantmod)
from.dat <- as.Date("01/01/08", format="%m/%d/%y")
to.dat <- as.Date("12/31/13", format="%m/%d/%y")
getSymbols("GOOG", src="google", from=from.dat, to=to.dat)

head(GOOG)

## summarize monthly and store as time series
mGoog <- to.monthly(GOOG)
googOpen <- Op(mGoog)
ts1 <- ts(googOpen, frequency=12)
plot(ts1,xlab="Years+1", ylab="GOOG")

## decompose a time series into parts
plot(decompose(ts1,), xlab="Years+1")

## training and test set
ts1Train <- window(ts1, start=1, end=5)
ts1Test <- window(ts1, start=5, end=(7-0.01))
ts1Train

plot(ts1Train)
lines(ma(ts1Train,order=3),col="red")

## exponential smoothing
ets1 <- ets(ts1Train,model="MMM")
fcast <- forecast(ets1)
plot(fcast); lines(ts1Test,col="red")
accuracy(fcast, ts1Test)

# Unsupervised prediction
## Key ideas
## - sometimes you don't know the labels for prediction
## - To build a predictor
## -- create clusters
## -- name clusters
## -- build predictor for clusters
## - in a new dataset - predict clusters

data(iris); library(ggplot2); library(caret);
inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training <- iris[inTrain,]; testing <- iris[-inTrain,]
dim(training); dim(testing)

## clusters with k-means
kMeans1 <- kmeans(subset(training, select=-c(Species)), centers=3)
training$clusters <- as.factor(kMeans1$cluster)
qplot(Petal.Width,Petal.Length,colour=clusters, data=training)

## compare to real labels
table(kMeans1$cluster, training$Species)

## build predictor
modFit <- train(clusters ~., data=subset(training,select=-c(Species)), method="rpart")
table(predict(modFit, training), training$Species)

## apply on test
testClusterPred <- predict(modFit, testing)
table(testClusterPred, testing$Species)

