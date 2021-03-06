# Predicting with trees
data(iris); library(ggplot2);library(caret);library(kernlab);
names(iris)

table(iris$Species)

inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)

training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training);dim(testing);

qplot(Petal.Width, Sepal.Width, colour=Species, data=training)
qplot(Petal.Length, Sepal.Length, colour=Species, data=training)
modFit <- train(Species ~., method = "rpart", data=training)
print(modFit$finalModel)

## Plot tree
plot(modFit$finalModel, uniform=TRUE, main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

## Prettier plots
library(rattle);library(rpart.plot);
fancyRpartPlot(modFit$finalModel)

## predicting new values
predictions <- predict(modFit, newdata=testing)
predictions

confusionMatrix(predictions, testing$Species)

# Bagging
## Ozone data
library(ElemStatLearn); data(ozone,package="ElemStatLearn")
ozone <- ozone[order(ozone$ozone)]
head(ozone) 

## Let's predict temperature as a function of ozone
ll <- matrix(NA,nrow=10,ncol=155)
for(i in 1:10) {
  ss <- sample(1:dim(ozone)[1], replace=T)
  ozone0 <- ozone[ss,]; ozone0 <- ozone0[order(ozone0$ozone),]
  loess0 <- loess(temperature ~ ozone, data=ozone0, span=0.2)
  ll[i,] <- predict(loess0, newdata=data.frame(ozone=1:155))
}

## plotting the curves
plot(ozone$ozone,ozone$temperaturerature,pch=19,cex=0.5)
for(i in 1:10) { lines(1:155, ll[i,], col="grey", lwd=2)}
lines(1:155,apply(ll,2,mean), col="red", lwd=2)

## bagging in caret package
predictors <- data.frame(ozone=ozone$ozone)
temperature <- ozone$temperature
treebag <- bag(predictors, temperature, B=10,
               bagControl = bagControl(fit=ctreeBag$fit,
                                       predict = ctreeBag$pred,
                                       aggregate = ctreeBag$aggregate))

## example of custom baging 
plot(ozone$ozone, temperature,col="lightgrey",pch=19)
points(ozone$ozone,predict(treebag$fits[[1]]$fit,predictors), pch=19,col="red")
points(ozone$ozone,predict(treebag, predictors), pch=19,col="blue")

## have a look at functions (parts of bagging)
ctreeBag$pred
ctreeBag$aggregate

# Random Forests
## Basic idea
## - bootstrap samples
## - at each split, bootstrap variables
## - Grow multiple trees and vote

data(iris); library(ggplot2)
inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

library(caret)
modFit <- train(Species ~ ., data=training, method="rf", prox=TRUE)
modFit

## getting a single tree
getTree(modFit$finalModel, k=2)

## Class centers
irisP <- classCenter(training[,c(3,4)], training$Species, modFit$finalModel$prox)
irisP <- as.data.frame(irisP); irisP$Species <- rownames(irisP)
p <- qplot(Petal.Width, Petal.Length, col=Species, data=training)
p+ geom_point(aes(x=Petal.Width, y=Petal.Length, col=Species), size =5, shape=4, data=irisP)

## predicting new values
pred <- predict(modFit,testing); testing$predRight <- pred==testing$Species
table(pred,testing$Species)

qplot(Petal.Width, Petal.Length, colour=predRight,data=testing,main="newdata Predictions")

# Boosting
## Basic idea ( https://ru.wikipedia.org/wiki/AdaBoost )
## - take lots of possible weak predictors
## - Weight them and add them up
## - Get a stronger predictor
library(ISLR); data(Wage); library(ggplot2); library(caret);
Wage <- subset(Wage,select=-c(logwage))
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,];

## Fit the model
modFit <- train(wage ~., method="gbm",data=training,verbose=FALSE)

## plot the results
qplot(predict(modFit,testing), wage,data=testing)

# Model based prediction
## Basic idea 
## - assume the data follow a probabilistic model
## - use Bayes' theorem to identify optimal classifiers
## Pros:
## - can takeadvantages of structure of the data
## - may be computationally convenient
## - are reasonable accurate on real problems
## Cons:
## - make additional assumptions about the data
## - when the model is incorrect you may get reduced accuracy

data(iris); library(ggplot2);
names(iris)
table(iris$Species)

inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing);

## building predictions
modlda <- train(Species ~., data=training, method="lda")
modnb <- train(Species ~., data=training, method="nb")
plda <- predict(modlda,testing); pnb = predict(modnb,testing)
table(plda,pnb)

## comparision of the result
equalPredictions = (plda == pnb)
qplot(Petal.Width, Sepal.Width, colour=equalPredictions, data=testing)















