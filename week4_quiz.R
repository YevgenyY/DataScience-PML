# Q1.
# Set the variable y to be a factor variable in both the training and test set. Then set 
# the seed to 33833. Fit (1) a random forest predictor relating the factor variable y to 
# the remaining variables and (2) a boosted predictor using the "gbm" method. Fit these 
# both with the train() command in the caret package.

# What are the accuracies for the two approaches on the test data set? What is the accuracy 
# among the test set samples where the two methods agree?
library(ElemStatLearn); library(caret)
data(vowel.train)
data(vowel.test)

vowel.test$y <- as.factor(vowel.test$y)
vowel.train$y <- as.factor(vowel.train$y)
set.seed(33833)

modFit1 <- train(y ~ ., method="rf", data=vowel.train);
modFit2 <- train(y ~ ., method="gbm", data=vowel.train);

pred1 <- predict(modFit1, vowel.test)
pred2 <- predict(modFit2, vowel.test)

cfm1 <- confusionMatrix(vowel.test$y, pred1)
cfm1$overall['Accuracy']

cfm2 <- confusionMatrix(vowel.test$y, pred2)
cfm2$overall['Accuracy']

## agreed accuracy
idx <- (pred1 == pred2)
confusionMatrix(vowel.test$y[idx], pred1[idx])$overall['Accuracy']


# Q2
# Set the seed to 62433 and predict diagnosis with all the other variables using a random 
# forest ("rf"), boosted trees ("gbm") and linear discriminant analysis ("lda") model. Stack 
# the predictions together using random forests ("rf"). What is the resulting accuracy on the 
# test set? Is it better or worse than each of the individual predictions?
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
# train models
modFitRF <- train(diagnosis ~., method="rf", data=training)   # random forests
modFitBT <- train(diagnosis ~., method="gbm", data=training)  # boosted trees
modFitLDA <- train(diagnosis ~., method="lda", data=training) # linear discriminant analysis

# predict diagnosis
predRF <- predict(modFitRF, testing)
predBT <- predict(modFitBT, testing)
predLDA <- predict(modFitLDA, testing)

confusionMatrix(testing$diagnosis, predRF)$overall['Accuracy']
confusionMatrix(testing$diagnosis, predBT)$overall['Accuracy']
confusionMatrix(testing$diagnosis, predLDA)$overall['Accuracy']

# stack models
predDF <- data.frame(predRF, predBT, predLDA, diag=testing$diagnosis)
combModFit <- train(diag ~., method="rf", data=predDF)
predComb <- predict(combModFit, testing)
confusionMatrix(testing$diagnosis, predComb)$overall['Accuracy']

# Q3
# Set the seed to 233 and fit a lasso model to predict Compressive Strength. Which variable is
# the last coefficient to be set to zero as the penalty increases? (Hint: it may be useful to look 
# up ?plot.enet).

set.seed(3523)
library(AppliedPredictiveModeling); library(caret)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)
modFit <- train(CompressiveStrength ~., method="lasso", data=training)

# plot the model
plot.enet(modFit$finalModel, xvar="penalty", use.color=TRUE)

# Q4
# Fit a model using the bats() function in the forecast package to the training time series. Then 
# forecast this model for the remaining time points. For how many of the testing points is the true 
# value within the 95% prediction interval bounds?

library(lubridate) # For year() function below
dat = read.csv("data/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

library(forecast)
mod_ts <- bats(tstrain)
fcast <- forecast(mod_ts, level = 95, h = dim(testing)[1])
sum(fcast$lower < testing$visitsTumblr & testing$visitsTumblr < fcast$upper) / 
  dim(testing)[1]

# Q5
# Set the seed to 325 and fit a support vector machine using the e1071 package to predict Compressive 
# Strength using the default settings. Predict on the testing set. What is the RMSE?
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(325)
library(e1071); library(caret); library(forecast)
modFitSVM <- svm(CompressiveStrength ~ ., data = training)
predSVM <- predict(modFitSVM, testing)
accuracy(predSVM, testing$CompressiveStrength)


