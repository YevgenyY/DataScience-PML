# Q1. Which of the following commands will create training and test sets 
# with about 50% of the observations assigned to each?
library(AppliedPredictiveModeling)
data(AlzheimerDisease)

summary(predictors)
summary(diagnosis)

dim(predictors); length(diagnosis)

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]
dim(training); dim(testing)

adData = data.frame(diagnosis,predictors)
testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[-testIndex,]
testing = adData[testIndex,]
dim(training); dim(testing)

# Q2 Make a plot of the outcome (CompressiveStrength) versus the index of the samples. 
# Color by each of the variables in the data set (you may find the cut2() function in 
# the Hmisc package useful for turning continuous covariates into factors). What do 
# you notice in these plots?

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

names <- colnames(concrete)
names <- names[-length(names)]

featurePlot(x = training[, names], y = training$CompressiveStrength, plot = "pairs")

## it is clear that there is no connections between outcom and any variable

index <- seq_along(1:nrow(training))
ggplot(data = training, aes(x = index, y = CompressiveStrength)) + geom_point() + 
  theme_bw()

library(Hmisc)
cutCS <- cut2(training$CompressiveStrength, g = 4)
summary(cutCS)

ggplot(data = training, aes(y = index, x = cutCS)) + geom_boxplot() + geom_jitter(col = "blue") + 
  theme_bw()

featurePlot(x = training[, names], y = cutCS, plot = "box")
# There is a non-random pattern in the plot of the outcome versus index that does not appear to be perfectly explained by any predictor suggesting a variable may be missing.

# Q3 Make a histogram and confirm the SuperPlasticizer variable is skewed. Normally you might 
# use the log transform to try to make the data more symmetric. Why would that be a poor choice 
# for this variable?
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

ggplot(data = training, aes(x = Superplasticizer)) + geom_histogram() + theme_bw()

# Answer: The log transform does not reduce the skewness of the non-zero values of SuperPlasticizer

# Q4 Find all the predictor variables in the training set that begin with IL. 
# Perform principal components on these variables with the preProcess() function 
# from the caret package. Calculate the number of principal components needed to 
# capture 90% of the variance. How many are there?
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]training = adData[ inTrain,]
testing = adData[-inTrain,]
training = adData[inTrain,]

IL_variables <- grep("^IL", names(training), value = TRUE)
preProc <- preProcess(training[, IL_variables], method = "pca", thresh = 0.9)
preProc
# Answer: 9

# Q5 Create a training data set consisting of only the predictors with variable
# names beginning with IL and the diagnosis. Build two predictive models, one 
# using the predictors as they are and one using PCA with principal components 
# explaining 80% of the variance in the predictors. Use method="glm" in the 
# train function.

# What is the accuracy of each method in the test set? Which is more accurate?
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

trainSubset = training[,grep("^IL", names(training))]
testSubset = testing[,grep("^IL", names(testing))]
pp = preProcess(trainSubset, thresh = 0.8, method = "pca")
trainTransformed <- predict(pp, trainSubset)
testTransformed <- predict(pp, testSubset)
trainSubset$diagnosis = training$diagnosis
testSubset$diagnosis = testing$diagnosis
trainTransformed$diagnosis = training$diagnosis
testTransformed$diagnosis = testing$diagnosis
glmpca = train(diagnosis ~ ., data = trainTransformed, method = "glm")
glm = train(diagnosis ~ ., data = trainSubset, method = "glm")
confusionMatrix(testSubset$diagnosis,predict(glm, testSubset))$overall["Accuracy"]
confusionMatrix(testTransformed$diagnosis,predict(glmpca, testTransformed))$overall["Accuracy"]
