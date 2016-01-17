library(caret); library(kernlab); data(spam);

# Intro to Carret package
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)
dim(testing)

## fit a model
set.seed(32343)

modelFit <- train(type ~., data=training, method="glm")
modelFit

modelFit$finalModel

## make some predictions with the model
predictions <- predict(modelFit, newdata=testing)
predictions

## here you should look at "predictions" dataframe and compare some elements with "testing" dataframe
predictions[1002]
testing[1002,]

## get some stats with confusion matrix
confusionMatrix(predictions, testing$type)

# Data slicing - folds
set.seed(32323)

## build training set k-folds
folds <- createFolds(y=spam$type, k=10, list=TRUE,returnTrain=TRUE)
sapply(folds, length) # all folds are almost the same size

## look at the first fold
folds[[1]][1:10]

## build test set k-folds
folds <- createFolds(y=spam$type, k=10, list=TRUE,returnTrain=FALSE)
sapply(folds, length) # all folds are smaller size than training set folds
folds[[1]][1:10]

# Data slicing - time slices
set.seed(32323)
tme <- 1:1000
folds <- createTimeSlices(y=tme, initialWindow=20, horizon=10)
names(folds)

# look at the first folds
folds$train[[1]]
folds$test[[1]]

# Training options
set.seed()
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

modelFit <- train(type ~ ., data=training, method="glm")

# Plotting predictors
library(ISLR); library(ggplot2); library(caret);
data(Wage);

summary(Wage)

## build testing and training datasets
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing);

## build plots
###feature plot
featurePlot(x=training[, c("age","education","jobclass")], y=training$wage,
            plot="pairs")

### qlot
qplot(age,wage,data=training) # find chunks

### add jobclass information
qplot(age,wage,colour=jobclass,data=training)

### add regression smoothers
qq <- qplot(age,wage,colour=education,data=training)
qq + geom_smooth(method='lm', formula=y~x)

## making factors
library(Hmisc)
cutWage <- cut2(training$wage, g=3)
table(cutWage)

p1 <- qplot(cutWage, age, data=training, fill=cutWage, geom=c('boxplot'))
p1

### show the points in the boxplot
library(gridExtra)
p2 <- qplot(cutWage, age, data=training, fill=cutWage, geom=c('boxplot','jitter'))
grid.arrange(p1,p2,ncol=2)

## tables
t1 <- table(cutWage, training$jobclass)
t1
prop.table(t1,1)

## Density plot
qplot(wage, colour=education, data=training, geom="density")

# Basic Preprocessing
inTrain <- createDataPartition(y=spam$type, p-0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

## Show capital ave lengths
hist(training$capitalAve, main="", xlab="ave. capital run length")

## calculate mean, etc -> sd is much larger than mean, so it's very skewed variables
## hence we should preproces them
mean(training$capitalAve)
sd(training$capitalAve)

## preprocess variables
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve))/sd(trainCapAve)

mean(trainCapAveS); sd(trainCapAveS)

## standartizing test set
testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(testCapAveS); sd(testCapAveS)

## standartizing preProcess function
preObj <- preProcess(training[,-58], method=c("center", "scale")) # -58 -> except "type" (spam/nospam)

trainCapAveS <- predict(preObj, training[,-58])$capitalAve
mean(trainCapAveS); sd(trainCapAveS)

testCapAveS <- predict(preObj,testing[-58])$capitalAve
mean(testCapAveS); sd(testCapAveS)

## standartizing - preProcess argument
set.seed(32343)
modelFit <- train(type ~., data=training, preProcess=c("center","scale"), method="glm")
modelFit

## standartizing Box-Cox transforms
preObj <- preProcess(training[,-58], method=c("BoxCox"))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
mean(trainCapAveS); sd(trainCapAveS)

par(mfrow=c(1,2)); hist(trainCapAveS); qqnorm(trainCapAveS)

## standartizing - imputting data
set.seed(13343)

### make some values NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size=1, prob=0.05)==1
training$capAve[selectNA] <- NA

### impute and standartize
preObj <- preProcess(training[,-58], method="knnImpute")
capAve <- predict(preObj, training[-58])$capAve
mean(capAve); sd(capAve)

### standartize truth values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth - mean(capAveTruth))/sd(capAveTruth)
mean(capAveTruth); sd(capAveTruth)

# Covariance
library(kernlab); data(spam)
spam$capitalAveSq <- spam$capitalAve^2

## Level 1: raw data -> covariates
## Level 2: tidy covariates -> new covariates
library(ISLR); library(ggplot2); library(caret);

inTrain <- createDataPartition(y=Wage$wage, 
              p=0.75, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]

## Basic idea - convert factor variables to indicator variables
table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass, data=training)
### here we have 2-quantitive classes instead of 2-factors (Industrial, Informational)
head(predict(dummies, newdata=training)) 

## removing zero covariance
nsv <- nearZeroVar(training, saveMetrics=TRUE)
nsv

# spline basis
library(splines)
bsBasis <- bs(training$age, df=3)
bsBasis

## fitting curves with splines
lm1 <- lm(wage ~ bsBasis, data=training)
plot(training$age,training$wage,pch=19,cex=0.5)
points(training$age,predict(lm1,newdata=training), col="red",pch=19,cex=0.5)

# Preprocessing with PCA
library(caret); library(kernlab); data(spam);
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

M <- abs(cor(training[,-58]))
diag(M) <- 0
which(M > 0.8, arr.ind = T)

# correlated predictors
names(spam[c(34,32)])
plot(spam[c(34,32)])

## we could rotete the plot
X = 0.71*training$num415 + 0.71*training$num857
Y = 0.71*training$num415 - 0.71*training$num857
plot(X,Y)

## principal component analysis in R - prcomp
smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])

## PCA on SPAM data
typeColor <- ((spam$type=="spam")*1 + 1)
prComp <- prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1], prComp$x[,2], col=typeColor, xlab="PC1", ylab="PC2")

## PCA with caret
preProc <- preProcess(log10(spam[,-58]+1), method="pca", pcaComp=2)
spamPC <- predict(preProc, log10(spam[,-58]+1))
plot(spamPC[,1], spamPC[,2], co=typeColor)

## train it
preProc <- preProcess(log10(training[,-58]+1), method="pca", pcaComp=2)
trainPC <- predict(preProc, log10(training[,-58]+1))
modelFit <- train(training$type ~., method="glm", data=trainPC)

## preprocessing with PCA
testPC <- predict(preProc, log10(testing[,-58]+1))
confusionMatrix(testing$type,predict(modelFit, testPC))

# Predicting with regression
## Key ideas
## - Fit a simple regression model
## - Plug in new covariates and multiply by the coefficients
## - Useful when linear models is (nearly) correct

## Pros
## - easy to implement, 
## - easy to interpret

## Cons
## - Often poor performance in nonlinear settings

library(caret); data(faithful); set.seed(333)
inTrain <- createDataPartition(y=faithful$waiting, p=0.5, list=FALSE)

trainFaith <- faithful[inTrain,]; testFaith <- faithful[-inTrain,]
head(trainFaith)

plot(trainFaith$waiting, trainFaith$eruptions,pch=19, col="blue",xlab="waiting", ylab="Duration")

## fit a linear model ED = b0 + b1*WT1 + e1
lm1 <- lm(eruptions ~ waiting, data=trainFaith)
summary(lm1)
plot(trainFaith$waiting, trainFaith$eruptions,pch=19, col="blue",xlab="waiting", ylab="Duration")
lines(trainFaith$waiting, lm1$fitted,lwd=3)

## predict a new value (wt = 80)
coef(lm1)[1] + coef(lm1)[2]*80

newdata <- data.frame(waiting=80)
predict(lm1, newdata)

## plot predictions - training and test
par(mfrow=c(1,2))
plot(trainFaith$waiting, trainFaith$eruptions, pch=19, col="blue", xlab="waiting", ylab="duration")
lines(trainFaith$waiting,predict(lm1, lwd=3))
plot(testFaith$waiting, testFaith$eruptions,pch=19,col="blue", xlab="waiting", ylab="duration")
lines(testFaith$waiting, predict(lm1, newdata=testFaith), lwd=3)

## get training set/test set errors
### Calculate RMSE on training
sqrt(sum((lm1$fitted - trainFaith$eruptions)^2))

### Calculate RMSE on test
sqrt(sum((predict(lm1,newdata=testFaith) - testFaith$eruptions)^2))

## prediction intervals
pred1 <- predict(lm1, newdata=testFaith,interval="prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting, testFaith$eruptions,pch=19, col="blue")
matlines(testFaith$waiting[ord],pred1[ord,], type="l",,col=c(1,2,2), lty=c(1,1,1), lwd=3)

## do the same with caret
modFit <- train(eruptions ~ waiting, data=trainFaith,method="lm")
summary(modFit)

# Predicting with regression multiple covariates
library(ISLR); library(ggplot2); library(caret)
data(Wage); Wage <- subset(Wage, select=-c(logwage))
summary(Wage)

## build testing and training datasets
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing);

## feature plot
featurePlot(x=training[,c("age", "education", "jobclass")], y= training$wage, plot="pairs")

## plot age verus wage
qplot(age,wage,data=training, colour=jobclass)
qplot(age,wage,data=training, colour=education)

## fit a linear model
## ED = b0 + b1*age +b2*I(jobclass = "Information") + SUM(1,2,3,4)I(education = levelK)
modFit <- train(wage ~ age + jobclass + education, method="lm", data=training)
finMod <- modFit$finalModel
print(modFit)

## diagnostic plot
plot(finMod, 1, pch=19, cex=0.5, col="#00000010")

## color by variables not used in the model
qplot(finMod$fitted, finMod$residuals, colour=race, data=training)

## plot by index
plot(finMod$residuals,pch=19)

## predicted versus thuth in test set
pred <- predict(modFit, testing)
qplot(wage,pred, colour=year, data=testing)

## if you want to use all covariates
modFitAll <- train(wage ~., data=training, method="lm")
pred <- predict(modFitAll, testing)
qplot(wage, pred,data=testing)







