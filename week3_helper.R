# Predicting with trees
data(iris); library(ggplot2);library(caret);library(kernlab);
names(iris)

table(iris$Species)

inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)

training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training);dim(testing);

qplot(Petal.Width, Sepal.Width, colour=Species, data=training)
modFit <- train(Species ~., method = "rpart", data=training)
print(modFit$finalModel)

## Plot tree
plot(modFit$finalModel, uniform=TRUE, main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

## Prettier plots
library(rattle);library(rpart.plot);
fancyRpartPlot(modFit$finalModel)

## predicting new values
predict(modFit, newdata=testing)
