library(caret); library(kernlab); data(spam);

inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)
dim(testing)

# fit a model
set.seed(32343)

modelFit <- train(type ~., data=training, method="glm")
modelFit

modelFit$finalModel

# make some predictions with the model
predictions <- predict(modelFit, newdata=testing)
predictions

# here you should look at "predictions" dataframe and compare some elements with "testing" dataframe
predictions[1002]
testing[1002,]

# get some stats with confusion matrix
confusionMatrix(predictions, testing$type)


