library(caret)
library(kernlab)
library(stringr)
library(randomForest)
setwd("C:/dev/DataScience/MachineLearning/Proj")

training <- read.csv("pml-training.csv", header=T)
testing <- read.csv("pml-testing.csv", header=T)

set.seed(22345)


##Transformations
colCnt <- dim(training)[2] - 1
colRemoveidx = c();

for (i in 1:colCnt)
  if(is.factor(training[[i]]))
    colRemoveidx <- c(colRemoveidx, i)

training <- training[,-colRemoveidx]

training <- training[,-c(9:24, 38:47, 57:65, 69:74,76:85,98:114)]
training <- training[,-c(1:4)]

inTrain <- createDataPartition(y=training$classe, p = 0.7, list = F)
subTrain <- training[inTrain,]
subTest <- training[-inTrain,]


totalPredictorCount <- dim(subTest)[2]

fit <- randomForest(classe ~. ,data=subTrain,mtry=sqrt(totalPredictorCount),importance=TRUE)
pred <- predict(fit, newdata=subTest)
confusionMatrix(subTest$classe, pred)

totalPredictorCount
