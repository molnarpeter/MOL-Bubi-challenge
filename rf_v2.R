library("caret", lib.loc="C:/R/R-3.1.1/library")

library(doSNOW)
cl <- makeCluster(5, type = "SOCK")
registerDoSNOW(cl)

setwd("C:/Bubi_challenge")
data500 <- read.csv("C:/Bubi_challenge/newdata500.csv", sep=";", dec=",", na.strings="#HI\x01NYZIK", stringsAsFactors=FALSE)

training_set <- subset(data500, set=='training')
test_set <- subset(data500, set=='test')

predictions =  data.frame(matrix(ncol = 213, nrow = 720))

training_set[,2] <- as.factor(training_set[,2])
training_set[,6] <- as.factor(training_set[,6])

test_set[,2] <- as.factor(test_set[,2])
test_set[,6] <- as.factor(test_set[,6])

for (i in 13:13 ) {
 
  model_rf <- train(training_set[,c(2,6:12)] , as.numeric(training_set[,i]) ,
        data = training_set, method = "rf", prox = TRUE, trControl = trainControl(method = "CV", number=3, allowParallel = TRUE))
  
  j=i-12
  prediction_rf <- predict(model_rf, newdata=training_set[,c(2,6:12)])
  predictions[,j] <- prediction_rf
  
}

#write.table(predictions, file="submission.csv")
