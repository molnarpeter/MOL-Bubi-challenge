set.seed(42)

library("caret", lib.loc="C:/R/R-3.1.1/library")

library(doSNOW)
cl <- makeCluster(5, type = "SOCK")
registerDoSNOW(cl)

setwd("C:/Bubi_challenge")
data <- read.csv("C:/Bubi_challenge/newdat_more_features_400_3.csv", sep=";", dec=",", na.strings="#HI\x01NYZIK", stringsAsFactors=FALSE)

gbmGrid <- expand.grid(interaction.depth = (1:6) * 2,
                       n.trees = (1:10)*100,
                       shrinkage = .0001,
                       n.minobsinnode = (1:3)
)

training_set <- subset(data, Test==0)

test_set <- subset(data, Test==1)

newGrid = expand.grid(mtry = c(1:14))

predictions =  data.frame(matrix(ncol = 400, nrow = 90))

diagnostics =  data.frame(matrix(ncol = 2, nrow = 400))


training_features <- training_set[,c(3,4,5,7:19)]
test_features <- test_set[,c(3,4,5, 7:19)]

for (i in 180:419 ) {
  
  j=i-19

gbmFit <- train(training_features, as.numeric(training_set[,i]),
                method = "gbm", 
                distribution="poisson",
                trControl = trainControl(method       = "repeatedcv",
                                         number       = 5,
                                         repeats      = 3,
                                         allowParallel = TRUE), 
                verbose = FALSE,
                
                tuneGrid = gbmGrid)

predictions[,j] <- predict(gbmFit, newdata=test_features)

diagnostics[j,1] <- min(gbmFit$results[,4])
diagnostics[j,2] <- max(gbmFit$results[,5])
}

write.table(predictions, file="submission_from_gbm_2.csv")
write.table(diagnostics, file="diagnostics_gbm_2.csv")