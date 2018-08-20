runCFS <- function(data){
  
  
  starttime <- Sys.time()
  
  subset <- cfs(class~., data)
  
  endtime <- Sys.time()
  print("total time of execution")
  print(endtime - starttime)
  
  print("Number of subset fields")
  print(length(subset))
  #print(ncol(data))
  classfld <- data[ncol(data)]
  colindices <- grep(subset, colnames(data))
  
  mydata <- cbind(data[colindices], classfld)
  
  idx <- sample(1:nrow(data),trunc(.7*nrow(data)))
  data_train <- mydata[idx,]
  data_test <- mydata[-idx,]
  
  accuracy <- 0
  
  data_train$class <- as.factor(data_train$class)
  data_test$class <- as.factor(data_test$class)
  
  # train a model here

  model <- train(class ~ ., method = "rpart", data = data_train) # Code for decision tree model
  
  predictions <- predict(model, data_test, na.action = na.pass)
  
  cm <- confusionMatrix(predictions, data_test$class)
  
  accuracy <- cm$overall['Accuracy']
  print(accuracy)
  

  return (accuracy)
}
