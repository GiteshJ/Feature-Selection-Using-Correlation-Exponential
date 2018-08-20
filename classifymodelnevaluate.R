classifymodelnevaluate <- function(data_train, data_test)
{
  data_train$class<-as.factor(data_train$class)
  data_test$class<-as.factor(data_test$class)
  
  # train a model here
  #model <- svm(class ~ ., data = data_train)
  
  model <- train(class ~ ., method = "rpart", data = data_train) # Code for decision tree model

  #model <- train(class ~ ., method = "rf", data = data_train, prox = TRUE) # Code for random forest model
  
  predictions <- predict(model, data_test, na.action = na.pass)

  #tab <- table(predictions, data_test$class)

  cm <- confusionMatrix(predictions, data_test$class)
  print(cm)

  cm$overall['Accuracy']
}