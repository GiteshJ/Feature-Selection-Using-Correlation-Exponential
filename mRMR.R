ourmi<-function(data,class)
{
  x<-ncol(data)
  corc<-c()
  corm<-c()
  data<-cbind(data,class)
  summi <- 0
  mylist = list()
  mylist1 = list()
  k <- 0
  
  featnames <- names(data)
  for(i in 1:(ncol(data)-1))	
  {	
    mylist$feat <- featnames[i]
    featpair <- cbind(data[i], data[ncol(data)])
    probtable <- table(featpair)/nrow(featpair)	
    
    mi <- 0	
    
    for (x in 1:nrow(probtable))	
    {	
      for (y in 1:ncol(probtable))
      {
        if (probtable[x,y] == 0)
        {
          mi <- mi + 0
        }
        else
        {
          mi <- mi + probtable[x,y]*log2(probtable[x,y]/(sum(probtable[x,])*sum(probtable[,y])))
        }
      }
    }

    corc[i]<-mi
    

  }
  corm<-c()
  k<-0
  n<-ncol(data)-1
  corm<-data.frame()
  for(i in 1:n)
  {
  temp<-c()
  for(j in 1: n)	
  {	
    mylist$feat <- featnames[i]
    featpair <- cbind(data[i], data[j])
    probtable <- table(featpair)/nrow(featpair)	
    
    mi <- 0	
    
    for (x in 1:nrow(probtable))	
    {	
      for (y in 1:ncol(probtable))
      {
        if (probtable[x,y] == 0)
        {
          mi <- mi + 0
        }
        else
        {
          mi <- mi + probtable[x,y]*log2(probtable[x,y]/(sum(probtable[x,])*sum(probtable[,y])))
        }
      }
    }
 
    corm[i,j]<-mi
    
  
  }
  }
  

  
  y<-c()


  y$corc<-abs(corc)
  y$corm<-abs(corm)
  return(y)
}

mrmrfitga<-function(x)
{
  # Assumes corm ,corc and n are globally available
  print(x)
  #si<-which(x=='1',arr.ind = TRUE)+n-length(x)
  si<-which(x=='1',arr.ind = TRUE)
  if (sum(si)>0)
  {
  print(si)
  tgt<-corc[si]
  k<-length(tgt)
  rel<-corm[si,si]
  D<-mean(tgt)
  I<-sum(rel)/(k*k)
  if(k==1)
  {
    mrmr<-rel
  }
  else
  {
    mrmr<- D-I
  }
  }
  else {mrmr = 0}
  #print(mrmr)
  #print('reached here')
  return(mrmr)
}


runmRMR<-function(mydata)
{
  starttime <- Sys.time()
  
  mydata$class <- as.factor(mydata$class)
  data <- mydata[-ncol(mydata)]
  
  class <- mydata[,ncol(mydata)]
  
  idx<-sample(1:nrow(mydata),trunc(.7*nrow(mydata)))
  train<-mydata[idx,]
  test<-mydata[-idx,]
  
  k<-ourmi(train,class)
  
  
  corc<-k$corc
  
  corm<-k$corm
  
  n<-ncol(data)
  
  # with mRMR
  
  
  mrmrga<-ga(type = "binary",
             fitness = mrmrfitga,
             nBits = ncol(data),
             pcrossover = 0.7,
             pmutation = 0.001, names=names(data),popSize = 10*ncol(data),maxiter=200)
  
  y<- mrmrga@solution[1,]
  z<-which(y==1,arr.ind = TRUE)
  
  print("Number of features in subset")
  print(length(z))
  
  endtime <- Sys.time()
  print("time of execution")
  print(endtime - starttime)
  
  mydata <- cbind(mydata[z], class)
  idx <- sample(1:nrow(mydata),trunc(.7*nrow(mydata)))
  data_train <- mydata[idx,]
  data_test <- mydata[-idx,]
  
  accuracy <- 0
  
  
  model <- train(class ~ ., method = "rpart", data = data_train) # Code for decision tree model
  
  predictions <- predict(model, data_test, na.action = na.pass)
  
  
  cm <- confusionMatrix(predictions, data_test$class)
  
  accuracy <- cm$overall['Accuracy']
  
  
  return (accuracy)
  
}

