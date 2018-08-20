implementDSCA <-function(datafilename){
  
  #setwd("F:\\Working Folder\\PhD\\2 - Folder for WIP Papers\\Current works\\Paper 5 - Unification of supervised and unsupervised feature selection\\Coding")
  library(entropy)
  library(infotheo)
  library (caret)
  library (klaR)
  library (e1071)
  source("classifymodelnevaluate.R")

  #csvdata <- read.table("data/mfeat.csv", header=TRUE, sep=",")
  starttime <- Sys.time()
  csvdata <- read.table(datafilename, header=TRUE, sep=",")
  mydata <- csvdata[,-ncol(csvdata)]
  print("hi")
  class <- as.numeric(factor(csvdata$class , levels = unique(csvdata[,ncol(csvdata)])))
  csvdata <- cbind(mydata,class)
  
  cord <- matrix(nrow = ncol(mydata), ncol = ncol(mydata))
  
  for(i in 1:ncol(mydata))
  {
    for(j in 1:ncol(mydata)){
      featpair <- rbind( c(mydata[,i]), mydata[,j])
      if (i == j){
        cord[i, j] <- 0
      }
      else{
        if(is.finite(mi.plugin(featpair))){
          cord[i, j] <- mi.plugin(featpair)
        }
        else{
          cord[i, j] <- 0
        }
      }
    }
  }
  colnames(cord) <- names(mydata)
  rownames(cord) <- names(mydata)
  DScounter <- 1
  DS <- as.list(numeric())
  DSfeatnames <- as.list(numeric())
  while(length(cord) > 1){
    sumval <- sum(cord)/2
    sumDS <- max(cord)
    if (max(cord) == 0)
      break
    arrind <- arrayIndex(which.max(cord), dim=dim(cord))
    cord[arrind] = 0
    cord[arrind[1,2], arrind[1,1]] = 0
    DS[[DScounter]] <- c(arrind[1,1], arrind[1,2])
    while(sumDS <= (sumval - sumDS)){
      maxval <- 0
      for(i in 1:length(DS[[DScounter]])){
        if (max(cord[DS[[DScounter]][i], ]) > maxval){
          maxval <- max(cord[DS[[DScounter]][i], ])
          arrind <- which.max(cord[DS[[DScounter]][i], ])
        }
      }
      for(i in 1:length(DS[[DScounter]])){
        sumDS = sumDS + cord[DS[[DScounter]][i],arrind ]
        cord[DS[[DScounter]][i],arrind ] = 0
        cord[arrind, DS[[DScounter]][i]] = 0
      }
      
      DS[[DScounter]] <- c(DS[[DScounter]], arrind)
    }
    featindices <- DS[[DScounter]]
    DSfeatnames[[DScounter]] <- rownames(cord[featindices,])
    cord <- cord[-featindices, -featindices]
    DScounter <- DScounter + 1
  }
  featsubsetlist <- c()
  
  for (i in 1: length(DSfeatnames)){
    featsubset <- getDSsubsetfeats(DSfeatnames[[i]], csvdata)
    featsubsetlist <- c(featsubsetlist, featsubset)
  }
  
  endtime <- Sys.time()
  print("Total execution time : ")
  print(starttime - endtime)
  
  print(length(featsubsetlist))
  #print(featsubsetlist)
  x1 <- sample (1:10000, 1)
  set.seed (x1)
  split = 0.7
  
  modeldata <- cbind(csvdata[featsubsetlist], csvdata[ncol(csvdata)])
  trainIndex <- createDataPartition (y = csvdata$class, p = split, list = FALSE)
  data_train <- csvdata [trainIndex,]
  data_test <- csvdata [-trainIndex,]
  subsetaccuracy <- classifymodelnevaluate(data_train, data_test)
  print("Accuracy : ")
  print(subsetaccuracy)
  #subsetaccuracy
}

getDSsubsetfeats <- function(DSfeats, inputdata) {
  featsubset <- c()
  
  inputdata <- discretize(inputdata)
  
  matfeatclassmi <- matrix(nrow = 1, ncol = ncol(inputdata[DSfeats]))
  DSdata <- inputdata[DSfeats]
  colnames(matfeatclassmi) <- DSfeats
  
  maxval <- 0
  while(length(DSfeats) > 0){
    for(i in 1:length(DSfeats))
    {
      featpair <- rbind( c(DSdata[,i]), inputdata[ , ncol(inputdata)])
      
      if(is.finite(mi.plugin(featpair))){
        matfeatclassmi[1, i] <- mi.plugin(featpair)
      }
      else{
        matfeatclassmi[1, i] <- 0
      }
      if (length(featsubset) > 0){
        for (j in 1:length(featsubset)){
          featpair <- rbind( c(DSdata[,i]), DSdata[,j])
          if(is.finite(mi.plugin(featpair))){
            matfeatclassmi[1, i] <- matfeatclassmi[1, i] - mi.plugin(featpair)
          }
          
          matfeatclassmi[1, i] <- matfeatclassmi[1, i] + condinformation(DSdata[,i],DSdata[,j],inputdata[ , ncol(inputdata)],method="emp")
        }
      }
    }
    
    if(max(matfeatclassmi) > maxval){
      maxval <- max(matfeatclassmi)
      featsubset <- DSfeats[which.max(matfeatclassmi)]
      DSfeats <- DSfeats[-which.max(matfeatclassmi)]
    }
    else{
      break
    }
  }

  featsubset
}


arrayIndex <- function(i, dim) {
  ndim <- length(dim);      # number of dimension
  v <- cumprod(c(1,dim));  # base
  
  # Allocate return matrix
  j <- matrix(NA, nrow=length(i), ncol=ndim);
  
  i <- (i-1);     # one-based indices
  for (kk in 1:ndim)
    j[,kk] <- (i %% v[kk+1])/v[kk];
  1 + floor(j);  # one-based indices
}