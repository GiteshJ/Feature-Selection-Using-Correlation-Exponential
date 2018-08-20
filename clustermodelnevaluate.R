clustermodelnevaluate <- function(subsetdata, maxclus)
{
  library(cluster)
  library(fpc)
  
  purity <- c()
  sumpure <- 0
  for ( l in 1 : 100)
  {
    x1 <- sample(1:10000, 1)
    set.seed(x1)

    kmdata <- kmeans(subsetdata[-ncol(subsetdata)], length(table(subsetdata[ncol(subsetdata)])), iter.max = 35, nstart = 10)
    pure <- as.matrix(table(kmdata$cluster,t(subsetdata[ncol(subsetdata)])))
    sumpure = 0
    
   for(i in 1 : ncol(pure))
    {
      sumpure <- sumpure + max(pure[i,])
    }
    temp <- sumpure / nrow(subsetdata)
    purity <- c(purity, temp)
  }
  
  return(mean(purity))
  
  'x <- as.data.frame(subsetdata)
  print(head(subsetdata))
  print(maxclus)
  id <- as.integer(x[1,1])
  people <- length(as.vector(x[,1]))
  if (people == 1){
    p = 0
  } 
  else {
    diss <- daisy(x, metric="gower")
    asw <- numeric(maxclus)
    for (k in 2:maxclus) {
      asw[[k]] <- pam(diss, k, diss=T) $ silinfo $ avg.width
    }
    k.best <- which.max(asw)
    swg <- asw[k.best]
  }  
  swg  

  return(swg)'
  
}