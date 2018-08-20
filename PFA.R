
pfa<-function(data,thresh,fudge)
{
    x<-scale(data)
    xpca<-prcomp(x)
    xev<-xpca$sdev^2
    xevm<-as.matrix(xev)
    xevm<-xevm/sum(xevm[,1])
    pc<-1
    sum<-0
  
    for (i in 1:nrow(xevm))
    {
      sum<-sum+xevm[i,]
      if(sum>thresh)
      {
        pc<-i
        break
      }
    }
    mf<-c()
    aq<-as.data.frame(abs(xpca$rotation[,1:pc]))
    fudge<-pc+fudge
    for (h in 1:10)
    {
    x=sample(1:10000,1)
    set.seed(x)
    km<-kmeans(aq,fudge,iter.max = 25, nstart = 10)
    aqc<-cbind(aq,km$cluster)
    minv<-c()
    f<-c()
    for (i in 1:fudge)
    {
    aqcs<- subset(aqc,km$cluster==i,select=c(1:ncol(aqc)-1))
    min<-500
    for(j in 1:nrow(aqcs))
    {
     temp<-rbind(aqcs[j,],km$center[i,])
     tempdist<-dist(temp)
     if(tempdist<min)
     {f
      min<- tempdist
      mine<- aqcs[j,]
      z<-rownames(aqcs[j,])
     }
    }
   f<-c(f,z)
   minv<-rbind(minv,mine)
  }
  mf<-c(mf,f)
  }
  
  mf<-rownames(sort(table(mf),decreasing=TRUE))[1:fudge]
  return(mf)
}

purity <- function(data,nc,nvr)
{
  purity<- c()
  sumpure<-0
  #removing the class
  datac<-data[-nvr]
  datac<-scale(datac)
  #performing K means on the data
  
  for ( l in 1 : 100)
  {
  x1<-sample(1:10000,1)
  
  set.seed(x1)
  kmdata<-kmeans(datac,nc,iter.max = 25, nstart = 10)
  # comparing the class and the cluster information
  
  pure<-as.matrix(table(kmdata$cluster,t(data[nvr])))
  
  
  # Looping to find out maximum of each class
  
  for(i in 1 : ncol(pure))
  {
  sumpure<-sumpure+max(pure[i,])
  
  }
  
  purity<-c(purity,sumpure/nrow(data))
  sumpure<-0
  }
  return(mean(purity))

}

runPFA <- function(data)
{
  class <- data[ncol(data)]
  data <- data[-ncol(data)]
  
  fs<-fselect(data,.9)
  fpf<-pfa(data,.9,0)
  
  print("PFA : ")
  print(length(c(fpf)))
  print(c(fpf))
  data1<-cbind(data[c(fpf)],class)
  #a<-purity(data1,length(table(class)),ncol(data1))


  #maxclus <- nrow(unique(class))
  
  #a <- getSW(data1[-ncol(data1)], maxclus)
  #return(a)
}


getSW <- function(subsetdata, maxclus)
{
  x <- as.data.frame(subsetdata)
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
}




