ASW<-function(x, maxclus){

  x <- as.data.frame(x)
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
