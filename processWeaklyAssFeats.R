processWeaklyAssFeats <- function(myweakdata, beta, classficn_ind)
{
  library("entropy")
  
  ftsubsetcount <- round((ncol(myweakdata)*beta),0)

  ftnames <- c()
  ftsubsetnames <- c()
  ftnames <- names(myweakdata)

  x <- ncol(myweakdata)
  corc <- c()
  
  if (classficn_ind == 0 ){
    for(i in 1:(x-1))
    {
      freqs2d = rbind( c(myweakdata[,i]), myweakdata[,x])
      corc[i] <- mi.plugin(freqs2d)
    }
  }
  
  if (classficn_ind == 1 ){
    for(i in 1:x)
    {
      corc[i] <- entropy.ChaoShen(myweakdata[,-i], unit=c("log2"))
    }
  }
  
  y <- c()
  y$corc <- corc

  ranken <- rank(-y$corc)
  
  if (classficn_ind == 0 ){
    x <- x - 1
  }

  for (i in 1:ftsubsetcount){
    for (j in 1:x)
    {
      if (ranken[j] == i){
        ftsubsetnames <- c(ftsubsetnames, ftnames[j])
      }
    }
  }
  
  return(ftsubsetnames)
}