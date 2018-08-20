getSilhouttewidth <-function(x, maxclus){
  
  source("ASW.R")
 
  mydata <- read.table("data/apndcts.csv", header=TRUE, sep=",")
  maxclus <- length(unique(class))
  
  mydata <- mydata[ , - ncol(mydata)]
  SW_val <- ASW (mydata, maxclus)
  
  print(SW_val)
}