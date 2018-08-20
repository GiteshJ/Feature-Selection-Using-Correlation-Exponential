main <- function(filename, beta)
{
  library(igraph)
  library(dplyr)
  source("generate_Classification_Graph.R")
  source("generate_Clustering_Graph.R")

  df <- 0

  filename1 <- paste0("data/", filename, ".csv")
  print(filename1)
  mydata <- read.csv(filename1, header=TRUE, sep=",")
  mydata <- na.omit(mydata)

  #print(paste("This analysis is done on data set : ", filename, " with value of alpha  = ", alpha, " and value of beta = ", beta, " for classification"))
  df <- generate_Classification_Graph(filename, mydata, beta)
 
  return(df)
  
}