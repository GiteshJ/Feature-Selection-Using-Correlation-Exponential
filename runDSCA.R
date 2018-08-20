runDSCA <- function()
{
  source("implementDSCA.R")
  
  filenames <- list.files("data", pattern = "csv")
  
  for (i in 1:length(filenames)){
    datafilename <- filenames[[i]]
    datafilename <- paste0("data/", datafilename)

    starttime <- Sys.time()

    print(datafilename)
    featsubsetlist <- implementDSCA(datafilename)
    
    endtime <- Sys.time()
    print("Total execution time : ")
    print(starttime - endtime)
    
    #print(featsubsetlist)
    #print(cum_accuracy/100)
  }
}

rundensesubgraph <- function()
{
  source("densesubgraph.R")
  source("clustermodelnevaluate.R")
  
  filenames <- list.files("data-review", pattern = "csv")
  
  for (i in 1:length(filenames)){
    datafilename <- paste0("data-review/", filenames[[i]])
    csvdata <- read.table(datafilename, header=TRUE, sep=",")
    mydata <- csvdata[,-ncol(csvdata)]
    class <- csvdata[ , ncol(csvdata)]
    maxclus <- length(unique(class))
    
    featsubset <- densesubgraph(mydata)
    
    SW <- clustermodelnevaluate(mydata[featsubset], maxclus)
    
    print(datafilename)
    print(featsubset)
    #print(SW)
  }
  
  
}