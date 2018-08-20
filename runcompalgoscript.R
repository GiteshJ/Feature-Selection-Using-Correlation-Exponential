runcompalgoscript <- function(classif_flag)
{
  Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre1.8.0_111')
  
  library(rJava)
  library(xlsxjars)
  library(xlsx) 
  library(FSelector)
  library(GA)
  library(entropy)
  library(cluster)
  library(caret)
  source("main.R")
  source("CFS.R")
  source("mRMR.R")
  source("Laplacian.R")
  source("PFA.R")
  
  rdf <- 0
  
  filenames <- list.files("data", pattern = "csv")
  
  for (i in 1:length(filenames)){
    datafilename <- filenames[[i]]
    print(datafilename)
    filename <- paste0("data/", datafilename)
    mydata <- read.table(filename, header=TRUE, sep=",")
    mydata <- na.omit(mydata)
    
    if (classif_flag == 0){
      #print(paste("This analysis is done on data set : ", filename, " for classification"))
      cdf <- data.frame(filenames[[i]])
      cdf <- cbind(cdf, runCFS (mydata))

      names(cdf) <- c("Dataset", "CFS_Accuracy", "mRMR_Accuracy")
      if(i == 1){
        rdf <- cdf
        names(rdf) <- c("Dataset", "CFS_Accuracy", "mRMR_Accuracy")
      }
      else {
        rdf <- rbind(rdf, cdf)
      }
      #print(rdf)
    }
    
    if (classif_flag == 1){
      #print(paste("This analysis is done on data set : ", filename, " for clustering"))
      cdf <- data.frame(filenames[[i]])
      cdf <- cbind(cdf, runPFA (mydata))
      #print(cdf)
      cdf <- cbind(cdf, runLaplacian (mydata))
      #print(cdf)
      

      names(cdf) <- c("Dataset", "Laplacian_Purity", "PFA_Purity")
      
      if(i == 1){
        rdf <- cdf
        names(rdf) <- c("Dataset", "Laplacian_Purity", "PFA_Purity")
      }
      else {
        rdf <- rbind(rdf, cdf)
      }
      #print(rdf)
    }
  }
  
  outputfilename <- paste0("output/Output_compalgo_", format(Sys.time(), "%b-%d-%Y_%H-%M"), ".xlsx")
  write.xlsx(x = rdf, file = outputfilename, sheetName = "Comp Algo Output", row.names = FALSE)

}