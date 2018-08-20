runscriptlean <- function()
{
  Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre1.8.0_111')
  
  library(rJava)
  library(xlsxjars)
  library(xlsx) 
  library(igraph)
  library(dplyr)
  library(FSelector)
  library(GA)
  source("mRMR.R")

  filenames <- list.files("data", pattern = "csv")

  for (i in 1:length(filenames)){
    datafilename <- filenames[[i]]

    print(datafilename)
    main(datafilename, 0.67, 0.8, 0)
    main(datafilename, 0.67, 0.8, 1)
  }
  
}