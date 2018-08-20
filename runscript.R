runscript <- function()
{
  Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre1.8.0_111')
  
  library(rJava)
  library(xlsxjars)
  library(xlsx) 
  source("main.R")

  rdf <- 0
  sheetname <- 0
  
  filenames <- list.files("data", pattern = "csv")

  for (i in 1:length(filenames)){
    datafilename <- filenames[[i]]
    
    #Block of code for classification ...
    cdf <- data.frame(filenames[[i]])
    cdf <- cbind(cdf, main(datafilename, 0.67, 0.8, 0))
    cdf <- cbind(cdf, main(datafilename, 0.67, 0.7, 0))
    cdf <- cbind(cdf, main(datafilename, 0.67, 0.6, 0))
    cdf <- cbind(cdf, main(datafilename, 0.8, 0.8, 0))
    cdf <- cbind(cdf, main(datafilename, 0.8, 0.7, 0))
    cdf <- cbind(cdf, main(datafilename, 0.8, 0.6, 0))
    cdf <- cbind(cdf, main(datafilename, 0.9, 0.8, 0))
    cdf <- cbind(cdf, main(datafilename, 0.9, 0.7, 0))
    cdf <- cbind(cdf, main(datafilename, 0.9, 0.6, 0))
    sheetname <- "Accuracy_Values"
    

    #Block of code for clustering ...
    'cdf <- data.frame(filenames[[i]])
    cdf <- cbind(cdf, main(datafilename, 0.67, 0.8, 1))
    cdf <- cbind(cdf, main(datafilename, 0.67, 0.7, 1))
    cdf <- cbind(cdf, main(datafilename, 0.67, 0.6, 1))
    cdf <- cbind(cdf, main(datafilename, 0.8, 0.8, 1))
    cdf <- cbind(cdf, main(datafilename, 0.8, 0.7, 1))
    cdf <- cbind(cdf, main(datafilename, 0.8, 0.6, 1))
    cdf <- cbind(cdf, main(datafilename, 0.9, 0.8, 1))
    cdf <- cbind(cdf, main(datafilename, 0.9, 0.7, 1))
    cdf <- cbind(cdf, main(datafilename, 0.9, 0.6, 1))
    sheetname <- "SW_Values"'
    
    #print(cdf)
    #print(dim(cdf))
    
    names(cdf) <- c("Dataset", "Allfeatcount", "Allfeataccuracy", "Alpha1Beta1_1_featcount", "Alpha1Beta1_1_feataccuracy", "Alpha1Beta1_2_featcount", "Alpha1Beta1_2_feataccuracy"
                             , "Allfeatcount", "Allfeataccuracy", "Alpha1Beta2_1_featcount", "Alpha1Beta2_1_feataccuracy", "Alpha1Beta2_2_featcount", "Alpha1Beta2_2_feataccuracy"
                             , "Allfeatcount", "Allfeataccuracy", "Alpha1Beta3_1_featcount", "Alpha1Beta3_1_feataccuracy", "Alpha1Beta3_2_featcount", "Alpha1Beta3_2_feataccuracy"
                             , "Allfeatcount", "Allfeataccuracy", "Alpha2Beta1_1_featcount", "Alpha2Beta1_1_feataccuracy", "Alpha2Beta1_2_featcount", "Alpha2Beta1_2_feataccuracy"
                             , "Allfeatcount", "Allfeataccuracy", "Alpha2Beta2_1_featcount", "Alpha2Beta2_1_feataccuracy", "Alpha2Beta2_2_featcount", "Alpha2Beta2_2_feataccuracy"
                             , "Allfeatcount", "Allfeataccuracy", "Alpha2Beta3_1_featcount", "Alpha2Beta3_1_feataccuracy", "Alpha2Beta3_2_featcount", "Alpha2Beta3_2_feataccuracy"
                             , "Allfeatcount", "Allfeataccuracy", "Alpha3Beta1_1_featcount", "Alpha3Beta1_1_feataccuracy", "Alpha3Beta1_2_featcount", "Alpha3Beta1_2_feataccuracy"
                             , "Allfeatcount", "Allfeataccuracy", "Alpha3Beta2_1_featcount", "Alpha3Beta2_1_feataccuracy", "Alpha3Beta2_2_featcount", "Alpha3Beta2_2_feataccuracy"
                             , "Allfeatcount", "Allfeataccuracy", "Alpha3Beta3_1_featcount", "Alpha3Beta3_1_feataccuracy", "Alpha3Beta3_2_featcount", "Alpha3Beta3_2_feataccuracy")
    

    if(i == 1){
      rdf <- cdf
      names(rdf) <- c("Dataset", "Allfeatcount", "Allfeataccuracy", "Alpha1Beta1_1_featcount", "Alpha1Beta1_1_feataccuracy", "Alpha1Beta1_2_featcount", "Alpha1Beta1_2_feataccuracy"
                      , "Allfeatcount", "Allfeataccuracy", "Alpha1Beta2_1_featcount", "Alpha1Beta2_1_feataccuracy", "Alpha1Beta2_2_featcount", "Alpha1Beta2_2_feataccuracy"
                      , "Allfeatcount", "Allfeataccuracy", "Alpha1Beta3_1_featcount", "Alpha1Beta3_1_feataccuracy", "Alpha1Beta3_2_featcount", "Alpha1Beta3_2_feataccuracy"
                      , "Allfeatcount", "Allfeataccuracy", "Alpha2Beta1_1_featcount", "Alpha2Beta1_1_feataccuracy", "Alpha2Beta1_2_featcount", "Alpha2Beta1_2_feataccuracy"
                      , "Allfeatcount", "Allfeataccuracy", "Alpha2Beta2_1_featcount", "Alpha2Beta2_1_feataccuracy", "Alpha2Beta2_2_featcount", "Alpha2Beta2_2_feataccuracy"
                      , "Allfeatcount", "Allfeataccuracy", "Alpha2Beta3_1_featcount", "Alpha2Beta3_1_feataccuracy", "Alpha2Beta3_2_featcount", "Alpha2Beta3_2_feataccuracy"
                      , "Allfeatcount", "Allfeataccuracy", "Alpha3Beta1_1_featcount", "Alpha3Beta1_1_feataccuracy", "Alpha3Beta1_2_featcount", "Alpha3Beta1_2_feataccuracy"
                      , "Allfeatcount", "Allfeataccuracy", "Alpha3Beta2_1_featcount", "Alpha3Beta2_1_feataccuracy", "Alpha3Beta2_2_featcount", "Alpha3Beta2_2_feataccuracy"
                      , "Allfeatcount", "Allfeataccuracy", "Alpha3Beta3_1_featcount", "Alpha3Beta3_1_feataccuracy", "Alpha3Beta3_2_featcount", "Alpha3Beta3_2_feataccuracy")
    }
    else {
      rdf <- rbind(rdf, cdf)
    }
  }
  
  
  outputfilename <- paste0("output/Output_", format(Sys.time(), "%b-%d-%Y_%H-%M"), ".xlsx")
  write.xlsx(x = rdf, file = outputfilename, sheetName = sheetname, row.names = FALSE)

}