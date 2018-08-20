processStronglyAssFeats <- function(cordadj, vert_indset)
{
  indset <- c()
  vertcover <- c()
  #print(cordadj)
  for (i in 1:nrow(cordadj)){
    if (!(rownames(cordadj)[i] %in% vertcover)){
      indset <- c(indset, rownames(cordadj)[i])
      row <- cordadj[i,]
      for (j in 1:ncol(cordadj)){
        if(row[j] == 1){
          if (!(colnames(cordadj)[j] %in% vertcover)){
            vertcover <- c(vertcover, colnames(cordadj)[j])
          }
        }
      }
    }
  }
  
  if(vert_indset == "vert"){
    return(vertcover)
  }
  if(vert_indset == "indset"){
    return(indset)
  }

}