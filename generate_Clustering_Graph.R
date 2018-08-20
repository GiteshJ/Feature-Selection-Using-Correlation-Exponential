generate_Clustering_Graph <- function(csvname, inputdata, maxclus, alpha, beta)
{
  library(caret)
  library(klaR)
  library("e1071")
  source("processWeaklyAssFeats.R")
  source("processStronglyAssFeats.R")
  source("clustermodelnevaluate.R")
  
  countdatawoclassfield <- ncol(inputdata)
  subset_feat1_accuracy <- 0
  subset_feat2_accuracy <- 0
  tot_subset_feat1_count <- 0
  tot_subset_feat2_count <- 0

  cord <- abs(cor(inputdata))
  for (i in 1:ncol(cord)) {cord[i,i]=0}
  cord1 <- apply(cord,c(1,2),myfunc<- function(x){return(ifelse(x>=alpha,1,0))})
  s<-colSums(cord1)
  z<-which(s==0)
  cordadj<-cord1
  if(length(z)>0) {cordadj<-cord1[-z,-z]}
  
  g1 <- graph.adjacency(cordadj, mode="undirected", weighted = TRUE, diag = FALSE)
  
  strongass <- as.vector(V(g1)$name)

  all_feat_accuracy <- clustermodelnevaluate(inputdata, maxclus)

  if (length(strongass) == 0 ){
    featsubset_weak <- processWeaklyAssFeats (inputdata, beta, 1)
    
    subset_feat1_accuracy <- clustermodelnevaluate(inputdata[featsubset_weak], maxclus)
    
    tot_subset_feat1_count <- length(featsubset_weak)
  }
  
  if (length(strongass) != 0 ){
    if (length(strongass) == countdatawoclassfield){
      featsubset_strong_vertcover <- processStronglyAssFeats (cordadj, "vert")
      featsubset_strong_indset <- processStronglyAssFeats (cordadj, "indset")

      subset_feat1_accuracy <- clustermodelnevaluate(inputdata[featsubset_strong_vertcover], maxclus)

      subset_feat2_accuracy <- clustermodelnevaluate(inputdata[featsubset_strong_indset], maxclus)

      tot_subset_feat1_count <- length(featsubset_strong_vertcover)
      tot_subset_feat2_count <- length(featsubset_strong_indset)
    }
    
    if (length(strongass) != countdatawoclassfield){
      featsubset_strong_vertcover <- processStronglyAssFeats (cordadj, "vert")
      featsubset_strong_indset <- processStronglyAssFeats (cordadj, "indset")
      
      weakass <- setdiff(names(inputdata), strongass)
      featsubset_weak <- processWeaklyAssFeats(inputdata[weakass], beta, 1)

      feat_overall_vertcover <- c(featsubset_strong_vertcover, featsubset_weak)
      subset_feat1_accuracy <- clustermodelnevaluate(inputdata[feat_overall_vertcover], maxclus)

      feat_overall_indset <- c(featsubset_strong_indset, featsubset_weak)
      subset_feat2_accuracy <- clustermodelnevaluate(inputdata[feat_overall_indset], maxclus)

      tot_subset_feat1_count <- length(feat_overall_vertcover)
      tot_subset_feat2_count <- length(feat_overall_indset)
    }
  }

  df <- data.frame(countdatawoclassfield, all_feat_accuracy, tot_subset_feat1_count, subset_feat1_accuracy, tot_subset_feat2_count, subset_feat2_accuracy)
  print(df)
  return(df)

}

