generate_Classification_Graph <- function (csvname, inputdata, beta)
{
  library (caret)
  library (klaR)
  library (e1071)
  library(expm)
  
  source ("processWeaklyAssFeats.R")
  source ("processStronglyAssFeats.R")
  source ("classifymodelnevaluate.R")

  tot_all_feat_accuracy <- 0
  tot_subset_feat1_accuracy <- 0
  tot_subset_feat2_accuracy <- 0
  tot_subset_feat1_count <- 0
  tot_subset_feat2_count <- 0
  countdatawoclassfield <- ncol(inputdata) - 1
  
  for (l in 1 : 100)
  {
    ## Taking 70% of the sample size as model training data
    
    x1 <- sample (1:10000, 1)
    set.seed (x1)
    split = 0.7
    trainIndex <- createDataPartition (y = inputdata$class, p = split, list = FALSE)
    data_train <- inputdata [trainIndex,]
    data_test <- inputdata [-trainIndex,]
    vclass_train <- data_train [ncol(data_train)]
    vclass_test <- data_test [ncol(data_test)]
    
    cord <- abs(cor(data_train[-ncol(data_train)]))
    #print(cord)
    cord_exp <- expm(cord)
    cord_exp = cord_exp ^ 2
    me = mean(cord_exp)
    cord_exp = cord_exp/me
    
	for (i in 1 : ncol(cord_exp)) {cord_exp[i,i]=-1}
	#print(cord_exp)
   cord_sort= t(apply(cord_exp, 1, sort, decreasing = TRUE))
   
   #print(cord_sort)
  
   count= log(ncol(inputdata), base=2)
	count=ceiling(count)
   #print(count)
   
   logn_sort = cord_sort[,1:count]
	cord1=cord_exp
	#print(cord1)
  	#print(logn_sort)
    
    for (i in 1 : ncol(cord_exp)) {

	top_ln_index=match(logn_sort[i,],cord1[i,])
	#print(top_ln_index)
	cord1[i,top_ln_index]=1
	cord1[i,-top_ln_index]=0

}
	#print(cord1)
    
    
   # cord1 <- apply (cord_exp, c(1,2), myfunc<- function(x){return(ifelse(x>=mh+y,1,0))})
 
    
    s <- colSums (cord1)
    z <- which (s == 0)
    cordadj <- cord1
    
    if (length(z)>0) {cordadj <- cord1 [-z,-z]}
    
    g1 <- graph.adjacency (cordadj, mode="undirected", weighted = TRUE, diag = FALSE)
    
    
   
    strongass <- as.vector (V(g1)$name)

    all_feat_accuracy <- classifymodelnevaluate(data_train, data_test)
    tot_all_feat_accuracy <- tot_all_feat_accuracy + all_feat_accuracy
    
    if (length(strongass) == 0 ){
      featsubset_weak <- processWeaklyAssFeats (data_train, beta, 0)
      tot_subset_feat1_count <- length(featsubset_weak)
      subset_feat1_accuracy <- classifymodelnevaluate(data_train, data_test)
      tot_subset_feat1_accuracy <- tot_subset_feat1_accuracy + subset_feat1_accuracy

    }
    
    if (length(strongass) != 0 ){
      if (length(strongass) == countdatawoclassfield){
        featsubset_strong_vertcover <- processStronglyAssFeats (cordadj, "vert")
        featsubset_strong_indset <- processStronglyAssFeats (cordadj, "indset")

        # Changed code block starts ...
        #featsubset_strong_vertcover <- processWeaklyAssFeats(data_train[featsubset_strong_vertcover], beta, 0)
        #featsubset_strong_indset <- processWeaklyAssFeats(data_train[featsubset_strong_indset], beta, 0)
        # Changed code block ends ...
 
        tot_subset_feat1_count <- length(featsubset_strong_vertcover)
        tot_subset_feat2_count <- length(featsubset_strong_indset)       
        
        subset_feat1_accuracy <- classifymodelnevaluate(cbind(data_train[featsubset_strong_vertcover], vclass_train), cbind(data_test[featsubset_strong_vertcover], vclass_test))
        tot_subset_feat1_accuracy <- tot_subset_feat1_accuracy + subset_feat1_accuracy
        
        subset_feat2_accuracy <- classifymodelnevaluate(cbind(data_train[featsubset_strong_indset], vclass_train), cbind(data_test[featsubset_strong_indset], vclass_test))
        tot_subset_feat2_accuracy <- tot_subset_feat2_accuracy + subset_feat2_accuracy
      }
      
      if (length(strongass) != countdatawoclassfield){
        featsubset_strong_vertcover <- processStronglyAssFeats (cordadj, "vert")
        featsubset_strong_indset <- processStronglyAssFeats (cordadj, "indset")

        #weakass <- setdiff(names(data_train[-ncol(data_train)]), strongass)
        weakass <- setdiff(names(data_train), strongass)
        
        # Original code block starts ...
        
        
        featsubset_weak <- processWeaklyAssFeats(data_train[weakass], beta, 0)

        feat_overall_vertcover <- c(featsubset_strong_vertcover, featsubset_weak)
        feat_overall_indset <- c(featsubset_strong_indset, featsubset_weak)
        
        
        # Original code block ends ...
        
        # Changed code block starts ...
        '
        feat_overall_vertcover <- cbind(weakass, featsubset_strong_vertcover)
        feat_overall_vertcover <- processWeaklyAssFeats(data_train[feat_overall_vertcover], beta, 0)
        
        feat_overall_indset <- cbind(weakass, featsubset_strong_indset)
        feat_overall_indset <- processWeaklyAssFeats(data_train[feat_overall_indset], beta, 0)
        '
        # Changed code block ends ...
        
        tot_subset_feat1_count <- length(feat_overall_vertcover)
        tot_subset_feat2_count <- length(feat_overall_indset)
        
        subset_feat1_accuracy <- classifymodelnevaluate(cbind(data_train[feat_overall_vertcover], vclass_train), cbind(data_test[feat_overall_vertcover], vclass_test))
        tot_subset_feat1_accuracy <- tot_subset_feat1_accuracy + subset_feat1_accuracy
        
        subset_feat2_accuracy <- classifymodelnevaluate(cbind(data_train[feat_overall_indset], vclass_train), cbind(data_test[feat_overall_indset], vclass_test))
        tot_subset_feat2_accuracy <- tot_subset_feat2_accuracy + subset_feat2_accuracy
      }
    }
  }
  
  fname = paste0("data/", csvname, ".png")
  
  png(filename=fname)
  
  plot(g1, vertex.label.color="black")
  dev.off()
  
  df <- data.frame(countdatawoclassfield, tot_all_feat_accuracy/100, tot_subset_feat1_count, tot_subset_feat1_accuracy/100, tot_subset_feat2_count, tot_subset_feat2_accuracy/100)
  print(df)
  return(df)
}