densesubgraph<-function(data, nm=names(data),avgden=0,iter= 1)
{
  cat('Iteration:-',iter)
  mici<-MICI(data)
  v<-ncol(mici)
  fs<-c()
  avgden<-sum(mici)/(v * (v - 1)/2)
  ewd<-c()
  sv<-c()
  print(avgden)
  for (i in 1:v)
  {
    if (sum(mici[i,])==0)
      ewd[i] = 0
    else
      ewd[i]=sum(mici[i,])/length( which(mici[i,]>0))
  }
  md=sum(ewd)/v
  sv<-which(ewd>md)
  micinew=mici[sv,sv]
  nmold<-nm
  
  if (length(micinew)>1)
  {
    vnew<-ncol(micinew)
    avgdennew<-sum(micinew)/(vnew * (vnew - 1)/2)
    
  }else{
    avgdennew = 0
  }
  if (avgdennew > avgden)
  {
    iter= iter + 1
    densesubgraph(data[,sv],nm[sv] ,avgdennew, iter)
  }else
  {
    print('Found the densest subgraph')
    return(nmold)
  }
  
}

MICI<-function(data)
{
  n<-ncol(data)
  micimat<-matrix(nrow=n,ncol=n)
  for (i in 1:n)
  {
    for (j in 1:n)
    {
      micimat[i,j]=(var(data[i])+ var(data[j])-sqrt((var(data[i] )+var(data[j]))^2-4*var(data[ i])*var(data[j])*(1-cor(data[ i],data[j])^2)))/2
    }
  }
  micimat[is.nan(micimat)==TRUE] = 0
  micimat
}
