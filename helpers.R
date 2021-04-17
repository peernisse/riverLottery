

#Calculate probabilities

clp<-function(df,n,N,m){
  
  #Defs
  #df = data frame
    #n = number of applicants in your group who applied for the same date
    #N = historical number of applications for that date
    #m = number of launches available on that date   
 
   # df<-zzz
   # N<-"TOTAL_APPLICATIONS"
   # n<-"GROUP_N"
   # m<-"AVAILABLE"
  
    ######
  rv<-vector()
  
  
  for(i in 1:nrow(df)){
    #i<-1
    eval<-df[i,]
    v<-vector()
    
    for(j in 1:eval[[m]]){
      #j<-3
      
      p<-(eval[[N]]-eval[[n]])/eval[[N]]
      v[j]<-p
      eval[[N]]<-eval[[N]]-1
    
   }
    
    rv[i]<-(1-prod(v))
    
    
    
  }
  
  
 rv
  
  
  
}



#Normalize data to between 0 and 1------------

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


#Randomize and create train and test data sets------------------

mkSets<-function(df,seed=42){
  #Creates a list of 2 1 train df and 1 test df
  #Testing
  #df<-df_1
  #seed<-42
  
  ####
  
  set.seed(seed)
  
  nRows<-1:nrow(df)
  
  train_rows <- sample(nRows,0.6*length(nRows))
  
  test_rows <- setdiff(nRows,train_rows)
  
  stopifnot(intersect(test_rows,train_rows) == 0)
  
  train<-df[train_rows,] %>% as_tibble(.)
  test<-df[test_rows,]  %>% as_tibble(.)
  
  lsOut<-list()
  lsOut[["train"]]<-train
  lsOut[['test']]<-test
  
  return(lsOut)
  
}#end mkSets


















