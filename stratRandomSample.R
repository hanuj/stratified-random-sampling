stratRandomSample<-function(dataframe,columnName,size){
  
        if(size>1) {stop("size has to be less than or equal to 1")}

  dataframe<-as.data.frame(dataframe)
  
  l=length(dataframe[,columnName])
  
  dataframe[,columnName]<-as.factor(dataframe[,columnName])
  
  a<-levels(dataframe[,columnName])
  
  a_size<-length(a)
  
       if(a_size==1) {stop("selected column has only 1 level")}
  
  factor_sizes<-data.frame()
  
       for (i in 1:a_size){
                            factor_sizes[i,"name"]<-a[i]
                              
                            factor_sizes[i,"num"]<-length(dataframe[which(dataframe[,columnName]==a[i]),columnName])
                           }
  
       for (i in 1:a_size){factor_sizes[i,"relative"]<-round(factor_sizes[i,"num"]/sum(factor_sizes[,"num"]),4)}
  
  factor_sizes<-factor_sizes[order(factor_sizes[,"num"]),]
  print("Original data")
  print(factor_sizes)
  
  data_out<-data.frame() 
  
       for (i in 1:a_size){
         
         b<-sample(x=factor_sizes[i,2],size=factor_sizes[i,2]*size)
         
         c<-dataframe[which(dataframe[,columnName]==factor_sizes[i,1]),]
         
         data_out<-rbind(data_out,c[b,])
         
         }                         
  
  return(data_out)
}
