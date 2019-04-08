runTP<-function() {
  setwd("~/Documents/Models/Gilad/TDPAIR")
  source('~/Documents/Models/Gilad/TDPAIR/TP.R')
    
  f_list<-combn(dir("./extdata"),2)
  r_df<-data.frame()
  j<-1
  
  for (i in 1:dim(f_list)[2]) {
    tp_data<-pair_trade(paste("./extdata/",f_list[1,i],sep=""),paste("./extdata/",f_list[2,i],sep=""))
    if (length(tp_data) > 0) {
      if (j==1){
        r_df<-tp_data
      } else {
        r_df[j,]<-tp_data
        j=j+1        
      }
    }
  }
  
  write.table(r_df,"pairdata.csv",sep=",",col.names=T,row.names=F)
}
