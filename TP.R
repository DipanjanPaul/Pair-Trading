pair_trade<-function(file1,file2) {
##  setwd("/Users/dipanjanpaul/Documents/Models/Gilad/C S W 1 min 2010-to-day")
  r_df<-data.frame()
  r_df_temp<-data.frame()
  r_df_f<-data.frame()
  
  data1<-read.csv(file1,strip.white=TRUE,header=F)
  data2<-read.csv(file2,strip.white=TRUE,header=F)
  
  names(data1)<-c("X","High","Low","Open","Close","Volume")
  names(data2)<-c("X","High","Low","Open","Close","Volume")

  data_all<-merge(data1,data2,by.x='X',by.y='X')
  rm(data1,data2)
  
##  data1$X<-as.Date(data1$X)
  
##  cor<-array()
##  cor[1]<-cor(data_all$High.x,data_all$High.y)
##  cor[2]<-cor(data_all$Low.x,data_all$Low.y)l
##  cor[3]<-cor(data_all$Open.x,data_all$Open.y)
  
  cor_cl<-cor(data_all$Close.x,data_all$Close.y)
  cor_vol<-cor(data_all$Volume.x,data_all$Volume.y)

  if ((mean(c(cor_cl,cor_vol))) > 0.75) {
    data_all$Close_Dif<-data_all$Close.x-data_all$Close.y
    data_all$X_dt<-as.POSIXct(data_all$X)
    Close_otlr<-which(abs(data_all$Close_Dif - mean(data_all$Close_Dif)) > 1.5*sd(data_all$Close_Dif))
    Close_otlr_seq<-rle(unlist(sapply((1:length(Close_otlr)), function(a) ((Close_otlr[a] - Close_otlr[a-1]))))==1)
    Close_otlr_beg<-c(0, cumsum(Close_otlr_seq$lengths))[which(Close_otlr_seq$values)] + 1 
    Close_otlr_end<-cumsum(Close_otlr_seq$lengths)[which(Close_otlr_seq$values)]
    
    r_df<-data_all[Close_otlr[Close_otlr_beg],][,]
    r_df[,14:26]<-data_all[Close_otlr[Close_otlr_end],][,]
##    r_df$X.time_diff <- difftime(as.POSIXct(r_df$X.1),as.POSIXct(r_df$X),units="min")
    r_df$X.time_diff <- difftime((r_df$X_dt.1),(r_df$X_dt),units="min")

    for (i in 1:(nrow(r_df)-1)) {
      r_df[i,c("drawdown")]<-difftime((data_all[intersect(which(((data_all$X_dt) > (r_df$X_dt.1[i])) & 
                                                          ((data_all$X_dt) < (r_df$X_dt[i+1]))), 
                                         which(abs(data_all$Close_Dif - mean(data_all$Close_Dif)) < 
                                                 .75*sd(data_all$Close_Dif))),])[1,13],r_df$X_dt.1[i],units="min")
      
    }



    r_df$X.Filename <- file1
    r_df$Y.Filename <- file2
    
    r_df_temp<-r_df[c((which(r_df$X.time_diff > 0.9*max(r_df$X.time_diff))),(which(as.Date(r_df$X.1) == max(as.Date(data_all$X))))),]
    
##    r_df_temp<-r_df[(which(r_df$X.time_diff > 0.9*max(r_df$X.time_diff))),]
    
##    if (row.names(r_df_temp)[(dim(r_df_temp)[1])] == row.names(data_all)[(dim(data_all)[1])]) {
##      r_df_f = r_df_temp[(dim(r_df_temp)[1]),]
##    }
        
##    write.table(r_df_f,"C1S1_Rep.csv",sep=",",col.names=T,row.names=F)
  }

  return(r_df_temp)
}