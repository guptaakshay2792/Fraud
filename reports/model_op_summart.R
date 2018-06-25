
output_summary <- function(aks,pred,select_op_cols, op_path,file_name, file_name_summary, file_name_ks){
  aks$COUNT <- 1
  aks$pred_rank <- ave(aks[,pred], FUN = function(x) rank(-x, ties.method = "first"))
  Percentile <- quantile(aks$pred_rank,probs= seq(0,1,by=0.1))
  aks<-aks[order(aks$pred_rank),]
  aks$Percentile<-cut(aks$pred_rank,breaks = Percentile,labels = FALSE,include.lowest = TRUE)
  write.csv(aks[,select_op_cols],paste0(op_path,file_name,".csv"))
  scoring_buld <- aggregate(cbind(
    COUNT,
    y_auto1
  ) ~ Percentile, aks, sum)
  
  
  write.csv(scoring_buld,paste0(op_path,file_name_summary,".csv"))    
  
  
  scoring_buld$non_event <- scoring_buld$COUNT-scoring_buld$y_auto1
  scoring_buld$cumm_per_event <- cumsum(scoring_buld$y_auto1)/sum(scoring_buld$y_auto1)
  scoring_buld$cumm_per_non_event <- cumsum(scoring_buld$non_event)/sum(scoring_buld$non_event)
  scoring_buld$ks <- scoring_buld$cumm_per_event-scoring_buld$cumm_per_non_event
  write.csv(scoring_buld,paste0(op_path,file_name_ks,".csv"))    
}