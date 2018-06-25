library(reshape2)
library(RODBC)

dbhandle <- odbcDriverConnect('driver={ODBC Driver 13 for SQL Server};
                              server=bflazdw-dev.database.windows.net;
                              database=BFL_EDW;uid=Bridgei2i_user;
                              pwd=Bajaj@123')


ks_calc_prob_decile <- function(d2, cuts, y){
  d2$COUNT <- 1
  d2<-d2[order(d2$pred),]
  d2$Decile<-cut(-d2$pred,breaks = -cuts,labels = FALSE,include.lowest = TRUE, right = FALSE)
  scoring_buld <- aggregate(cbind(
    COUNT,
    d2[,y]
  ) ~ Decile, d2, sum)
  colnames(scoring_buld)[3]<-y
  pred_min <- aggregate(pred~ Decile, d2, min)
  colnames(pred_min)[2]<-"pred_min"
  pred_max <- aggregate(pred~ Decile, d2, max)
  colnames(pred_max)[2]<-"pred_max"
  scoring_buld$non_event <- scoring_buld$COUNT-scoring_buld[,y]
  scoring_buld$cumm_per_event <- cumsum(scoring_buld[,y])/sum(scoring_buld[,y])
  scoring_buld$cumm_per_non_event <- cumsum(scoring_buld$non_event)/sum(scoring_buld$non_event)
  scoring_buld$ks <- scoring_buld$cumm_per_event-scoring_buld$cumm_per_non_event
  scoring_buld$event_rate <- scoring_buld[,y]/scoring_buld$COUNT
  scoring_buld <- merge(scoring_buld, pred_min, by = "Decile", all=TRUE)
  scoring_buld <- merge(scoring_buld, pred_max, by = "Decile", all=TRUE)
  return (list(scoring_buld,d2))
}

prob_cuts_base <- function(d1, prd){
  d1$COUNT <- 1
  d1$pred_rank <- ave(d1[,prd], FUN = function(x) rank(-x, ties.method = "first"))
  breaks_prob_dec <- quantile(d1$pred_rank,probs= seq(0,1,by=0.1))
  d1$Decile<-cut(d1$pred_rank,breaks = breaks_prob_dec,labels = FALSE,include.lowest = TRUE)
  breaks_prob_dec1 <- aggregate(d1[,prd]~Decile,d1, min)[,2]
  breaks_prob_dec2 <- append(breaks_prob_dec1,1)
  return (breaks_prob_dec2)
}

##############y2############################
read_path <- "Y:/RPL_Vintage/model/Iteration_y2_4/"
write_path <- "Y:/RPL_Vintage/ks_and_strategy/"

d1 <- read.csv(paste0(read_path,"build_9__400.csv"))

breaks_prob_dec2 <- prob_cuts_base(d1, "pred")

mar_y2 <- ks_calc_prob_decile(d1, breaks_prob_dec2, "y2_3m")
ks_mar_y2 <- mar_y2[[1]]
write.csv(ks_mar_y2,paste0(write_path,"ks_mar_pred_cutoffs_y2.csv"))
mar_tagging_y2 <- mar_y2[[2]]
write.csv(mar_tagging_y2,paste0(write_path,"mar_y2_tagging.csv"))


d2 <- read.csv(paste0(read_path,"Holdout_9__400.csv"))
jul_y2 <- ks_calc_prob_decile(d2, breaks_prob_dec2, "y2_3m")
ks_jul_y2 <- jul_y2[[1]]
write.csv(ks_jul_y2,paste0(write_path,"ks_jul_pred_cutoffs_y2.csv"))
jul_tagging_y2 <- jul_y2[[2]]
write.csv(jul_tagging_y2,paste0(write_path,"jul_y2_tagging.csv"))


##############y1############################
read_path <- "Y:/RPL_Vintage/model/y1_iteration_31_new_data_oct_dev/"
write_path <- "Y:/RPL_Vintage/ks_and_strategy/"

d1 <- read.csv(paste0(read_path,"Holdout_oct_3__400.csv"))

breaks_prob_dec2 <- prob_cuts_base(d1, "pred")

oct_y1 <- ks_calc_prob_decile(d1, breaks_prob_dec2, "y1_3m")
ks_oct_y1 <- oct_y1[[1]]
write.csv(ks_oct_y1,paste0(write_path,"ks_oct_pred_cutoffs_y1.csv"))
oct_tagging_y1 <- oct_y1[[2]]
write.csv(oct_tagging_y1,paste0(write_path,"oct_y1_tagging.csv"))


d2 <- read.csv(paste0(read_path,"Holdout_mar_with_overlap3__400.csv"))
mar_y1 <- ks_calc_prob_decile(d2, breaks_prob_dec2, "y1_3m")
ks_mar_y1 <- mar_y1[[1]]
write.csv(ks_mar_y1,paste0(write_path,"ks_mar_pred_cutoffs_y1.csv"))
mar_tagging_y1 <- mar_y1[[2]]
write.csv(mar_tagging_y1,paste0(write_path,"mar_y1_tagging_y1.csv"))

d3 <- read.csv(paste0(read_path,"Holdout_jan_with_overlap3__400.csv"))
jan_y1 <- ks_calc_prob_decile(d3, breaks_prob_dec2, "y1_3m")
ks_jan_y1 <- jan_y1[[1]]
write.csv(ks_jan_y1,paste0(write_path,"ks_jan_pred_cutoffs_y1.csv"))
jan_tagging_y1 <- jan_y1[[2]]
write.csv(jan_tagging_y1,paste0(write_path,"jan_y1_tagging.csv"))



###################Strategy################

colnames(mar_tagging_y2)<-c("X_y2", "CUSTOMER_ID", "Percentile_y2", "y2_3m", "pred_y2", "HIT_FLAG", "COUNT_y2","Decile_y2")
colnames(mar_tagging_y1)<-c("X_y1", "CUSTOMER_ID", "Percentile_y1", "y1_3m", "pred_y1", "COUNT_y1","Decile_y1")

strategy_input <- merge(mar_tagging_y2, mar_tagging_y1, by= "CUSTOMER_ID", all=TRUE)
strategy_input$X_y2<-NULL
strategy_input$X_y1<-NULL
strategy_input$Percentile_y2<-NULL
strategy_input$Percentile_y1<-NULL
strategy_input$COUNT_y2<-NULL
strategy_input$COUNT_y1<-NULL
strategy_input <- strategy_input[,c("CUSTOMER_ID","HIT_FLAG", "y2_3m", "pred_y2", "Decile_y2", "y1_3m", "pred_y1","Decile_y1")]
strategy_input$strategy_code <- paste0(strategy_input$Decile_y2, "_", strategy_input$Decile_y1)

write.csv(strategy_input, paste0(write_path,"strategy_code.csv"))


strategy <- function(strategy_input, var, aggr_fun){
  strategy_input$count <- 1
  aggr_fun <- dcast(strategy_input, Decile_y2 ~ Decile_y1, value.var=var, fun.aggregate = aggr_fun, na.rm=T)
  return(aggr_fun)
}
bull<-sqlQuery(dbhandle, 'select * from bridgei2i.RPLVINTAGE_mar_17_SUMM2')
colnames(strategy_input1)
strategy_input1 <- merge(strategy_input, bull, by.x = "CUSTOMER_ID", by.y = "customer_id")
strategy_input1$occupation_category <- ifelse(strategy_input1$occupation_category == "SALARIED", 1,0)
strategy_input1$hit_flag <- ifelse(strategy_input1$hit_flag == 1, 1.0,0)
strategy_input1$rcd_vintage <- as.numeric(strategy_input1$rcd_vintage)
strategy_input1$score <- as.numeric(strategy_input1$score)
var <- c("Current_EMI","first_PL_line","TOT_PL_line_3MON","first_PL_line_y2","TOT_PL_line_3MON_y2","hit_flag","rcd_vintage", "score","occupation_category","count")
for (i in var){
  print(i)
  write.csv(strategy(strategy_input1, var = i, aggr_fun = mean), paste0(write_path,"strategy_output_mean_",i,".csv"))
  write.csv(strategy(strategy_input1, var = i, aggr_fun = median), paste0(write_path,"strategy_output_median_",i,".csv"))
}



#############Percentile level#################

prob_cuts_base <- function(d1, prd){
  d1$COUNT <- 1
  d1$pred_rank <- ave(d1[,prd], FUN = function(x) rank(-x, ties.method = "first"))
  breaks_prob_dec <- quantile(d1$pred_rank,probs= seq(0,1,by=0.01))
  d1$Decile<-cut(d1$pred_rank,breaks = breaks_prob_dec,labels = FALSE,include.lowest = TRUE)
  breaks_prob_dec1 <- aggregate(d1[,prd]~Decile,d1, min)[,2]
  breaks_prob_dec2 <- append(breaks_prob_dec1,1)
  return (breaks_prob_dec2)
}
##############y2############################
path <- "C:/Users/akshay.gupta/Desktop/working files RPL/local/local/y2_selected"
setwd(path)

d1 <- read.csv("build_9__400.csv")

breaks_prob_dec2 <- prob_cuts_base(d1, "pred")

mar_y2 <- ks_calc_prob_decile(d1, breaks_prob_dec2, "y2_3m")
ks_mar_y2 <- mar_y2[[1]]
write.csv(ks_mar_y2,"ks_mar_pred_cutoffs_per.csv")
mar_tagging_y2 <- mar_y2[[2]]
write.csv(mar_tagging_y2,"mar_y2_tagging_per.csv")


d2 <- read.csv("Holdout_9__400.csv")
jul_y2 <- ks_calc_prob_decile(d2, breaks_prob_dec2, "y2_3m")
ks_jul_y2 <- jul_y2[[1]]
write.csv(ks_jul_y2,"ks_jul_pred_cutoffs_per.csv")
jul_tagging_y2 <- jul_y2[[2]]
write.csv(jul_tagging_y2,"jul_y2_tagging_per.csv")


##############y1############################
path <- "C:/Users/akshay.gupta/Desktop/working files RPL/local/local/y1"
setwd(path)

d1 <- read.csv("Holdout_oct_3__400.csv")

breaks_prob_dec2 <- prob_cuts_base(d1, "pred")

oct_y1 <- ks_calc_prob_decile(d1, breaks_prob_dec2, "y1_3m")
ks_oct_y1 <- oct_y1[[1]]
write.csv(ks_oct_y1,"ks_oct_pred_cutoffs_per.csv")
oct_tagging_y1 <- oct_y1[[2]]
write.csv(oct_tagging_y1,"oct_y1_tagging_per.csv")


d2 <- read.csv("Holdout_mar_with_overlap3__400.csv")
mar_y1 <- ks_calc_prob_decile(d2, breaks_prob_dec2, "y1_3m")
ks_mar_y1 <- mar_y1[[1]]
write.csv(ks_mar_y1,"ks_mar_pred_cutoffs_per.csv")
mar_tagging_y1 <- mar_y1[[2]]
write.csv(mar_tagging_y1,"mar_y1_tagging_per.csv")

d3 <- read.csv("Holdout_jan_with_overlap3__400.csv")
jan_y1 <- ks_calc_prob_decile(d3, breaks_prob_dec2, "y1_3m")
ks_jan_y1 <- jan_y1[[1]]
write.csv(ks_jan_y1,"ks_jan_pred_cutoffs_per.csv")
jan_tagging_y1 <- jan_y1[[2]]
write.csv(jan_tagging_y1,"jan_y1_tagging_per.csv")



###################Strategy################

colnames(mar_tagging_y2)<-c("X_y2", "CUSTOMER_ID", "Percentile_y2", "y2_3m", "pred_y2", "HIT_FLAG", "COUNT_y2","Decile_y2")
colnames(mar_tagging_y1)<-c("X_y1", "CUSTOMER_ID", "Percentile_y1", "y1_3m", "pred_y1", "COUNT_y1","Decile_y1")

strategy_input <- merge(mar_tagging_y2, mar_tagging_y1, by= "CUSTOMER_ID", all=TRUE)
strategy_input$X_y2<-NULL
strategy_input$X_y1<-NULL
strategy_input$Percentile_y2<-NULL
strategy_input$Percentile_y1<-NULL
strategy_input$COUNT_y2<-NULL
strategy_input$COUNT_y1<-NULL
strategy_input <- strategy_input[,c("CUSTOMER_ID","HIT_FLAG", "y2_3m", "pred_y2", "Decile_y2", "y1_3m", "pred_y1","Decile_y1")]
strategy_input$strategy_code <- paste0(strategy_input$Decile_y2, "_", strategy_input$Decile_y1)

path <- "C:/Users/akshay.gupta/Desktop/working files RPL/local/local/y2_selected"
setwd(path)

write.csv(strategy_input, "strategy_code_per.csv")

var <- c("Current_EMI","first_PL_line","TOT_PL_line_3MON","first_PL_line_y2","TOT_PL_line_3MON_y2","hit_flag","rcd_vintage", "score","occupation_category")
for (i in var){
  write.csv(strategy(strategy_input, var = i, aggr_fun = mean), paste0("strategy_output_per_mean_",i,".csv"))
  write.csv(strategy(strategy_input, var = i, aggr_fun = median), paste0("strategy_output_per_median_",i,".csv"))
}
