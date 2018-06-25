
psi <- function(mar, oct){
  mar1<-aggregate(obs_percent~variable+level,mar,sum)
  mar2 <- mar1[mar1$variable!="customer_id",]
  
  oct1<-aggregate(obs_percent~variable+level,oct,sum)
  oct2 <- oct1[oct1$variable!="customer_id",]
  
  
  tmp<-merge(oct2,mar2, by = c("variable", "level"), all.x = T, all.y=T)
  write.csv(tmp, "obs_perc_num.csv")
  tmp$diff <- tmp$obs_percent.x-tmp$obs_percent.y
  tmp$ln <- log(tmp$obs_percent.x/tmp$obs_percent.y)
  tmp$psi <- tmp$diff*tmp$ln
  
  var_psi <- aggregate(psi~variable, tmp, sum)
  return (var_psi)
}

psi_input <- function(mar,selected_cols){
  tmp <- data.frame()
  for(var in selected_cols){
  mar1<-aggregate(cbind(count,y1_3m)~mar[,var],mar,sum)
  colnames(mar1) <- c("level","count","bad")
  mar1$obs_percent <- mar1$count/sum(mar1$count)*100
  mar1$bad_per <- mar1$bad/sum(mar1$bad)*100
  mar1$good_per <- (mar1$count - mar1$bad)/(sum(mar1$count) - sum(mar1$bad))*100
  mar1$variable<-var
  tmp <- rbind(tmp, mar1)
  }
  return (tmp)
}
tmp<-mar1
mar<-mar1
woe_iv <- function(mar){
  mar1<-aggregate(bad_per~variable+level,mar,sum)
  mar2 <- mar1[mar1$variable!="customer_id",]
  
  mar3<-aggregate(good_per~variable+level,mar,sum)
  mar4 <- mar3[mar3$variable!="customer_id",]

  tmp<-merge(mar1,mar4, by = c("variable", "level"), all.x = T, all.y=T)
  
  tmp$diff <- tmp$good_per-tmp$bad_per
  tmp$woe <- log(tmp$good_per/tmp$bad_per)
  tmp$IV <- tmp$diff*tmp$woe
  
  return (tmp)
}


oct <- readRDS("bin_oct.RDS")
selected_cols <- c('BFL_TM_SN_FIRST_LOAN_CD',	'BFL_SEC_UNSEC_LV_RAT',	'BFL_NO_LIVE_LOANS',	'BFL_NO_LIVE_TWL_9M',	'BFL_NO_LIVE_UNSEC',	'BFL_MAX_OS_LIVE_CD',	'BFL_NO_CLSD_CD_6M',	'Count_LiveCDLoanOnus_last_bt_oneyr',	'BFL_BAL_SAMT_CD',	'BFL_RAT_OS_LIVE_UNSEC_to_OS_LIVE',	'BFL_RAT_OS_LIVE_CD_to_OS_LIVE',	'BFL_SUM_SANC_LAST_6M_OVR',	'BFL_RAT_OS_LIVE_PL_to_OS_LIVE',	'BFL_RAT_OS_LIVE_CD_to_SANC_LIVE_CD',	'BFL_RAT_OS_LIVE_PL_to_SANC_LIVE_PL',	'BFL_RAT_OS_LIVE_UNSEC_to_SANC_LIVE_UNSEC',	'BFL_RAT_OS_LIVE_CD_to_SANC_AMT_LIVE',	'BFL_RAT_OS_LIVE_PL_to_SANC_AMT_LIVE',	'BFL_RAT_OS_LIVE_UNSEC_to_SANC_AMT_LIVE',	'NUM_PL_9M',	'AvailableEMICardLimit',	'BFL_RAT_SANC_LIVE_PL_to_SANC_AMT_LIVE',	'NEW_EXP_RAT_9M',	'MAX_UTILIZATION_CC',	'BFL_AVG_INT_PURCH_DAYS_OVR',	'RAT_SANC_PL_to_SANC_UNSEC',	'LAST_CD_MOB',	'MAX_SANC_LIVE_PL',	'TM_SN_LAST_LOAN_CD',	'RAT_SANC_LIVE_PL_to_SANC_UNSEC',	'NEW_EXP_RAT_6M',	'MaxMonthlyEMI_oneyr',	'total_POS_onus_oneyr',	'RAT_SANC_CLOSED_PL_to_SANC_UNSEC',	'BFL_TM_SN_LAST_LOAN_CD',	'Age_at_1st_cc',	'AVG_OS_LIVE_REV',	'AVG_INT_PURCH_DAYS_PL',	'BFL_RAT_SANC_LIVE_CD_to_SANC_AMT_LIVE',	'MAX_SANC_LIVE_CC',	'BFL_AVG_INT_PURCH_DAYS_UNSEC',	'max_POS_onus_EMI_CARD',	'AVG_TENURE_PL',	'MAX_SANC_CD',	'BFL_SUM_SANC_LAST_3M_CD',	'MAX_OS_LIVE_UNSEC',	'BFL_MAX_SANC_LIVE_UNSEC',	'BFL_TOTAL_SANC_UNSEC',	'TM_SN_LAST_LIVE_PL',	'TOTAL_SANC_CLOSED_UNSEC',	'SUM_SANC_LAST_9M_OVR',	'MAX_SANC_UNSEC',	'SUM_SANC_LAST_9M_UNSEC',	'TotalAmtFin_oneyr',	'AVG_OS_LIVE_CD',	'AvgAmtFin_oneyr',	'BFL_RAT_SANC_PL_to_SANC_UNSEC',	'TM_SN_LAST_LIVE_UNSEC',	'BFL_SUM_SANC_LAST_9M_UNSEC',	'BFL_TM_SN_FIRST_LIVE_CD',	'max_POS_onus',	'min_POS_onus_twoyr',	'BFL_NO_LV_LOAN_TKN_6M',	'min_POS_onus_oneyr',	'RAT_SANC_CLOSED_UNSEC_to_SANC_UNSEC',	'MAX_SANC_CLOSED_UNSEC',	'BFL_SUM_SANC_LAST_12M_UNSEC',	'min_POS_onus',	'LV_LAST_9M_TOT_LV_RAT',	'BFL_RAT_SANC_LIVE_UNSEC_to_SANC_AMT_LIVE',	'BFL_SUM_SANC_LAST_12M_CD',	'Vintage_on_EMI_Card',	'NUM_UNSEC_6M',	'BFL_TM_SN_LAST_LIVE_OVR',	'BFL_MAX_SANC_LIVE_CD',	'BFL_MAX_SANC_CLOSED_CD',	'BFL_RAT_OS_LIVE_to_SANC_AMT_LIVE',	'Count_LiveCDLoanOnus_twoyr',	'BFL_AVG_SANC_CD',	'Count_RCD_Trans',	'TM_SN_LAST_CLS_PL',	'RCD_VINTAGE',	'Count_AC_oneyr',	'TotalAmtFin_last_bt_oneyr',	'BFL_MAX_SANC_CD',	'BFL_NEW_EXP_RAT_9M',	'NO_CLSD_PL',	'AvgAmtFin_last_bt_oneyr',	'BFL_TM_SN_LAST_CLS_UNSEC',	'BFL_NEW_EXP_RAT_12M',	'BFL_TM_SN_FIRST_CLS_UNSEC',	'AVG_SANC_SEC',	'BFL_MAX_OS_LIVE',	'BFL_MAX_TENURE_OVR',	'NUM_CD_9M',	'BFL_NO_LIVE_PL_9M',	'BFL_NO_LIVE_UNSEC_9M',	'BFL_NO_LOANS_3M',	'BFL_CLSD_TOT_RATIO',	'BFL_NUM_PL',	'BFL_NUM_UNSEC',	'BFL_LV_LAST_9M_TOT_LV_RAT',	'BFL_NO_LOANS_9M',	'BFL_NO_LIVE_GL_6M',	'BFL_NUM_UNSEC_12M',	'BFL_NO_LIVE_PL',	'BFL_NO_LIVE_SEC_9M',	'BFL_NO_LV_UNSEC_TKN_12M',	'BFL_NO_CLSD_UNSEC_12M',	'BFL_NO_CLSD_LOANS_9M',	'DistinctCategoryDesc_last_bt_oneyr',	'BFL_NO_LIVE_AL_9M',	'y1_3m')
oct$count <-1
oct1 <- psi_input(oct, selected_cols)



mar <- readRDS("bin_mar.RDS")
selected_cols <- c(	'BFL_TM_SN_FIRST_LOAN_CD',	'BFL_SEC_UNSEC_LV_RAT',	'BFL_NO_LIVE_LOANS',	'BFL_NO_LIVE_TWL_9M',	'BFL_NO_LIVE_UNSEC',	'BFL_MAX_OS_LIVE_CD',	'BFL_NO_CLSD_CD_6M',	'Count_LiveCDLoanOnus_last_bt_oneyr',	'BFL_BAL_SAMT_CD',	'BFL_RAT_OS_LIVE_UNSEC_to_OS_LIVE',	'BFL_RAT_OS_LIVE_CD_to_OS_LIVE',	'BFL_SUM_SANC_LAST_6M_OVR',	'BFL_RAT_OS_LIVE_PL_to_OS_LIVE',	'BFL_RAT_OS_LIVE_CD_to_SANC_LIVE_CD',	'BFL_RAT_OS_LIVE_PL_to_SANC_LIVE_PL',	'BFL_RAT_OS_LIVE_UNSEC_to_SANC_LIVE_UNSEC',	'BFL_RAT_OS_LIVE_CD_to_SANC_AMT_LIVE',	'BFL_RAT_OS_LIVE_PL_to_SANC_AMT_LIVE',	'BFL_RAT_OS_LIVE_UNSEC_to_SANC_AMT_LIVE',	'NUM_PL_9M',	'AvailableEMICardLimit',	'BFL_RAT_SANC_LIVE_PL_to_SANC_AMT_LIVE',	'NEW_EXP_RAT_9M',	'MAX_UTILIZATION_CC',	'BFL_AVG_INT_PURCH_DAYS_OVR',	'RAT_SANC_PL_to_SANC_UNSEC',	'LAST_CD_MOB',	'MAX_SANC_LIVE_PL',	'TM_SN_LAST_LOAN_CD',	'RAT_SANC_LIVE_PL_to_SANC_UNSEC',	'NEW_EXP_RAT_6M',	'MaxMonthlyEMI_oneyr',	'total_POS_onus_oneyr',	'RAT_SANC_CLOSED_PL_to_SANC_UNSEC',	'BFL_TM_SN_LAST_LOAN_CD',	'Age_at_1st_cc',	'AVG_OS_LIVE_REV',	'AVG_INT_PURCH_DAYS_PL',	'BFL_RAT_SANC_LIVE_CD_to_SANC_AMT_LIVE',	'MAX_SANC_LIVE_CC',	'BFL_AVG_INT_PURCH_DAYS_UNSEC',	'max_POS_onus_EMI_CARD',	'AVG_TENURE_PL',	'MAX_SANC_CD',	'BFL_SUM_SANC_LAST_3M_CD',	'MAX_OS_LIVE_UNSEC',	'BFL_MAX_SANC_LIVE_UNSEC',	'BFL_TOTAL_SANC_UNSEC',	'TM_SN_LAST_LIVE_PL',	'TOTAL_SANC_CLOSED_UNSEC',	'SUM_SANC_LAST_9M_OVR',	'MAX_SANC_UNSEC',	'SUM_SANC_LAST_9M_UNSEC',	'TotalAmtFin_oneyr',	'AVG_OS_LIVE_CD',	'AvgAmtFin_oneyr',	'BFL_RAT_SANC_PL_to_SANC_UNSEC',	'TM_SN_LAST_LIVE_UNSEC',	'BFL_SUM_SANC_LAST_9M_UNSEC',	'BFL_TM_SN_FIRST_LIVE_CD',	'max_POS_onus',	'min_POS_onus_twoyr',	'BFL_NO_LV_LOAN_TKN_6M',	'min_POS_onus_oneyr',	'RAT_SANC_CLOSED_UNSEC_to_SANC_UNSEC',	'MAX_SANC_CLOSED_UNSEC',	'BFL_SUM_SANC_LAST_12M_UNSEC',	'min_POS_onus',	'LV_LAST_9M_TOT_LV_RAT',	'BFL_RAT_SANC_LIVE_UNSEC_to_SANC_AMT_LIVE',	'BFL_SUM_SANC_LAST_12M_CD',	'Vintage_on_EMI_Card',	'NUM_UNSEC_6M',	'BFL_TM_SN_LAST_LIVE_OVR',	'BFL_MAX_SANC_LIVE_CD',	'BFL_MAX_SANC_CLOSED_CD',	'BFL_RAT_OS_LIVE_to_SANC_AMT_LIVE',	'Count_LiveCDLoanOnus_twoyr',	'BFL_AVG_SANC_CD',	'Count_RCD_Trans',	'TM_SN_LAST_CLS_PL',	'RCD_VINTAGE',	'Count_AC_oneyr',	'TotalAmtFin_last_bt_oneyr',	'BFL_MAX_SANC_CD',	'BFL_NEW_EXP_RAT_9M',	'NO_CLSD_PL',	'AvgAmtFin_last_bt_oneyr',	'BFL_TM_SN_LAST_CLS_UNSEC',	'BFL_NEW_EXP_RAT_12M',	'BFL_TM_SN_FIRST_CLS_UNSEC',	'AVG_SANC_SEC',	'BFL_MAX_OS_LIVE',	'BFL_MAX_TENURE_OVR',	'NUM_CD_9M',	'BFL_NO_LIVE_PL_9M',	'BFL_NO_LIVE_UNSEC_9M',	'BFL_NO_LOANS_3M',	'BFL_CLSD_TOT_RATIO',	'BFL_NUM_PL',	'BFL_NUM_UNSEC',	'BFL_LV_LAST_9M_TOT_LV_RAT',	'BFL_NO_LOANS_9M',	'BFL_NO_LIVE_GL_6M',	'BFL_NUM_UNSEC_12M',	'BFL_NO_LIVE_PL',	'BFL_NO_LIVE_SEC_9M',	'BFL_NO_LV_UNSEC_TKN_12M',	'BFL_NO_CLSD_UNSEC_12M',	'BFL_NO_CLSD_LOANS_9M',	'DistinctCategoryDesc_last_bt_oneyr',	'BFL_NO_LIVE_AL_9M',	'y1_3m')
mar$count <-1
mar1 <- psi_input(mar, selected_cols)

mar2 <- psi(oct1, mar1)

write.csv(mar2, "psi_oct_mar.csv")
