tmp <- readRDS("train_bin.RDS")
colnames(tmp)<-toupper(colnames(tmp))  
  
# var_num <- c('AGE_IN_BUREAU_MONTH',	'AVG_INT_PURCH_DAYS_CD',	'AVG_INT_PURCH_DAYS_GL',	'AVG_INT_PURCH_DAYS_OVR',	'AVG_INT_PURCH_DAYS_REV',	'AVG_INT_PURCH_DAYS_SEC',	'AVG_INT_PURCH_DAYS_TWL',	'AVG_INT_PURCH_DAYS_UNSEC',	'AVG_TENURE_OVR',	'AVG_TENURE_PL',	'AVG_TENURE_UNSEC',	'AGE_AT_1ST_CC',	'BAL_SAMT_CD',	'BAL_SAMT_REV',	'MAX_TENURE_CD',	'MAX_TENURE_OVR',	'MONTHLY_SURPLUS',	'MAX_MOB_CD',	'MAX_MOB_UNSEC',	'NBR_BFL_ENQ',	'NEW_EXP_RAT_12M',	'NO_CLSD_CD_12M',	'NO_CLSD_CD_9M',	'NO_LIVE_AL_12M',	'NO_LIVE_BL_12M',	'NO_LIVE_CD_12M',	'NO_LIVE_EL_12M',	'NO_LIVE_GL_12M',	'NO_LIVE_LABD_12M',	'NO_LIVE_LAS_6M',	'NO_LIVE_OD_12M',	'NO_LIVE_PL_12M',	'NO_LIVE_REV_12M',	'NO_LIVE_SEC_12M',	'NO_LIVE_TWL_12M',	'NO_LIVE_UNSEC_12M',	'NO_LV_CD_TKN_12M',	'NO_LV_CD_TKN_9M',	'NUM_CD',	'NUM_DISTINCT_TLS',	'OTB_RATIO',	'TIME_SINCE_PEAK_EMI',	'TM_SN_FIRST_CLS_AL',	'TM_SN_FIRST_CLS_GL',	'TM_SN_FIRST_LIVE_AL',	'TM_SN_FIRST_LIVE_CC',	'TM_SN_FIRST_LIVE_CD',	'TM_SN_FIRST_LIVE_OVR',	'TM_SN_FIRST_LIVE_REV',	'TM_SN_FIRST_LIVE_SEC',	'TM_SN_FIRST_LIVE_UNSEC',	'TM_SN_FIRST_LOAN_GL',	'TM_SN_LAST_CLS_AL',	'TM_SN_LAST_CLS_CC',	'TM_SN_LAST_CLS_CD',	'TM_SN_LAST_CLS_OVR',	'TM_SN_LAST_CLS_PL',	'TM_SN_LAST_CLS_REV',	'TM_SN_LAST_CLS_SEC',	'TM_SN_LAST_CLS_UNSEC',	'TM_SN_LAST_LIVE_AL',	'TM_SN_LAST_LIVE_CD',	'TM_SN_LAST_LIVE_OVR',	'TM_SN_LAST_LIVE_PL',	'TM_SN_LAST_LIVE_REV',	'TM_SN_LAST_LIVE_UNSEC',	'TM_SN_LAST_LOAN_AL',	'TM_SN_LAST_LOAN_CC',	'TM_SN_LAST_LOAN_CD',	'TM_SN_LAST_LOAN_PL',	'TM_SN_LAST_LOAN_REV',	'TOT_ENQ',	'YEARLY_SURPLUS',	'CURR_POS_TO_POS_3MNTH',	'CURR_POS_TO_POS_6MNTH',	'CURR_POS_UNSEC_TO_CURR_POS',	'CUSTOMER_SEGMENTATION_FINAL',	'DIST_TRD_BFL',	'MIN_TENURE_PL',	'NO_OF_LOANS',	'NO_OF_LOANS_UNSEC',	'POS_6MNTH',	'POS_TO_AMTFIN_LV',	'POS_TO_AMTFIN_LV_UNSEC',	'PROD_OVRL_WGTRANK',	'RESTYPE_NULL',	'TIME_SINCE_LAST_LOAN',	'TIME_SINCE_LAST_LOAN_CD',	'TIME_SINCE_LAST_LOAN_UNSEC',	'VINTAGE_CUST', 'Y_CAN')
var <- c('Y_CAN', 'Nature_of_Business',	'Nature_Business',	'prod_i_5',	'prod_i_4',	'prod_i_3',	'Company_Type',	'COMP_CATEGORY',	'prod_i_2',	'prod_i_1',	'prod_rank_bucket',	'repay_rank_bucket',	'ip_flag',	'last_category_desc',	'first_category_desc',	'city_cluster',	'FIRST_LOAN',	'LAST_LOAN',	'city_cat',	'CITY_TIER',	'ocupation_category',	'GENDER',	'MARITAL_STATUS',	'Age_Bucket',	'aff_seg_new',	'Profession_type',	'aff_seg_new_2',	'persona_stamp_v5',	'HL_band',	'CD_band',	'CC_band',	'UNSECURED_band',	'wealth_persona',	'LAST_CD_LOAN_PRODUCT')
# var <- toupper(var)
tmp <- tmp[,var]
i <- sapply(tmp, is.factor)
tmp[i] <- lapply(tmp[i], as.character)
tmp[is.na(tmp)]<-"MISSING"
tmp[i] <- lapply(tmp[i], as.factor)

tmp1 <- readRDS("test_bin.RDS")
# colnames(tmp1)<-toupper(colnames(tmp1))  
tmp1 <- tmp1[,var]
i <- sapply(tmp1, is.factor)
tmp1[i] <- lapply(tmp1[i], as.character)
tmp1[is.na(tmp1)]<-"MISSING"
tmp1[i] <- lapply(tmp1[i], as.factor)


psi_woe <- function(data, y,var){
  data$count<-1
  d1 <- aggregate(cbind(count,data[,y])~data[,var], data, sum, na.action = na.pass)
  d1$var <- var
  colnames(d1) <- c("level","count","y","var")
  return(d1)
}


# var <- colnames(tmp)
d <- data.frame()
for (i in var){
  print(i)
  d2 <- psi_woe(tmp, "y_auto1", i)
  d<-rbind(d,d2)
}

dv <- data.frame()
for (i in var){
  print(i)
  d2 <- psi_woe(tmp1, "y_auto1", i)
  dv<-rbind(dv,d2)
}

df <- merge(d,dv, by=c("var","level"),all.x=T,all.y = T)
colnames(df)<-c("var","level","count_train","y_train","count_val","y_val")
write.csv(df,"psi_woe1_char.csv")
