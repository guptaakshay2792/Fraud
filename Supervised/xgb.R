#library(xda)
library(MLmetrics)
library(xgboost)

setwd("D:/Income Imputation/Data")


gbm_data<-readRDS("income_bureau_merged.RDS")


colnames(gbm_data)<-toupper(colnames(gbm_data))

gbm_data_all<-gbm_data

gbm_data<-subset(gbm_data_all,gbm_data_all$DISBURSEMENTDATE <= as.Date('2017-04-30'))
gbm_score<-subset(gbm_data_all,gbm_data_all$DISBURSEMENTDATE > as.Date('2017-04-30'))



set.seed(1234)
select.obs <- runif(nrow(gbm_data))
gbm.data.train <- gbm_data[select.obs<0.7,]
gbm.data.test <- gbm_data[select.obs>=0.7,]
gbm_data_val<- gbm_score




# Creating set_flag which indicates to which split it belongs to (Training, Test, Validation)

gbm.data.train$set_flag <- "Training"
gbm.data.test$set_flag <- "Test"


new_tr <- model.matrix(~.+0,data = gbm.data.train[,!(names(gbm.data.train) %in% c("EMI_FINAL"))]) 
new_ts <- model.matrix(~.+0,data = gbm.data.test[,!(names(gbm.data.test) %in% c("EMI_FINAL"))])
new_val <- model.matrix(~.+0,data = gbm_data_val[,!(names(gbm_data_val) %in% c("EMI_FINAL"))])

labels <- gbm.data.train$EMI_FINAL 
ts_label <- gbm.data.test$EMI_FINAL
val_label <- gbm_data_val$EMI_FINAL


dtrain <- xgb.DMatrix(data = new_tr,label = labels) 
dtest <- xgb.DMatrix(data = new_ts,label=ts_label)
dval <- xgb.DMatrix(data = new_val,label=val_label)


#Declaring lists and arrays required to store roc values ---------------------------------
train_accuracy=list()
test_accuracy=list()
val_accuracy=list()
training_roc=list()
test_roc=list()
val_roc=list()
best_trees=list()
training_mape=list()
training_rmse=list()
training_mae=list()
test_mape=list()
test_rmse=list()
test_mae=list()
val_mape=list()
val_rmse=list()
val_mae=list()
training_accuracy_30=list()
training_accuracy_20=list()
training_accuracy_10=list()
test_accuracy_30=list()
test_accuracy_20=list()
test_accuracy_10=list()
val_accuracy_30=list()
val_accuracy_20=list()
val_accuracy_10=list()


dep=c(3)

inner <- 1



int_dep<-dep
for (int_dep in dep) #defines values of interaction.depth
{ 
    
    k <- inner
    n <- int_dep
    
    print(paste("interation_depth=", int_dep))
    
    params <- list(
                      booster = "gbtree" 
                      #Sets the booster type (gbtree, gblinear or dart) to use. 
                      #For classification problems, you can use gbtree, dart. 
                      #For regression, you can use any
                      #[default=gbtree]
                      , objective = "binary:logistic" 
                      #Will train a binary classification model(binary:logistic), reg:linear for regression
                      , eta=0.01
                      #Learning rate, Typically, it lies between 0.01 - 0.3(default). Range: (0,1)
                      , gamma=0
                      #controls regularization. Higher the value, higher the regularization. 
                      #Default = 0 means no regularization.
                      #Tune trick: Start with 0 and check CV error rate. 
                      #If you see train error >>> test error, then use gamma. 
                      #Higher the gamma, lower the difference in train and test CV. 
                      #Remember that gamma brings improvement when you want to use shallow (low max_depth) trees
                      , max_depth=int_dep
                      #Depth of tree
                      , min_child_weight=1 
                      #default=1, range:(0,Inf)
                      , subsample=0.5
                      #It controls the number of samples (observations) supplied to a tree.
                      #Typically, its values lie between (0.5-0.8), [default=1][range: (0,1)]
                      , colsample_bytree=1
                      #It control the number of features (variables) supplied to a tree
                      #Typically, its values lie between (0.5,0.9), [default=1][range: (0,1)]
                  )
    
    xgbcv <- xgb.cv (   params = params
                        , data = dtrain
                        , nrounds = 4000
                        # Max number of iterations. For classification, it is similar to the number of trees
                        # Should be tuned using CV
                        , nfold = 3
                        , showsd = T
                        , stratified = T
                        , print_every_n  = 10
                        , early_stopping_rounds =  2000
                        , maximize = F
			                  , metrics = "rmse"
    ) 
    
    # Getting the best round/tree/iteration from xgbcv
    bestTrees <- xgbcv$best_iteration
    best_trees[k]=bestTrees
    
    print(paste0("Best Tress - ", bestTrees))
#7946, 541
    xgb1 <- xgb.train (  
                           params = params
                         , data = dtrain
                         , nrounds = bestTrees
                         , watchlist = list(train=dtrain,test=dtest)
                         , print_every_n = 10
                         #, early_stopping_rounds = 3000
                         , maximize = F 
                         , eval_metric = "auc"
                         #These metrics are used to evaluate a model's accuracy on validation data. 
                         #For regression, default metric is RMSE. For classification, default metric is error.
                         #Available error functions are as follows:
                            #mae - Mean Absolute Error (used in regression)
                            #logloss - Negative loglikelihood (used in classification)
                            #auc - Area under curve (used in classification)
                            #rmse - Root mean square error (used in regression)
                            #error - By default, it uses the 0.5 threshold for predicted values 
                                    #to define negative and positive instances. Different threshold 
                                    #(e.g., 0.) could be specified as "error@0."
                            #merror - similar to error but for mulitclassification
                            #mlogloss - multiclass logloss (used in classification)
                       )
    
    mat <- xgb.importance (feature_names = colnames(new_tr), model = xgb1)
    write.csv(mat, file=paste0("VAR_IMP_",n,".csv"))
  
    # Predicting on training
    pred_gbm<- predict(xgb1, newdata=dtrain, type='response', ntreelimit = bestTrees)
    gbm.data.train<-cbind(gbm.data.train,pred_gbm)
    
    # Predicting on test
    pred_gbm<- predict(xgb1, newdata=dtest, type='response', ntreelimit = bestTrees)
    gbm.data.test<-cbind(gbm.data.test,pred_gbm)
    
    # Predicting on validation
    pred_gbm<- predict(xgb1, newdata=dval, type='response', ntreelimit = bestTrees)
    gbm_data_val<-cbind(gbm_data_val,pred_gbm)
    
    training_roc[k]=MLmetrics::AUC(gbm_data_split[gbm_data_split$set_flag=='Training' ,"EMI_FINAL"],
                                 gbm_data_split[gbm_data_split$set_flag=='Training',"pred_gbm"])
    
    val_roc[k] = MLmetrics::AUC(gbm_data_split[gbm_data_split$set_flag=='Validate',"EMI_FINAL"],
                                     gbm_data_split[gbm_data_split$set_flag=='Validate',"pred_gbm"])
    
    test_roc[k] = MLmetrics::AUC(gbm_data_split[gbm_data_split$set_flag=='Test',"EMI_FINAL"],
                               gbm_data_split[gbm_data_split$set_flag=='Test',"pred_gbm"])
    
    
    gbm.data.train$COUNT <- 1
    gbm.data.train$pred_rank <- ave(gbm.data.train$pred, FUN = function(x) rank(-x, ties.method = "first"))
    Percentile <- quantile(gbm.data.train$pred_rank,probs= seq(0,1,by=0.01))
    gbm.data.train<-gbm.data.train[order(gbm.data.train$pred_rank),]
    gbm.data.train$Percentile<-cut(gbm.data.train$pred_rank,breaks = Percentile,labels = FALSE,include.lowest = TRUE)
    
    write.csv(gbm.data.train,paste0("Train_ALLDATA_",n,".csv"))  
    
    scoring_buld <- aggregate(cbind(
      COUNT,
      pred_gbm,
      EMI_FINAL
    ) ~ Percentile, gbm.data.train, sum)
    
    # z<-gbm_buld[,c("Percentile","PI_PC_FRAUD","pred_rank","POLICY_CONDITION_FRAUD","POLICY_CONDITION_FRAUD","STAGED_ACCIDENT_FRAUD","DUI_FRAUD","FRAUD_FLAG","triage_flag","INVESTIGATION_FLAG","Savings","Third_Party_Savings","STAGED_THEFT_SAVINGS")]
    write.csv(scoring_buld,paste0("Summary_Train_",n,".csv"))  
    
    # 
    gbm.data.test$COUNT <- 1
    gbm.data.test$pred_rank <- ave(gbm.data.test$pred, FUN = function(x) rank(-x, ties.method = "first"))
    Percentile <- quantile(gbm.data.test$pred_rank,probs= seq(0,1,by=0.01))
    gbm.data.test<-gbm.data.test[order(gbm.data.test$pred_rank),]
    gbm.data.test$Percentile<-cut(gbm.data.test$pred_rank,breaks = Percentile,labels = FALSE,include.lowest = TRUE)
    
    write.csv(gbm.data.train,paste0("Test_ALLDATA_",n,".csv"))  
    
    scoring_buld <- aggregate(cbind(
      COUNT,
      pred_gbm,
      EMI_FINAL
    ) ~ Percentile, gbm.data.test, sum)
    
    # z<-gbm_buld[,c("Percentile","PI_PC_FRAUD","pred_rank","POLICY_CONDITION_FRAUD","POLICY_CONDITION_FRAUD","STAGED_ACCIDENT_FRAUD","DUI_FRAUD","FRAUD_FLAG","triage_flag","INVESTIGATION_FLAG","Savings","Third_Party_Savings","STAGED_THEFT_SAVINGS")]
    write.csv(scoring_buld,paste0("Summary_Test_",n,".csv"))  
    
    gbm_data_val$COUNT <- 1
    gbm_data_val$pred_rank <- ave(gbm_data_val$pred, FUN = function(x) rank(-x, ties.method = "first"))
    Percentile <- quantile(gbm_data_val$pred_rank,probs= seq(0,1,by=0.01))
    gbm_data_val<-gbm_data_val[order(gbm_data_val$pred_rank),]
    gbm_data_val$Percentile<-cut(gbm_data_val$pred_rank,breaks = Percentile,labels = FALSE,include.lowest = TRUE)
    
    write.csv(gbm_data_val,paste0("VAL_ALLDATA_",n,".csv"))  
    
    scoring_buld <- aggregate(cbind(
      COUNT,
      pred_gbm,
      EMI_FINAL
    ) ~ Percentile, gbm_data_val, sum)
    
    # z<-gbm_buld[,c("Percentile","PI_PC_FRAUD","pred_rank","POLICY_CONDITION_FRAUD","POLICY_CONDITION_FRAUD","STAGED_ACCIDENT_FRAUD","DUI_FRAUD","FRAUD_FLAG","triage_flag","INVESTIGATION_FLAG","Savings","Third_Party_Savings","STAGED_THEFT_SAVINGS")]
    write.csv(scoring_buld,paste0("Summary_Val_",n,".csv"))  
    
    inner <- inner+1  
    
    
    saveRDS(xgb1,file=paste0("Model_",n,".RDS"))
    saveRDS(xgbcv,file=paste0("Model_CV_",n,".RDS"))
    save.image(paste0("Workspace_",n,".RData"))
  
}

training_roc=list()
test_roc=list()
val_roc=list()

#
# # # # To Get ROC areas for training and test datasets in a dataframe --------------
training_roc_a = array(training_roc,c(k,1))
test_roc_a = array(test_roc,c(k,1))
val_roc_a = array(val_roc,c(k,1))

params = list()
depth_vals = list()
# #
for (i in 1:length(dep))
{
  params = list(rep(dep[i],length(obs)))
  depth_vals = unlist(c(depth_vals,params))
  # #
}
# #
best_trees = unlist(best_trees)
# obs_vals = unlist(list(rep(obs,length(dep))))
roc_df = data.frame(training_roc_a, test_roc_a, val_roc_a)
roc_df_bind = cbind(depth_vals, best_trees, roc_df)
roc_df_bind_n <- data.frame(lapply(roc_df_bind, as.character), stringsAsFactors=FALSE)
write.csv(roc_df_bind_n, file=paste0('Summary','.csv'))
# 
# # #
# # #
# #
# 
