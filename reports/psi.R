
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

