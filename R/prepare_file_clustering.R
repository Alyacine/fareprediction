# Load coordinates to Cluster
if(TRUE){ # IF ONE BY ONE
  test.sp <- test[!duplicated(test[,1:2]),]
  train.sp <- train[!duplicated(train[,1:2]),]
}else{ # IF cluster both train and test
  two <- rbind(train.sp,test.sp)
  two<- two[!duplicated(two[,1:2]),]
  write.csv(two,"save_train.csv")
}