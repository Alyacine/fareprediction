
# Run python script for clustering 
# Please move the file save_train.csv to the correct folder

if(combined){
  Clusters <- paste("data/Clustering_test_dbscan_",0.04,'.csv',sep='')
  Clusters <- fread(Clusters)
  Clusters <- as.data.frame(Clusters) 
  
  colnames(Clusters)[1] <- "starting_latitude.1"
  colnames(Clusters)[2] <- "starting_longitude.1"
  
  #---------------------------- Test assigning Clusters
  k <- data.frame(test)
  p <- data.frame(Clusters[,c(1,2,8)])
  
  test.sp <- merge(k, p,by=c("starting_latitude.1","starting_longitude.1"),all.x=TRUE)
  
  
  test.sp$Clusters<- NULL
  colnames(test.sp)[31] <- "Clusters"
  test <- test.sp
  test <- data.table(test)
  remove(k) ; remove(p); remove(test.sp); gc()
  #---------------------------- Train assigning Clusters
  k <- data.frame(train)
  
  p <- data.frame(Clusters[,c(1,2,8)])
  
  train.sp <- merge(k, p,by=c("starting_latitude.1","starting_longitude.1"),all.x=TRUE)
  train <- train.sp
  
  train.sp$Clusters<- NULL
  colnames(train.sp)[31] <- "Clusters"
  
  train <- data.table(train)
  remove(k) ; remove(p); remove(train.sp); gc()
}else{
  #---------------------------- Test assigning Clusters
  Clusters <- paste("data/Clustering_seperated_sets_test_dbscan_",0.16,'.csv',sep='')
  
  Clusters <- fread(Clusters)
  Clusters <- as.data.frame(Clusters)
  
  colnames(Clusters)[1] <- "starting_latitude.1"
  colnames(Clusters)[2] <- "starting_longitude.1"
  
  Clusters <- Clusters[!duplicated(Clusters),]
  Clusters <-  Clusters[!(duplicated(Clusters[c("starting_latitude.1","starting_longitude.1")]) | duplicated(Clusters[c("starting_latitude.1","starting_longitude.1")], fromLast = TRUE)), ]
  Clusters <- Clusters[ order(Clusters[,1], Clusters[,2]), ]
  k <- data.frame(test)
  k <- k[c(3,4)]
  p <- data.frame(Clusters[,c(1,2,8)])
  
  test.sp <- merge(k, p,by=c("starting_latitude.1","starting_longitude.1"),all.x=TRUE)
  test.sp <- test.sp[ order(test.sp[,1], test.sp[,2]), ]
  test <- test[ order(test[,3], test[,4]), ]
  test$Clusters <- test.sp$ID_y
  #---------------------------- Train assigning Clusters
  Clusters <- paste("data/Clustering_seperated_sets_train_dbscan_",0.16,'.csv',sep='')
  
  Clusters <- fread(Clusters)
  Clusters <- as.data.frame(Clusters)
  
  colnames(Clusters)[1] <- "starting_latitude.1"
  colnames(Clusters)[2] <- "starting_longitude.1"
  
  Clusters <- Clusters[!duplicated(Clusters),]
  Clusters <-  Clusters[!(duplicated(Clusters[c("starting_latitude.1","starting_longitude.1")]) | duplicated(Clusters[c("starting_latitude.1","starting_longitude.1")], fromLast = TRUE)), ]
  Clusters <- Clusters[ order(Clusters[,1], Clusters[,2]), ]
  k <- data.frame(train)
  k <- k[c(3,4)]
  p <- data.frame(Clusters[,c(1,2,8)])
  
  train.sp <- merge(k, p,by=c("starting_latitude.1","starting_longitude.1"),all.x=TRUE)
  train.sp <- train.sp[ order(train.sp[,1], train.sp[,2]), ]
  train <- train[ order(train[,3], train[,4]), ]
  train$Clusters <- train.sp$ID_y
}
remove(Clusters); remove(k); remove(p); remove(train.sp); remove(test.sp); gc()