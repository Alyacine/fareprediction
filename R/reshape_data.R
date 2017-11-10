# IF FALSE reshape Train / TRUE reshape test
if(which){
  test <- test[order(test$taxi_id, test$starting_timestamp),]
  test$.id <- with(test,ave(taxi_id,taxi_id , FUN = seq_along))
  # Get first item from each group 
  temp_IDs <- test[test$.id==1,]$ID
  #Get last item from each group
  test_3 <- test %>% 
    group_by(taxi_id) %>%
    filter(.id == max(.id))
  
  test$.id <- with(test, ave(taxi_id, taxi_id, FUN =function(x) if((length(test[test$taxi_id==x[1],]$taxi_id)/2 %% 2) == 0) {rep(1:(length(test[test$taxi_id==x[1],]$taxi_id)/2),each=2)}else{rep(1:((length(test[test$taxi_id==x[1],]$taxi_id)/2)+1),each=2)}))
  test$FACTOR <- with(test, ave(test$taxi_id, taxi_id,FUN = function(x) rep(1:2,1,length(x))))
  test$ite <- ave(test$taxi_id, test$taxi_id,FUN = function(x) x)
  test$type <- 0
  
  test_2 = test[!(test$ID %in% temp_IDs) & !(test$ID %in% test_3$ID),]
  test_2$type <- 1 
  test_2$.id <- with(test_2, ave(taxi_id, taxi_id, FUN =function(x) if((length(test_2[test_2$taxi_id==x,]$taxi_id)/2 %% 2) == 0) {rep(1:(length(test_2[test_2$taxi_id==x,]$taxi_id)/2),each=2)}else{rep(1:((length(test_2[test_2$taxi_id==x,]$taxi_id)/2)+1),each=2)}))
  test_2$FACTOR <- with(test_2, ave(test_2$taxi_id, taxi_id,FUN = function(x) rep(1:2,1,length(x))))
  
  
  final_set <- list(test, test_2)
  visited <- FALSE
  first <- TRUE
  for( f_set in final_set){
    #Reshape complexity is very high
    #We split train data into 24 sets
    k = 24
    number_taxis <- as.data.frame(unique(f_set$taxi_id))
    colnames(number_taxis)[1] <- "taxi_id"
    
    number_taxis$set <- sample(1:k, length(number_taxis$taxi_id), replace = TRUE)
    f_set = merge(x = f_set, y = number_taxis, by = "taxi_id", all = TRUE)
    
    for(i in 1:k){
      set <- subset(f_set, set == i)
      print(paste0("Set ",i,"/24 of all test data."))
      ## Lets Reshape now !!!
      tmp_loop <- reshape(set, 
                          direction="wide",
                          idvar=c("taxi_id","type",".id","set"),
                          timevar="FACTOR")
      if(!visited){
        visited=TRUE
        all_sets <- rbind(tmp_loop)
      }else{
        all_sets <- rbind(all_sets,tmp_loop)
      }
      
      gc()
      gc()
    }
  }
  if(TRUE){
    
    all_sets_test <- all_sets
    IDS <- all_sets_test$ID.1
    tmp <- test[!(test$ID %in% IDS),]
    colnames(tmp) <- paste(colnames(tmp),".1",sep="")
    colnames(tmp)[2] <- "taxi_id"
    all_sets_test <- rbind.fill(all_sets_test,tmp)
    test <- all_sets_test
    
    # Clean Data 
    columns_out <- c("type",
                     "set",
                     "ite.1",
                     "ite.2",
                     ".id",
                     ".id.1",
                     "FACTOR.1",
                     "type.1")
    test <- all_sets_test
    test <- as.data.table(test)
    
    test <- test[, -columns_out, with = FALSE]
    str(test)
    colnames(test)[6] <- "revenue_class"
    
    remove(all_sets_test)
    
    remove(test_2)
    remove(test_3)
    
    gc()
    #Distance and Duration --- Test data
    test$DISTANCE <- earthDist (test$starting_longitude.2, test$starting_latitude.2,
                                test$starting_longitude.1, test$starting_latitude.1)
    test$DURATION <- test$starting_timestamp.2 - test$starting_timestamp.1
  }
}else {
  train <- train[order(train$taxi_id, train$starting_timestamp),]
  train$.id <- with(train,ave(taxi_id,taxi_id , FUN = seq_along))
  # Get first item from each group 
  temp_IDs <- train[train$.id==1,]$ID
  #Get last item from each group
  train_3 <- train %>% 
    group_by(taxi_id) %>%
    filter(.id == max(.id))
  
  train$.id <- with(train, ave(taxi_id, taxi_id, FUN =function(x) if((length(train[train$taxi_id==x,]$taxi_id)/2 %% 2) == 0) {rep(1:(length(train[train$taxi_id==x,]$taxi_id)/2),each=2)} else {rep(1:((length(train[train$taxi_id==x,]$taxi_id)/2)+1),each=2)}))
  train$FACTOR <- with(train, ave(train$taxi_id, taxi_id,FUN = function(x) rep(1:2,1,length(x))))
  train$ite <- ave(train$taxi_id, train$taxi_id,FUN = function(x) x)
  train$type <- 0 
  
  train_2 = train[!(train$ID %in% temp_IDs) & !(train$ID %in% train_3$ID),]
  train_2$type <- 1 
  train_2$.id <- with(train_2, ave(taxi_id, taxi_id, FUN =function(x) if((length(train_2[train_2$taxi_id==x,]$taxi_id)/2 %% 2) == 0) {rep(1:(length(train_2[train_2$taxi_id==x,]$taxi_id)/2),each=2)}else{rep(1:((length(train_2[train_2$taxi_id==x,]$taxi_id)/2)+1),each=2)}))
  train_2$FACTOR <- with(train_2, ave(train_2$taxi_id, taxi_id,FUN = function(x) rep(1:2,1,length(x))))
  
  final_set <- list(train,train_2)
  
  first <- TRUE
  visited <- FALSE
  for( f_set in final_set){
    
    #Reshape complexity is very high
    #We split train data into 12 sets
    k = 12
    
    number_taxis <- as.data.frame(unique(f_set$taxi_id))
    colnames(number_taxis)[1] <- "taxi_id"
    
    number_taxis$set <- sample(1:k, length(number_taxis$taxi_id), replace = TRUE)
    f_set = merge(x = f_set, y = number_taxis, by = "taxi_id", all = TRUE)
    
    for(i in 1:k){
      set <- subset(f_set, set == i)
      print(i)
      print(paste0("Set ",i,"/12 of all train data."))
      ## Lets Reshape now !!!
      tmp_loop <- reshape(set, 
                          direction="wide",
                          idvar=c("taxi_id","type",".id","set"),
                          timevar="FACTOR")
      if(!visited){
        visited=TRUE
        all_sets <- rbind(tmp_loop)
      }else{
        all_sets <- rbind(all_sets,tmp_loop)
      }
      
      gc()
      gc()
    }
  }
  if(TRUE){
  all_sets_train <- all_sets
  
  IDS <- all_sets_train$ID.1
  tmp <- train[!(train$ID %in% IDS),]
  
  colnames(tmp) <- paste(colnames(tmp),".1",sep="")
  colnames(tmp)[2] <- "taxi_id"
  all_sets_train <- rbind.fill(all_sets_train,tmp)
  
  #Clean Data
  columns_out <- c("type",
                   "set",
                   "ite.1",
                   "ite.2",
                   ".id",
                   ".id.1",
                   "FACTOR.1",
                   "type.1")
  train <- all_sets_train
  train <- as.data.table(train)
  
  train <- train[, -columns_out, with = FALSE]
  colnames(train)[6] <- "revenue_class"
  
  remove(all_sets_train)
  
  remove(train_2)
  remove(train_3)
  gc()
  #Distance and Duration --- Train data
  train$DISTANCE <- earthDist (train$starting_longitude.2, train$starting_latitude.2,
                               train$starting_longitude.1, train$starting_latitude.1)
  train$DURATION <- train$starting_timestamp.2 - train$starting_timestamp.1
  }
}


remove(all_sets)
remove(tmp)
remove(tmp_loop)
remove(set)
remove(f_set)
remove(number_taxis)
remove(final_set)
remove(IDS)
remove(temp_IDs)
remove(i)
remove(k)
remove(columns_out)
remove(first)
remove(visited)
remove(which)
