#
# submission.R
# ------------------------------------------------------------------------------

# Metric -----------------------------------------------------------------------

kappaCW <-function(preds,trues) {
  # Cohen's Kappa and weighted Kappa
  # https://inclass.kaggle.com/c/taxi-fare-prediction-challenge-epia-2017/details/evaluation
  #
  require(irr)
  table(preds)
  return (kappa2(cbind(preds,trues),"squared")$value)
}

# Submission -------------------------------------------------------------------

submission <-function(train, test, perc_training) {
  #
  # Simple Houldout for final validation
  # Default 66% for training and 44% for validation
  # 
  gc()
  if(rrf==TRUE){
    cat(" \n \t Learner Type : Regularized Random Forest \n--\n")
  }else{
    cat(" \n \t Learner Type : XGboost \n--\n")
  }
  cat(" \t Feature Selection ...\n--\n")
  
  features.name <- c("starting_latitude.1",
                     "weekday.1",
                     "starting_longitude.1",
                     "Day.1",
                     "DISTANCE",
                     "DURATION",
                     "Hour.1",
                     "Minute.1",
                     "Second.1",
                     "taxi_id",
                     #"Cluster",
                     "revenue_class")
  
  #------------------------------------------------------------------------
  gc()
  cat(" \t Splitting data in train and validation set ...\n--\n")
  '%!in%' <- function(x,y)!('%in%'(x,y))
  #idx <- which(train$Month <= 2/12)
  
  set.seed(416)
  cat(" \t Learning submission model ...\n--\n")
  
  idx <- sample(x = c(1:nrow(train)), size = perc_training*nrow(train))
  #while(length(unique(train[idx,]$Address))!=44){
  #  idx <- sample(x = c(1:nrow(train)), size = perc_training*nrow(train))
  #}
  
  train$Hour.1 <- as.numeric(train$Hour.1)
  train$Minute.1 <- as.numeric(train$Minute.1)
  train$Second.1 <- as.numeric(train$Second.1)
  test$Hour.1 <- as.numeric(test$Hour.1)
  test$Minute.1 <- as.numeric(test$Minute.1)
  test$Second.1 <- as.numeric(test$Second.1)
 
  train_2 = train[ idx, features.name, with = FALSE]
  train_3 = train[ -idx, features.name, with = FALSE]
  
  
  features_test.name <- c("ID.1",
                          "starting_latitude.1",
                          "weekday.1",
                          "starting_longitude.1",
                          "Day.1",
                          "DISTANCE",
                          "DURATION",
                          "Hour.1",
                          "Minute.1",
                          "Second.1",
                          "taxi_id",
                          #"Cluster",
                          "revenue_class")
  
  test$weekday.1 =as.factor(test$weekday.1)
  test$weekday.2 =as.factor(test$weekday.2)
  
  cat(" \t ----------------------------------------------------------------\n--\n")
  if( rrf==TRUE ){
    
    if(FALSE){
      cat(" \t Running a Random Forest with K-fold CV ... \n--\n")
      # K-fold Validation
      k = 5
      train$set <- sample(1:k, nrow(train), replace = TRUE)
      list <- 1:k
      # prediction and test set data frames that we add to with each iteration over
      # the folds
      prediction <- data.frame()
      validsetCopy <- data.frame()
      
      #Creating a progress bar to know the status of CV
      progress.bar <- create_progress_bar("text")
      progress.bar$init(k)
     
      #function for k fold
      for(i in 1:k){
        # remove rows with id i from dataframe to create training set
        # select rows with id i to create test set
        trainingset <- subset(train, id %in% list[-i])
        validset <- subset(train, id %in% c(i))
        class_indx <- which(colnames(validset) == "revenue_class")
        
        #run a random forest model
        mymodel <- randomForest(formula = revenue_class ~.,
                                data    = trainingset,
                                ntree=1000)
        
        
        #remove response column 1, Sepal.Length
        temp <- as.data.frame(predict(mymodel, validset[, -class_indx,with=FALSE]))
        temp <- as.data.frame(predict(mymodel, validset[, -class_indx,with=FALSE]))
        
        # append this iteration's predictions to the end of the prediction data frame
        prediction <- rbind(prediction, temp)
        
        # append this iteration's test set to the test set copy data frame
        # keep only the revenue_class Column
        validsetCopy <- rbind(validsetCopy, as.data.frame(validset[, class_indx,with=FALSE]))
        progress.bar$step()
      }
      # add predictions and actual Sepal Length values
      result <- cbind(prediction, validsetCopy[, 1])
      names(result) <- c("Predicted", "Actual")
      result$Difference <- abs(result$Actual - result$Predicted)
      summary(result$Difference)
      
      cat(" \t Evaluation using a validation set ...\n--\n")
      
      #validation_result <- predict ( submission_model, newdata = train_3 )
      metric <- kappaCW(preds = result$Predicted,
                        trues = result$Actual)
      print(sprintf("Evaluation Metric in Validation set: %.4f", metric))
    }else{
      class_indx <- which(colnames(train_3) == "revenue_class")
      
      train_2$revenue_class <- as.factor(train_2$revenue_class)
      train_3$revenue_class <- as.factor(train_3$revenue_class)
      
      
      train_3 = train_3[ , -class_indx,with=FALSE]
      str(train_2)
      submission_model <- randomForest(formula = revenue_class ~.,
                              data    = train_2,
                              ntree=500)
      gc()
      gc()
      train_2$taxi_id <- as.numeric(as.character(train_2$taxi_id))
      train_3$taxi_id <- as.numeric(as.character(train_3$taxi_id))
      test$taxi_id <- as.numeric(as.character(test$taxi_id))
      #submission_model <- cforest(formula = as.factor(revenue_class) ~.,
      #                            data    = train_2,
      #                            controls= cforest_unbiased(ntree=1000, mtry=3))
      print(submission_model)
      #varImpPlot(submission_model)
      #importance(submission_model , type=1)
      cat(" \t Evaluation using a validation set ...\n--\n")
      gc()
      validation_result <- predict ( submission_model, newdata = train_3 )
      
      metric <- kappaCW(preds = validation_result,
                        trues = train[ -idx, c("revenue_class")])
      gc()
      print(sprintf("Evaluation Metric in Validation set: %.4f", metric))
      if(TRUE) { # IF you want to make a submission .....
        # Submission ------------------------------------------------------
        cat(" \t Preparing a submission ...\n--\n")
        
        #test.dt$taxi_id = as.numeric(test.dt$taxi_id)
        #test.dt <- test.dt[,-1]
        test = test[,features_test.name, with = FALSE]
        IDS  <- test[,1]
        test <- test[,-1]
        str(test)
        cat(" \t Predicting output classes on test dataset ...\n--\n")
        classes <- predict(submission_model,test)
       
        cat(" \t Writing output to output folder ...\n--\n")
        
        # Format:
        # "ID","revenue_class"
        # "A160001","2"
        
        data_submission <- data.frame(ID=IDS,revenue_class=classes)
        
        write.csv(x = data_submission,
                  file = paste0("submissions/submission_", sprintf("%.5f",metric),".csv"),
                  row.names=FALSE)
      }
    }
    cat(" \t End of Random Forest ...\n--\n")
    return(metric)
  }
  cat(" \t \n DONE!")
}
metric = submission(train = train, test = test,perc_training=0.7)
