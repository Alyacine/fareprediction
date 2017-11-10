cat("\n - Lets visit Thessaloniki ------------------------\n")

# Clean memory
gc(reset=TRUE)
# Clean up environment variables
rm(list = ls())

# PARAMETERS #-------------------------------------------------------------
SEED <- 416
set.seed(SEED)

# WORKFLOW # --------------------------------------------------------------

cat("\n 1/7 - Load required libraries ---------------------------------\n")
source("R/libraries.R")
#
cat("\n 2/6 - Load Developped functions -------------------------------\n")
source("R/global_func.R")

cat("\n 3/7 - Load Train and Test data --------------------------------\n")
source("R/read_data.R")
#
cat("\n 4/7 - Preprocessing -------------------------------------------\n")
cat("\n Reshape Train DataSet -----------------------------------------\n")

which <- FALSE
source("R/reshape_data.R")

cat("\n Reshape Test DataSet -----------------------------------------\n")
which <- TRUE
source("R/reshape_data.R")

#Please ignore the 50 warnings()

cat("\n 5/7 - Preprocessing -------------------------------------------\n")
source("R/munging.R")
#
#Example Plot revenue_class class = "all" / Please try class = "number_of_class"
cat("\n \t - Example plot revenue_class - Enjoy GIS - \n Please go back to main.R for a new settings!\n")
train$revenue_class <- as.factor(as.character(train$revenue_class))
plot_by_revenue_class(data=train,
                      address="Thessaloniki",
                      zoom=11,
                      class="all")

#
cat("\n 6/7 - Load Dbscan Density-Based Clustering results from Python")
# IMPORTANT
# Please choose the type of Clustering - 
# Combined = TRUE if clustering made to two sets ( train and test)
# Combined = False if clustering made seperately

combined <- FALSE
source("R/prepare_file_clustering.R")

# After runing clustering.py load the results ( output latitude - longitude - cluster number)
#source("R/Load_Clustering.R")

cat("\n 7/7 - Submission ----------------------------------------------\n")
rrf <- TRUE # Random Forest by default 500 trees / Submitted with 900 trees
gc()
source("R/submission.R")

# End ---------------------------------------------------------------------

