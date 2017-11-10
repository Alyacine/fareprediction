
test_path <- "data/data_test_N_competition.csv"
train_path <- "data/data_train_competition.csv"

test <- read.csv(file = test_path, sep = ",")
train <- read.csv(file = train_path, sep = ",")


train <- undersampling(data = train)
