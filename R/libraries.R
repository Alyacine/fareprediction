#
# libraries
#
# https://gist.github.com/stevenworthington/3178163

ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE, repos="http://cran.dcc.fc.up.pt/")
    sapply(pkg, require, character.only = TRUE)
}

# Library list
#
# One package per line for easy switch on/off
packages <- c(
    "digest", # check data integraty of files
    # If you need functions from both plyr and dplyr, please load plyr first, then dplyr
    "Hmisc",
    "dplyr",
    "chron",
    "stats",
    "magrittr",
    "readr",
    #"data.table",
    "plyr",
    "stringr",
    "dplyr", #"data.table + dplyr code now lives in dtplyr.
    "dtplyr",
    "tidyr",
    "lubridate",
    "EMCluster",
    "geosphere",
    "data.table",
    "stringr",
    "chron",
    "reshape",
    "splitstackshape",
    # Spatial Data
    "fossil",
    "ggmap",
    #"geosphere",
    # "rgdal",
    # "rgeos",
    # "maptools",
    # "tmap",
    # Data Mining
    "DMwR",
    #"ROSE", # for SMOTE
    # Machine Learning libraries
    "C50",
    "irr",
    "rpart",
    "gbm",
    "randomForest",
    "ranger",
    "RRF",
    "e1071",
    "kernlab",
    "fpc",
    # "neuralnet",
    # "nnet",
    "xgboost", # Linux user should compile from source - requires devtools
    # Meta packages
    "caret",
    "RRF",
    "performanceEstimation",
    # Ploting
    # http://blog.kaggle.com/2016/11/30/seventeen-ways-to-map-data-in-kaggle-kernels/
    "ggplot2",
    "data.table"
    # "maps",
    # "leaflet",
    # "ggmap",
    # "stringi",
    # "mapproj"
)

ipak(packages)

sessionInfo()
