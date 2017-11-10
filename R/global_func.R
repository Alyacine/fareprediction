#Feauture engineering Functions
dates_time <- function(data) {
  
  # Convert Unix time to Date object in R format like 2015-01-01 02:00:36
  data$starting_timestamp.1 <- as.POSIXct(data$starting_timestamp.1,
                                          origin="1970-01-01")
  data$starting_timestamp.2 <- as.POSIXct(data$starting_timestamp.2,
                                          origin="1970-01-01")
  # Day of the year to get holidays
  data$day_year.1 <- yday(data$starting_timestamp.1) # we just have 121 first days
  data$day_year.2 <- yday(data$starting_timestamp.2) # we just have 121 first days
  data$weekday.1  <- as.factor(weekdays(data$starting_timestamp.1))
  data$weekday.2  <- as.factor(weekdays(data$starting_timestamp.2))
  
  # Order levels of the week
  levels(data$weekday.1)   <- c("1","2","3","4","5","6","7")
  levels(data$weekday.2)   <- c("1","2","3","4","5","6","7")
  
  data <- separate(data, starting_timestamp.1, c('date.1', 'time.1'), sep = ' ', remove = FALSE)
  data <- separate(data, date.1, c('Year.1', 'Month.1', 'Day.1'), sep = '-', remove = TRUE)
  data <- separate(data, time.1, c('Hour.1', 'Minute.1', 'Second.1'), sep = ':', remove = TRUE)
  
  data <- separate(data, starting_timestamp.2, c('date.2', 'time.2'), sep = ' ', remove = FALSE)
  data <- separate(data, date.2, c('Year.2', 'Month.2', 'Day.2'), sep = '-', remove = TRUE)
  data <- separate(data, time.2, c('Hour.2', 'Minute.2', 'Second.2'), sep = ':', remove = TRUE)
  
  data$Month.1  <- as.numeric(data$Month.1)/12
  HourMinute.1  <- as.numeric(data$Hour.1) + as.numeric(data$Minute.1)/60
  
  data$Month.2  <- as.numeric(data$Month.2)/12
  HourMinute.2  <- as.numeric(data$Hour.2) + as.numeric(data$Minute.2)/60
  
  data <- cbind(data, HourMinute.1)
  data <- cbind(data, HourMinute.2)
  
  # normalize day of month
  data <- mutate(data, Day.1 = as.numeric(Day.1)/monthDays(starting_timestamp.1))
  data <- mutate(data, Day.2 = as.numeric(Day.2)/monthDays(starting_timestamp.2))
  
  return(data)
}

holidays <- function(data) {
  #
  # https://www.timeanddate.com/holidays/greece/
  # http://www.epochconverter.com/days/2015
  #
  # Date	Weekday	Holiday Name	Holiday Type
  # Jan 01	Sunday	New Year's Day	Public holiday
  # Jan 06	Friday	Epiphany	Public holiday
  # Jan 30	Monday	The Three Holy Hierarchs	Observance
  # Feb 27	Monday	Clean Monday	Public holiday
  # Mar 20	Monday	March equinox	Season
  # Mar 25	Saturday	25th of March (national holiday)	Public holiday
  # Mar 25	Saturday	Annunciation of the Lord	Public holiday
  # Apr 14	Friday	Good Friday	Public holiday
  # Apr 16	Sunday	Easter Sunday	Public holiday
  # Apr 17	Monday	Easter Monday	Public holiday
  # May 01	Monday	Labor Day / May Day	Public holiday
  
  holidays <- c(1, 6, 54, 84, 100 , 102, 103, 121, 152)
  data$holiday.1 <-vector(mode="numeric", length=nrow(data))
  data$holiday.2 <-vector(mode="numeric", length=nrow(data))
  
  for (day in 1:tail(data$day_year.1, n=1)) {
    
    if (day %in% holidays){
      
      index = which(data$day_year.1 == day)
      data$holiday.1[index] = 1
    }
  }
  for (day in 1:tail(data$day_year.2, n=1)) {
    
    if (day %in% holidays){
      
     index = which(data$day_year.2 == day)
      data$holiday.2[index] = 1
    }
  }
  data$holiday.1 <- as.factor(data$holiday.1)
  data$holiday.2 <- as.factor(data$holiday.2)
  return(data)
}


order_data <- function(data) {
  data <- data[order(data$taxi_id,
                     data$day_year.1,
                     data$Hour.1,
                     data$Minute.1),
               ]
}

change_type <- function(data) {
  data$ID.1 <- as.character(data$ID.1)
  data$ID.2 <- as.character(data$ID.2)
  data$Hour.1 <- as.numeric(data$Hour.1)
  data$Second.1 <- as.numeric(data$Second.1)
  data$Minute.1 <- as.numeric(data$Minute.1)
  data$Hour.2 <- as.numeric(data$Hour.2)
  data$Second.2 <- as.numeric(data$Second.2)
  data$Minute.2 <- as.numeric(data$Minute.2)
  # Remove uncessary columns
  data <- subset(data, select=-c(Year.1
  )
  )
  # Change columns to factors
  data$revenue_class <- as.factor(data$revenue_class)
  
  # Remove uncessary columns
  data <- subset(data, select=-c(Year.2
  )
  )
  return(data)
  
}

undersampling <- function(data, n_cases=21887) {
  # > table(train$revenue_class)
  # 
  # 1      2      3      4      5 
  # 305454 578618 425971 115299  21887
  #
  cat("\n SAMPLING \n")
  
  class1 <- sample_n(tbl = data[as.numeric(data$revenue_class) == 1,], n_cases)
  class2 <- sample_n(tbl = data[as.numeric(data$revenue_class) == 2,], n_cases)
  class3 <- sample_n(tbl = data[as.numeric(data$revenue_class) == 3,], n_cases)
  class4 <- sample_n(tbl = data[as.numeric(data$revenue_class) == 4,], n_cases)
  class5 <- sample_n(tbl = data[as.numeric(data$revenue_class) == 5,], n_cases)
  
  data <- rbind(class1,class2, class3, class4, class5)
  cat("\n DONE \n")
  return(data)
}


earthDist <- function (lon1, lat1, lon2, lat2){
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- lon1 * rad
  b1 <- lat2 * rad
  b2 <- lon2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

plot_by_revenue_class <- function(data,address,zoom,class){
  map <- get_map(location = address , zoom = zoom,
                 maptype = "roadmap", source = "google")
  if(tolower(class)=="all"){
    class=c(1,2,3,4,5)
  }
  ggmap(map) + geom_point(data = data[data$revenue_class==class], aes(x = starting_longitude.1, y = starting_latitude.1, fill = revenue_class, alpha = 1), size = 3, shape = 21) 
}
