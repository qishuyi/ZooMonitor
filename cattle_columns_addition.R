#Day of Week
cattle_data <- mutate(cattle_data, Day_of_Week = wday(Date, label = TRUE))