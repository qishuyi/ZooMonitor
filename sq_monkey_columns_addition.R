#Day of week
sq_monkey_data <- mutate(sq_monkey_data, Day_of_Week = wday(Date, label = TRUE))