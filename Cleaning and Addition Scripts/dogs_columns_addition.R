##Day_of_Week Column
dogs_data <- mutate(dogs_data, Day_of_Week = wday(Date, label = TRUE))

#Adding Food Column
dogs_data <- cbind(dogs_data, Food = dogs_data$Day_of_Week) 
dogs_data$Food <- as.character(dogs_data$Food)

for(day in c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")){
  
  if(day == "Fri"){
    dogs_data$Food[dogs_data$Food == day] <- "Guinea Pig"
  }
  
  else if(day == "Wed" | day == "Sun"){
    dogs_data$Food[dogs_data$Food == day] <- "Bones"
  }
  
  else{
    dogs_data$Food[dogs_data$Food == day] <- "Ground Meat"
  }
}

##Season Column
dogs_data <- cbind(dogs_data, Season = dogs_data$Month)
dogs_data$Month <- as.character(dogs_data$Month)

for(month in c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")){
  
  if(month == "1" | month == "2" | month == "12"){
    dogs_data$Season[dogs_data$Season == month] <- "Winter"
  }
  else if(month == "3" | month == "4" | month == "5"){
    dogs_data$Season[dogs_data$Season == month] <- "Spring"
  }
  else if(month == "6" | month == "7" | month == "8"){
    dogs_data$Season[dogs_data$Season == month] <- "Summer"
  }
  
  else{
    dogs_data$Season[dogs_data$Season == month] <- "Fall"
  }}

##Temperature/Weather Columns
#Cleaning weather_temp dataset
weather_temp <- read_csv("Weather_Temperature.csv",
                         col_types = cols(.default = col_character(),
                                          TAVG = col_double(),
                                          TMAX = col_double(),
                                          TMIN = col_double(),
                                          TOBS = col_double(),
                                          WT01 = col_double(),
                                          WT02 = col_double(),
                                          WT03 = col_double(),
                                          WT05 = col_double(),
                                          WT06 = col_double(),
                                          WT07 = col_double(),
                                          WT08 = col_double(),
                                          WT09 = col_double(),
                                          WT11 = col_double()))

weather_temp$DATE <- as.Date(weather_temp$DATE, format = "%m/%d/%y")
names(weather_temp)[names(weather_temp) == "DATE"] <- "Date"
weather_temp <- weather_temp %>% select(-c(STATION, NAME, TOBS))
weather_temp <- weather_temp %>%
  mutate(TAVG_C = round((TAVG-32)*5/9)) %>%
  mutate(TMAX_C = round((TMAX-32)*5/9)) %>%
  mutate(TMIN_C = round((TMIN-32)*5/9))
weather_temp <- weather_temp %>% slice(1:717)

#Merge/Further Cleaning
dogs_data <- left_join(dogs_data, weather_temp, by = "Date")
##Average of Average-max Temperature column and its temperature level
dogs_data <- mutate(dogs_data, TAVG_TMAX_avg = (dogs_data$TMAX + dogs_data$TAVG)/2)
dogs_data <- within(dogs_data, Temp_Level <- as.integer(cut(TAVG_TMAX_avg, quantile(TAVG_TMAX_avg, probs=0:10/10), include.lowest=TRUE)))
dogs_data$Temp_Level <- as.character(dogs_data$Temp_Level)
dogs_data$Temp_Level <- factor(dogs_data$Temp_Level, c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

##Add column for six month intervals
start_date <- min(dogs_data[,'Date'])
end_date <- max(dogs_data[,'Date'])

# Create a vector to hold all six-month intervals
six_month_intervals <- c()

# Add all intervals into the vector in a loop
lower_bound <- start_date
while (lower_bound <= end_date) {
  upper_bound <- lower_bound
  month(upper_bound) <- month(upper_bound) + 6
  if(upper_bound > end_date) upper_bound <- end_date
  six_month_intervals[length(six_month_intervals)+1] <- paste(lower_bound, upper_bound, sep = "~")
  month(lower_bound) = month(lower_bound) + 6
}

# Add the six-month interval column based on the Date column
get_interval <- function(inputDate) {
  six_month_intervals[(as.yearmon(inputDate)-
                         as.yearmon(start_date))*12/6 + 1]
}
get_interval <- Vectorize(get_interval)
dogs_data <- mutate(dogs_data, Six_Month_Interval = get_interval(Date))

##Add Column for weather type
dogs_data <- select(dogs_data, c(-WT02, -WT06, -WT07, -WT09, -WT11))
dogs_data$WT01 <- ifelse(is.na(dogs_data$WT01), "0", "A")
dogs_data$WT03 <- ifelse(is.na(dogs_data$WT03), "0", "B")
dogs_data$WT05 <- ifelse(is.na(dogs_data$WT05), "0", "C")
dogs_data$WT08 <- ifelse(is.na(dogs_data$WT08), "0", "D")
dogs_data$Weather_Type <- paste0(dogs_data$WT01, dogs_data$WT03, 
                                 dogs_data$WT05, dogs_data$WT08)
dogs_data$Weather_Type[dogs_data$Weather_Type == "0000"] <- 0
dogs_data$Weather_Type[dogs_data$Weather_Type == "A000"] <- 1
dogs_data$Weather_Type[dogs_data$Weather_Type == "0B00"] <- 2
dogs_data$Weather_Type[dogs_data$Weather_Type == "000D"] <- 3
dogs_data$Weather_Type[dogs_data$Weather_Type == "AB00"] <- 4
dogs_data$Weather_Type[dogs_data$Weather_Type == "A00D"] <- 5
dogs_data$Weather_Type[dogs_data$Weather_Type == "0BC0"] <- 6
dogs_data$Weather_Type[dogs_data$Weather_Type == "AB0D"] <- 7
dogs_data$Weather_Type <- as.character(dogs_data$Weather_Type)

dogs_data <- dogs_data %>% select(-c(WT01, WT03, WT05, WT08))



