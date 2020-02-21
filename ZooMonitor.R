#Importing Libraries
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(grid)
library(formattable) #To make a nice table

#Run Data cleaning script
source("cleaning.R")

################## Master Data Frame

##Activeness Column
dogs_data <- dogs_data %>% unite("Activeness", c(IC1_Name, IC2_Name), remove = T)
dogs_data <- dogs_data %>% mutate(Activeness = gsub("NA_", "", Activeness))
dogs_data <- dogs_data %>% mutate(Activeness = gsub("_NA", "", Activeness))       

##Activity Column and Changing Long Column Names
dogs_data <- dogs_data %>% unite("Activity", c(IC1_Value, IC2_Value), remove = T)
dogs_data <- dogs_data %>% mutate(Activity = gsub("NA_", "", Activity))
dogs_data <- dogs_data %>% mutate(Activity = gsub("_NA", "", Activity))                                  

##Changing Name of Behavior in Activity Column
dogs_data <- dogs_data %>% mutate(Activity = gsub("Dog interaction", "Dog Int", Activity))
dogs_data <- dogs_data %>% mutate(Activity = gsub("Interacting with object", "Object Int", Activity))
dogs_data <- dogs_data %>% mutate(Activity = gsub("Walking Around", "Walking", Activity))
dogs_data <- dogs_data %>% mutate(Activity = gsub("Out of view", "Out of View", Activity))

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
weather_temp <- read_csv("Weather_Temperature.csv")
weather_temp$DATE <- as.Date(weather_temp$DATE, format = "%m/%d/%y")
weather_temp <- weather_temp %>% select(-c(STATION, NAME, TOBS))
weather_temp <- weather_temp %>%
  mutate(TAVG_C = round((TAVG-32)*5/9)) %>%
  mutate(TMAX_C = round((TMAX-32)*5/9)) %>%
  mutate(TMIN_C = round((TMIN-32)*5/9))
weather_temp <- weather_temp %>% slice(1:717)

#Merge/Further Cleaning
dogs_data <- left_join(dogs_data, weather_temp, by = c("Date" = "DATE"))
dogs_data <- dogs_data %>% select(-c(WT02, WT06, WT07, WT09, WT11))

##Average Temperature Level Column
dogs_data <- within(dogs_data, Temp_Level <- as.integer(cut(TAVG, quantile(TAVG, probs=0:10/10), include.lowest=TRUE)))
dogs_data$Temp_Level <- as.character(dogs_data$Temp_Level)
dogs_data$Temp_Level <- factor(dogs_data$Temp_Level, c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

##Average of Average-max Temperature column and its temperature level
dogs_data <- mutate(dogs_data, TAVG_TMAX_avg = (dogs_data$TMAX + dogs_data$TAVG)/2)
dogs_data <- within(dogs_data, Temp_Level2 <- as.integer(cut(TAVG_TMAX_avg, quantile(TAVG_TMAX_avg, probs=0:10/10), include.lowest=TRUE)))
dogs_data$Temp_Level2 <- as.character(dogs_data$Temp_Level2)
dogs_data$Temp_Level2 <- factor(dogs_data$Temp_Level2, c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))







################# For 2/18/2020

################## Plot of Activity of Dogs in each Hour (V_R)

#Subsetting Data for each Dog
dogs_data_Hunter <- dogs_data %>% filter(Name == "Hunter")
dogs_data_Minzi <- dogs_data %>% filter(Name == "Minzi")
dogs_data_Akilah <- dogs_data %>% filter(Name == "Akilah")
dogs_data_Amara <- dogs_data %>% filter(Name == "Amara")
dogs_data_JT <- dogs_data %>% filter(Name == "JT")

##Plot of Activity of Dogs in each Hour
ggplot(data = dogs_data) + geom_bar(aes(x = Activity), fill = "salmon") + facet_grid(Name ~Hour) + theme(axis.text.x = element_text(angle = 90)) + labs(title = "Bar Plots of Activity (Dog/Time)", y = "Frequency")

##Plot of Activity of Dogs in each Hour (Individual)
ggplot(data = dogs_data_Hunter) + geom_bar(aes(x = Activity), fill = "salmon") + facet_grid(. ~Hour) + theme(axis.text.x = element_text(angle = 90)) + labs(title = "Hunter: Barplots of Activity per Hour", y = "Frequency")
ggplot(data = dogs_data_Minzi) + geom_bar(aes(x = Activity), fill = "salmon") + facet_grid(. ~Hour) + theme(axis.text.x = element_text(angle = 90)) + labs(title = "Minzi: Barplots of Activity per Hour", y = "Frequency")
ggplot(data = dogs_data_Akilah) + geom_bar(aes(x = Activity), fill = "salmon") + facet_grid(.~ Hour) + theme(axis.text.x = element_text(angle = 90)) + labs(title = "Akilah: Barplots of Activity per Hour", y = "Frequency")
ggplot(data = dogs_data_Amara) + geom_bar(aes(x = Activity), fill = "salmon") + facet_grid(. ~Hour) + theme(axis.text.x = element_text(angle = 90)) + labs(title = "Amara: Barplots of Activity per Hour", y = "Frequency")
ggplot(data = dogs_data_JT) + geom_bar(aes(x = Activity), fill = "salmon") + facet_grid(. ~Hour) + theme(axis.text.x = element_text(angle = 90)) + labs(title = "JT: Barplots of Activity per Hour", y = "Frequency")

################## Other's Frequency (V_S)

# Group by Inactive behavior
inactive_prop <- 
  dogs_data %>%
  filter(Activeness == "Inactive") %>%
  group_by(Activity) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n) * 100)

active_prop <- 
  dogs_data %>%
  filter(Activeness == "Active") %>%
  group_by(Activity) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n) * 100)

## Other's Frequency
ggplot(data=active_prop, aes(x=Activity, y=freq)) + geom_bar(fill = "coral", alpha = 0.7, stat = "identity") + labs(title = "Percentage of active behaviors", x = "Dogs' behavior when active", y = "Percentage (%)")
ggplot(data=inactive_prop, aes(x=Activity, y=freq)) + geom_bar(fill = "coral", alpha = 0.7, stat = "identity") + labs(title = "Percentage of inactive behaviors", x = "Dogs' behavior when inactive", y = "Percentage (%)")


################## Activeness Through Week (V_H)

#Grouping the data by name, day of week, and activeness
dogs_data_grouped <- group_by(dogs_data, Name, Day_of_Week, Activeness)

#Making the summary a data frame
summary <- as.data.frame(summarise(dogs_data_grouped, n()))
names(summary)[names(summary) == "n()"] <- "counts"

#group_by summarise data table and making the sum of counts by each dog each day
summary <- summary %>% group_by(Name, Day_of_Week) %>%
  mutate(sum = sum(counts))

#Merging the summary data frame into the dogs data frame to have the column "counts"
dogs_data_DW <- left_join(dogs_data, summary, by = c("Name", "Day_of_Week", "Activeness"))

#Creating a percentage column
dogs_data_DW <- mutate(dogs_data_DW, percent = dogs_data_DW$counts/dogs_data_DW$sum*100)
dogs_data_DW$percent <- round(dogs_data_DW$percent, digits = 1)
dogs_data_DW$percent <- paste(dogs_data_DW$percent, "%")

##Plots of activeness by dogs by day
ggplot(dogs_data_DW, aes(x = Day_of_Week, y = counts, fill = Activeness)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x = "Day of Week", y="Counts") +
  geom_text(aes(label = percent), position = position_dodge(width = 0.9), size = 4) +
  facet_grid(Name ~.) 
  


################## For 3/3/2020

#Percentage of observations (Time of day)
time_of_day_viz <- ggplot(data = dogs_data, aes(x = Hour)) + 
  geom_bar(aes(y = ..count../nrow(dogs_data)*100), fill = "steelblue", width = .75) + 
  scale_x_discrete(limits = 9:16) +
  scale_y_continuous(limits = c(0,25)) + 
  geom_hline(yintercept = (1/8)*100, color = "darkmagenta", alpha = .45, linetype = "longdash") +
  labs(title = "Percentage of Observations (Per Time of Day)", x = "Time of Day", y = "Percentage (%)") +
  annotate("text", x= 16.6, y = 13.5 , label = "12.5%", color = "darkmagenta", size = 3.25)


day_of_week_viz <- ggplot(data = dogs_data, aes(x = Day_of_Week)) +
  geom_bar(aes(y = ..count../nrow(dogs_data)*100), fill = "steelblue2", width = .75) +
  scale_x_discrete(limits=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  scale_y_continuous(limits = c(0,60)) +
  labs(title = "Percentage of Observations (Per Day of Week)", x = "Day of Week", y = "Percentage (%)") +
  geom_hline(yintercept = (1/7)*100, color = "darkmagenta", alpha = .45, linetype = "longdash") + 
  annotate("text", x= 1.5, y = 16.5 , label = "14.28%", color = "darkmagenta", size = 3.25)

grid.arrange(day_of_week_viz, time_of_day_viz,  nrow = 1)


#Association B/W Food and Dog Behavior

#Vector for relabeling barplots
behavior_order <- c("Dog Int","Eating","Object Int", 
                    "Running", "Walking", "Alert", "Other", 
                    "Out of View", "Resting", "Sleeping")

#Ground Meat
ground_meat <- ggplot(data = dogs_data %>% filter(Food == "Ground Meat"), aes(x = Activity)) + 
  geom_bar(aes(y = ..count..), fill = "steelblue") +
  labs(title = "Bar Plot of Dog Behavior (Per Hour of Day)", subtitle  = "Food: Ground Meat"
       , y = "Frequency") + facet_grid(. ~ Hour) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete(limits = behavior_order)

#Bones
bones <- ggplot(data = dogs_data %>% filter(Food == "Bones"), aes(x = Activity)) + 
  geom_bar(aes(y = ..count..), fill = "purple2", width = .55) +
  labs(title = "Bar Plot of Dog Behavior (Per Hour of Day)", subtitle  = "Food: Bones"
       , y = "Frequency") + facet_grid(. ~ Hour) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(limits = c(0,500)) +
  scale_x_discrete(limits = behavior_order) 


#Plotting Food Graph
grid.arrange(ground_meat, bones, nrow = 2)


#Guinea Pigs (Not a large enough sample)
ggplot(data = dogs_data %>% filter(Food == "Guinea Pig"), aes(x = Activity)) + 
  geom_bar(aes(y = ..count.. / nrow(dogs_data %>% filter(Food == "Guinea Pig")) *100))




#################### Association Between Dogs' Behavior with Temperature(Using TAVG)
dogs_data_grouped2 <- group_by(dogs_data, Temp_Level, Activeness)
summary2 <- as.data.frame(summarise(dogs_data_grouped2, n()))
names(summary2)[names(summary2) == "n()"] <- "counts"
summary2 <- summary2 %>% group_by(Temp_Level) %>%
  mutate(sum = sum(counts))
dogs_data_TL <- left_join(dogs_data, summary2, by = c("Temp_Level", "Activeness"))
dogs_data_TL <- mutate(dogs_data_TL, percent = dogs_data_TL$counts/dogs_data_TL$sum*100)
dogs_data_TL$percent <- round(dogs_data_TL$percent, digits = 1)
dogs_data_TL$percent <- paste(dogs_data_TL$percent, "%")

##Activity by Temperature Plot (TAVG)
a <- ggplot(data = dogs_data_TL) + 
  geom_bar(aes(x = Temp_Level), fill = "salmon") + 
  facet_grid(.~ Activity) + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Activity Based on Temperature (TAVG)", x = "Temperature", y="Counts")

#Temperature Level Reference Table (TAVG)
Temp_Level_Reference <- as.data.frame(quantile(dogs_data$TAVG, 0:10/10))
names(Temp_Level_Reference)[names(Temp_Level_Reference) == "quantile(dogs_data$TAVG, 0:10/10)"] <- "Temperature (Fº)"
Temp_Level_Reference$`Temperature (Fº)` <- round(Temp_Level_Reference$`Temperature (Fº)`)
Temp_Level_Reference[1, 1] = "21 ~ 33"
Temp_Level_Reference[2, 1] = "34 ~ 38"
Temp_Level_Reference[3, 1] = "39 ~ 45"
Temp_Level_Reference[4, 1] = "46 ~ 50"
Temp_Level_Reference[5, 1] = "51 ~ 57"
Temp_Level_Reference[6, 1] = "58 ~ 62"
Temp_Level_Reference[7, 1] = "63 ~ 70"
Temp_Level_Reference[8, 1] = "70 ~ 72"
Temp_Level_Reference[9, 1] = "73 ~ 77"
Temp_Level_Reference[10, 1] = "78 ~ 86"
Temp_Level_Reference <- slice(Temp_Level_Reference, 1:10)
Temp_Level_Reference <- mutate(Temp_Level_Reference, Percent = rownames(Temp_Level_Reference))
Temp_Level_Reference <- mutate(Temp_Level_Reference, Level = rownames(Temp_Level_Reference))
Temp_Level_Reference <- Temp_Level_Reference %>% select(-Percent) 
Temp_Level_Reference <- Temp_Level_Reference[, c(2, 1)]
formattable(Temp_Level_Reference)

 


#################### Association Between Dogs' Behavior with Temperature(Using TAVG_TMAX_avg)
dogs_data_grouped3 <- group_by(dogs_data, Temp_Level2, Activeness)
summary3 <- as.data.frame(summarise(dogs_data_grouped3, n()))
names(summary3)[names(summary3) == "n()"] <- "counts"
summary3 <- summary3 %>% group_by(Temp_Level2) %>%
  mutate(sum = sum(counts))
dogs_data_TL2 <- left_join(dogs_data, summary3, by = c("Temp_Level2", "Activeness"))
dogs_data_TL2 <- mutate(dogs_data_TL2, percent = dogs_data_TL2$counts/dogs_data_TL2$sum*100)
dogs_data_TL2$percent <- round(dogs_data_TL2$percent, digits = 1)
dogs_data_TL2$percent <- paste(dogs_data_TL2$percent, "%")

##Activity by Temperature Plot (TAVG_TMAX_avg)
b <- ggplot(data = dogs_data_TL2) + 
  geom_bar(aes(x = Temp_Level2), fill = "salmon") + 
  facet_grid(.~ Activity) + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Activity Based on Temperature (TAVG_TMAX_avg)", x = "Temperature", y="Counts")

#Temperature Level Reference Table (TAVG_TMAX_avg)
Temp_Level_Reference2 <- as.data.frame(quantile(dogs_data$TAVG_TMAX_avg, 0:10/10))
names(Temp_Level_Reference2)[names(Temp_Level_Reference2) == "quantile(dogs_data$TAVG_TMAX_avg, 0:10/10)"] <- "Temperature (Fº)"
Temp_Level_Reference2$`Temperature (Fº)` <- round(Temp_Level_Reference2$`Temperature (Fº)`)
Temp_Level_Reference2[1, 1] = "34 ~ 40"
Temp_Level_Reference2[2, 1] = "41 ~ 45"
Temp_Level_Reference2[3, 1] = "46 ~ 53"
Temp_Level_Reference2[4, 1] = "54 ~ 59"
Temp_Level_Reference2[5, 1] = "60 ~ 65"
Temp_Level_Reference2[6, 1] = "66 ~ 69"
Temp_Level_Reference2[7, 1] = "70 ~ 77"
Temp_Level_Reference2[8, 1] = "78 ~ 81"
Temp_Level_Reference2[9, 1] = "82 ~ 85"
Temp_Level_Reference2[10, 1] = "86 ~ 94"
Temp_Level_Reference2 <- slice(Temp_Level_Reference2, 1:10)
Temp_Level_Reference2 <- mutate(Temp_Level_Reference2, Percent = rownames(Temp_Level_Reference2))
Temp_Level_Reference2 <- mutate(Temp_Level_Reference2, Level = rownames(Temp_Level_Reference2))
Temp_Level_Reference2 <- Temp_Level_Reference2 %>% select(-Percent) 
Temp_Level_Reference2 <- Temp_Level_Reference2[, c(2, 1)]
formattable(Temp_Level_Reference2)

##Combined Visuals (TAVG and TAVG_TMAX_avg)
grid.arrange(a, b, nrow = 2)


