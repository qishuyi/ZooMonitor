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
library(zoo)
library(tibble)
library(ggbeeswarm)

#Run Data cleaning script for each animal
source("dogs_cleaning.R")
source("cattle_cleaning.R")
source("sq_monkey_cleaning.R")

#Run column addition script
source("dogs_columns_addition.R")
source("cattle_columns_addition.R")
source("sq_monkey_columns_addition.R")






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

## Other's Percentage (in six-month intervals)
## Calculate percentage for each active behavior within each six-month interval
active_obs <- dogs_data %>% 
  filter(Activeness == 'Active') %>% 
  group_by(Six_Month_Interval, Activity) %>%
  summarize(n = n()) %>%
  mutate(Percentage = n/sum(n)*100)

## Calculate percentage for each inactive behavior within each six-month interval
inactive_obs <- dogs_data %>% 
  filter(Activeness == 'Inactive') %>% 
  group_by(Six_Month_Interval, Activity) %>%
  summarize(n = n()) %>%
  mutate(Percentage = n/sum(n)*100)

## Create stacked barplots for the percentages
ggplot(active_obs, aes(x=Six_Month_Interval, y=Percentage, fill=Activity)) +
  geom_col(position = "stack", width = 0.4) +
  labs(title = "Percentage of active behaviors (in 6-month intervals)", x = "Six Month Intervals", y = "Percentage of observations (%)") + 
  scale_fill_manual(values = c("coral","cornflowerblue", "darkolivegreen3", "darkorchid1", "darkcyan"))

ggplot(inactive_obs, aes(x=Six_Month_Interval, y=Percentage, fill=Activity)) +
  geom_col(position = "stack", width = 0.4) +
  labs(title = "Percentage of inactive behaviors (in 6-month intervals)", x = "Six Month Intervals", y = "Percentage of observations (%)") +
  scale_fill_manual(values = c("coral","cornflowerblue", "darkolivegreen3", "darkorchid1", "darkcyan"))

## Create barplots for the frequencies
active_obs2 <- dogs_data %>% filter(Activeness == 'Active')
inactive_obs2 <- dogs_data %>% filter(Activeness == "Inactive")
ggplot(data=active_obs2) + 
  geom_bar(aes(x=Activity), fill = "coral", alpha = 0.7) + 
  labs(title = "Frequency of recorded active behaviors (in 6-month intervals)", x = "Dogs' behavior when active", y = "Frequency") +
  facet_grid(. ~Six_Month_Interval)

ggplot(data=inactive_obs2) + 
  geom_bar(aes(x=Activity), fill = "coral", alpha = 0.7) + 
  labs(title = "Frequency of recorded inactive behaviors (in 6-month intervals)", x = "Dogs' behavior when inactive", y = "Frequency") +
  facet_grid((. ~Six_Month_Interval))

################## Activeness Through Week (V_H)

dogs_data_grouped <- group_by(dogs_data, Name, Day_of_Week, Activeness)
summary <- as.data.frame(summarise(dogs_data_grouped, n()))
names(summary)[names(summary) == "n()"] <- "counts"
summary <- summary %>% group_by(Name, Day_of_Week) %>%
  mutate(sum = sum(counts))
dogs_data_DW <- left_join(dogs_data, summary, by = c("Name", "Day_of_Week", "Activeness"))
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

################## Percentage of observations (Time of day)
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


################## Association B/W Food and Dog Behavior (Including Saturday)

#Vector for relabeling barplots
behavior_order <- c("Dog Int","Eating","Object Int", 
                    "Running", "Walking", "Alert", "Other", 
                    "Out of View", "Resting", "Sleeping")

#Vector for coloring labels
label_coloring <- rep(c("forestgreen","maroon"), times = c(5,5))

#Ground Meat (Including Saturday)
ground_meat <- ggplot(data = dogs_data %>% filter(Food == "Ground Meat"), aes(x = Activity)) + 
  geom_bar(aes(y = ..count..), fill = "steelblue") +
  labs(title = "Bar Plot of Dog Behavior (Per Hour of Day)", subtitle  = "Food: Ground Meat"
       , y = "Frequency") + facet_grid(. ~ Hour) +
  theme(axis.text.x = element_text(angle = 90, color = label_coloring)) +
  scale_x_discrete(limits = behavior_order) 
 

#Bones
bones <- ggplot(data = dogs_data %>% filter(Food == "Bones"), aes(x = Activity)) + 
  geom_bar(aes(y = ..count..), fill = "steelblue2", width = .55) +
  labs(title = "Bar Plot of Dog Behavior (Per Hour of Day)", subtitle  = "Food: Bones"
       , y = "Frequency") + facet_grid(. ~ Hour) +
  theme(axis.text.x = element_text(angle = 90, color = label_coloring)) +
  scale_y_continuous(limits = c(0,500)) +
  scale_x_discrete(limits = behavior_order) 



#Plotting Food Graph (Including Saturday)
grid.arrange(ground_meat, bones, nrow = 2)


################### Association B/W Food and Dog Behavior (NOT Including Saturday)


#Ground Meat (NOT Including Saturday)
ground_meat_nosat <- ggplot(data = dogs_data %>% filter(Food == "Ground Meat", Day_of_Week != "Sat"), aes(x = Activity)) + 
  geom_bar(aes(y = ..count..), fill = "steelblue") +
  labs(title = "Bar Plot of Dog Behavior (Per Hour of Day)" 
       , subtitle  = "Food: Ground Meat (Excluding Saturdays)"
       , y = "Frequency") + facet_grid(. ~ Hour) +
  theme(axis.text.x = element_text(angle = 90, color = label_coloring)) +
  scale_y_continuous(limits = c(0,200)) + 
  scale_x_discrete(limits = behavior_order) 
 

#Bones
bones_nosat <- ggplot(data = dogs_data %>% filter(Food == "Bones"), aes(x = Activity)) + 
  geom_bar(aes(y = ..count..), fill = "steelblue2", width = .65) +
  labs(title = "Bar Plot of Dog Behavior (Per Hour of Day)", subtitle  = "Food: Bones"
       , y = "Frequency") + facet_grid(. ~ Hour) +
  theme(axis.text.x = element_text(angle = 90, color = label_coloring)) +
  scale_y_continuous(limits = c(0,200)) +
  scale_x_discrete(limits = behavior_order) 



#Plotting Food Graph (NOT Including Saturday)
grid.arrange(ground_meat_nosat, bones_nosat, nrow = 2)


#################### Association Between Dogs' Behavior with Temperature Level(Using TAVG_TMAX_avg)
dogs_data_grouped2 <- group_by(dogs_data, Activity,Temp_Level)
summary2 <- as.data.frame(summarise(dogs_data_grouped2, n()))
names(summary2)[names(summary2) == "n()"] <- "counts"
summary2 <- summary2 %>% group_by(Activity) %>%
  mutate(sum = sum(counts)) %>% 
  mutate(percent = counts/sum*100)
summary2$percent <- round(summary2$percent, digits = 1)
summary2 <- summary2 %>% mutate(percent_sub = paste(percent, "%"))

##Activity by Temperature Level Plot (TAVG_TMAX_avg)
ggplot(data = summary2, aes(x = Temp_Level, y = counts, fill = Temp_Level), show.legend = FALSE) + 
  geom_bar(stat = "identity", position=position_dodge()) + 
  facet_grid(.~ Activity) +
  labs(title = "Activity Based on Temperature Level", subtitle = "with Temperature Grouped into 10 Levels", x = "Temperature", y="Counts", fill = "Temperature Level") 

#Temperature Level Reference Table (TAVG_TMAX_avg)
Temp_Level_Reference <- as.data.frame(quantile(dogs_data$TAVG_TMAX_avg, 0:10/10))
names(Temp_Level_Reference)[names(Temp_Level_Reference) == "quantile(dogs_data$TAVG_TMAX_avg, 0:10/10)"] <- "Temperature (Fº)"
Temp_Level_Reference$`Temperature (Fº)` <- round(Temp_Level_Reference$`Temperature (Fº)`)
Temp_Level_Reference[1, 1] = "34 ~ 40"
Temp_Level_Reference[2, 1] = "41 ~ 45"
Temp_Level_Reference[3, 1] = "46 ~ 53"
Temp_Level_Reference[4, 1] = "54 ~ 59"
Temp_Level_Reference[5, 1] = "60 ~ 65"
Temp_Level_Reference[6, 1] = "66 ~ 69"
Temp_Level_Reference[7, 1] = "70 ~ 77"
Temp_Level_Reference[8, 1] = "78 ~ 81"
Temp_Level_Reference[9, 1] = "82 ~ 85"
Temp_Level_Reference[10, 1] = "86 ~ 94"
Temp_Level_Reference <- slice(Temp_Level_Reference, 1:10)
Temp_Level_Reference <- mutate(Temp_Level_Reference, Percent = rownames(Temp_Level_Reference))
Temp_Level_Reference <- mutate(Temp_Level_Reference, Level = rownames(Temp_Level_Reference))
Temp_Level_Reference <- Temp_Level_Reference %>% select(-Percent) 
Temp_Level_Reference <- Temp_Level_Reference[, c(2, 1)]
formattable(Temp_Level_Reference, align = c("l", "r"))

#################### Association Between Dogs' Behavior with Temperature (Using TAVG_TMAX_avg)
dogs_data_sub <- dogs_data %>% filter(!(Activity == "Eating" | Activity == "Dog Int" | Activity == "Object Int"))

##Activity by Numeric Temperature Plot(Violin)
ggplot(dogs_data_sub, aes(x = Activity, y = TAVG_TMAX_avg)) +
  geom_violin() +
  labs(title = "Activity Based on Temperature", subtitle = "with Continuous Temperature", x = "Activity", y = "Temperature (F°)")

##Activity by Temperature Plot (TAVG_TMAX_avg) (beeswarm)
ggplot(dogs_data, aes(x = TAVG_TMAX_avg, y = Activity, color = Activity)) +
  geom_quasirandom(groupOnX=FALSE) +
  labs(title = "Activity Based on Temperature", subtitle = "with Continuous Temperature", x = "Temperature (F°)", y = "Activity")

#################### Association Between Dogs' Behavior with Weather
dogs_data_grouped3 <- group_by(dogs_data, Weather_Type, Activeness)
summary3 <- as.data.frame(summarise(dogs_data_grouped3, n()))
names(summary3)[names(summary3) == "n()"] <- "counts"
summary3 <- summary3 %>% group_by(Weather_Type) %>%
  mutate(sum = sum(counts)) %>% 
  mutate(percent = counts/sum*100)
summary3$percent <- round(summary3$percent, digits = 1)
summary3 <- summary3 %>% mutate(percent_sub = paste(percent, "%"))
##Activity by Weather Type Plot
ggplot(summary3, aes(x="", y=counts, fill=Activeness)) +
  geom_bar(stat="identity", position = "dodge") +
  facet_grid(.~Weather_Type) +
  geom_text(aes(label = percent_sub), position = position_dodge(width = 0.9), size = 2.7) +
  labs(title = "Activeness by Weather Type", x = "Activeness", y="Counts") 
##Weather Type Reference
Weather_Type_Reference <- as.data.frame(0:7)
names(Weather_Type_Reference)[names(Weather_Type_Reference) == "0:7"] <- "Weather_Type"
Weather_Type_Reference <- cbind(Weather_Type_Reference, Weather_Description = Weather_Type_Reference$Weather_Type)
for (a in c("0", "1", "2", "3", "4", "5", "6", "7"))
  if(a == "0"){
    Weather_Type_Reference$Weather_Description[Weather_Type_Reference$Weather_Description == "0"] <- "No Special Weather Type"
    } else if(a == "1"){
     Weather_Type_Reference$Weather_Description[Weather_Type_Reference$Weather_Description == "1"] <- "Fog"
    } else if(a == "2"){
     Weather_Type_Reference$Weather_Description[Weather_Type_Reference$Weather_Description == "2"] <- "Thunder"
    } else if(a == "3"){
     Weather_Type_Reference$Weather_Description[Weather_Type_Reference$Weather_Description == "3"] <- "Smoke or haze"
    } else if(a == "4"){
     Weather_Type_Reference$Weather_Description[Weather_Type_Reference$Weather_Description == "4"] <- "Fog and Thunder"
    } else if(a == "5"){
     Weather_Type_Reference$Weather_Description[Weather_Type_Reference$Weather_Description == "5"] <- "Fog and Smoke or haze"
    } else if(a == "6"){
     Weather_Type_Reference$Weather_Description[Weather_Type_Reference$Weather_Description == "6"] <- "Thunder and Hail"
    } else{
     Weather_Type_Reference$Weather_Description[Weather_Type_Reference$Weather_Description == "7"] <- "Fog and Thunder and Smoke or Haze"
    }
names(Weather_Type_Reference)[names(Weather_Type_Reference) == "Weather_Type"] <- "Weather Type"
names(Weather_Type_Reference)[names(Weather_Type_Reference) == "Weather_Description"] <- "Weather Description" 
formattable(Weather_Type_Reference, align = c("l", "r"))
  
  
################## Association B/W Events and Dog Behavior
## Compare dogs' behavior between zoo boo and brew at the zoo
## Compare dogs' behavior on the day of the event with the same day of week during the same season
## Brew at the zoo (4/27)
spring_sat_2019_data <- dogs_data %>% 
  filter (Season == "Spring", Year == 2019, Day_of_Week == "Sat", Hour == 10) %>%
  mutate(Events = ifelse(is.na(Notes), "No event", Notes))

spring_percentage_dog <- spring_sat_2019_data %>% 
  group_by(Name, Events, Activity) %>%
  summarize(n = n()) %>%
  mutate(Percentage = n/sum(n)*100)

## Zoo Boo (10/26)
fall_sat_2019_data <- dogs_data %>% 
  filter (Season == "Fall", Year == 2019, Day_of_Week == "Sat", Hour == 9) %>%
  mutate(Events = ifelse(is.na(Notes), "No event", Notes))

fall_percentage_dog <- fall_sat_2019_data %>% 
  group_by(Name, Events, Activity) %>%
  summarize(n = n()) %>%
  mutate(Percentage = n/sum(n)*100)

ggplot(data=spring_percentage_dog, aes(x=Events, y=Percentage, fill=Activity)) + 
  geom_col(position = "stack", width = 0.4) + 
  labs(title = "Percentage of behaviors on Saturdays in spring 2019 between 10:00~10:59am", x = "Events", y = "Percentage") +
  facet_grid(. ~ Name) +
  scale_fill_manual(values = c("khaki", "coral","cornflowerblue", "darkolivegreen3", "darkorchid1", "darkcyan"))

ggplot(data=fall_percentage_dog, aes(x=Events, y=Percentage, fill=Activity)) + 
  geom_col(position = "stack", width = 0.4) + 
  labs(title = "Percentage of behaviors on Saturdays in fall 2019 between 9:00~9:59am", x = "Events", y = "Percentage") +
  facet_grid(. ~ Name) +
  scale_fill_manual(values = c("khaki", "coral","cornflowerblue", "darkolivegreen3", "darkorchid1", "darkcyan", "aquamarine", "mediumvioletred"))


################## For 3/31/2020


################## (MONKEY) Head Spin per Monkey Visual

obs_per_monkey <- c(638,234,594,620,256,178)

#Raw Frequency Version
ggplot(data = sq_monkey_data %>% filter(Activity == "Head spin")) +
  geom_bar(aes(x = Name), fill = "springgreen3") +
  labs(title = "Raw Frequency of Head Spin per Monkey", x = "Monkey", y = "Frequency") 


#Percentage (of each monkey) Version
ggplot(data = sq_monkey_data %>% filter(Activity == "Head spin")) +
  geom_bar(aes(x = Name, y= ..count.. / obs_per_monkey), fill = "turquoise3") +
  labs(title = "Percentage of Head Spin per Monkey", 
       subtitle = "Percentage based on each monkey's total number of observations",
       x = "Monkey", y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 9, face = "italic"))

#change x and y axis laberling
#change % label decimals for y axis
#Add count to top of bar


################## (CATTLE) Positive Behavior Visual






################## Death/Birth Exploration




























################## Active vs. Inactive plot (cattle)

cattle_data_grouped <- group_by(cattle_data, Hour, Category)
summary <- as.data.frame(summarise(cattle_data_grouped, n()))
names(summary)[names(summary) == "n()"] <- "counts"
summary <- summary %>% group_by(Name, Hour) %>%
  mutate(sum = sum(counts))
cattle_data_hour <- left_join(cattle_data, summary, by = c("Name", "Day_of_Week", "Activeness"))
dogs_data_DW <- mutate(dogs_data_DW, percent = dogs_data_DW$counts/dogs_data_DW$sum*100)
dogs_data_DW$percent <- round(dogs_data_DW$percent, digits = 1)
dogs_data_DW$percent <- paste(dogs_data_DW$percent, "%")
##Plots of activeness by dogs by day
ggplot(dogs_data_DW, aes(x = Day_of_Week, y = counts, fill = Activeness)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x = "Day of Week", y="Counts") +
  geom_text(aes(label = percent), position = position_dodge(width = 0.9), size = 4) +
  facet_grid(Name ~.) 









################## Death/Birth Exploration (sq_monkey)

#2018-02-05 --> 2018-02-22 (on the day he died) --> 2018-03-04 (Damian)
#2018-11-11 --> no ob. on 2018-12-03 (on the day she died) --> 2019-01-03 (Pistachio)
#2019-03-17 --> no ob. on 2019-06-13 (on the day he came) --> 2019-06-18









