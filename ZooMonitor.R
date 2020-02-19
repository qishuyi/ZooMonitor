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

#Run Data cleaning script
source("cleaning.R")


##################Adding Columns

##Activeness Column
dogs_data <- dogs_data %>% unite("Activeness", c(IC1_Name, IC2_Name), remove = T)
dogs_data <- dogs_data %>% mutate(Activeness = gsub("NA_", "", Activeness))
dogs_data <- dogs_data %>% mutate(Activeness = gsub("_NA", "", Activeness))       

##Activity Column
dogs_data <- dogs_data %>% unite("Activity", c(IC1_Value, IC2_Value), remove = T)
dogs_data <- dogs_data %>% filter(str_detect(Activity, "NA") == TRUE) 
dogs_data <- dogs_data %>% mutate(Activity = gsub("NA_", "", Activity))
dogs_data <- dogs_data %>% mutate(Activity = gsub("_NA", "", Activity))                                  
dogs_data <- dogs_data %>% mutate(Activity = gsub("Dog interaction", "Dog Int", Activity))
dogs_data <- dogs_data %>% mutate(Activity = gsub("Interacting with object", "Obj Int", Activity))

##Day_of_Week Column
dogs_data <- mutate(dogs_data, Day_of_Week = wday(Date, label = TRUE))

##Weather Column

##Temperature Column



##################Plot of Activity of Dogs in each Hour (V_R)

#Subsetting Data for each Dog
dogs_data_Hunter <- dogs_data %>% filter(Name == "Hunter")
dogs_data_Minzi <- dogs_data %>% filter(Name == "Minzi")
dogs_data_Akilah <- dogs_data %>% filter(Name == "Akilah")
dogs_data_Amara <- dogs_data %>% filter(Name == "Amara")
dogs_data_JT <- dogs_data %>% filter(Name == "JT")

##Plot of Activity of Dogs in each Hour
ggplot(data = dogs_data) + geom_bar(aes(x = Activity), fill = "salmon") + facet_grid(Name ~ Hour) + theme(axis.text.x = element_text(angle = 90)) + labs(title = "Bar Plots of Activity (Dog/Time)", y = "Frequency")

##Plot of Activity of Dogs in each Hour (Individual)
ggplot(data = dogs_data_Hunter) + geom_bar(aes(x = Activity), fill = "salmon") + facet_grid(. ~ Hour) + theme(axis.text.x = element_text(angle = 90)) + labs(title = "Hunter: Barplots of Activity per Hour", y = "Frequency")
ggplot(data = dogs_data_Minzi) + geom_bar(aes(x = Activity), fill = "salmon") + facet_grid(. ~ Hour) + theme(axis.text.x = element_text(angle = 90)) + labs(title = "Minzi: Barplots of Activity per Hour", y = "Frequency")
ggplot(data = dogs_data_Akilah) + geom_bar(aes(x = Activity), fill = "salmon") + facet_grid(. ~ Hour) + theme(axis.text.x = element_text(angle = 90)) + labs(title = "Akilah: Barplots of Activity per Hour", y = "Frequency")
ggplot(data = dogs_data_Amara) + geom_bar(aes(x = Activity), fill = "salmon") + facet_grid(. ~ Hour) + theme(axis.text.x = element_text(angle = 90)) + labs(title = "Amara: Barplots of Activity per Hour", y = "Frequency")
ggplot(data = dogs_data_JT) + geom_bar(aes(x = Activity), fill = "salmon") + facet_grid(. ~ Hour) + theme(axis.text.x = element_text(angle = 90)) + labs(title = "JT: Barplots of Activity per Hour", y = "Frequency")

##################Other's Frequency (V_S)

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


##################Activeness Through Week (V_H)

#Grouping the data by name, day of week, and activeness
dogs_data_grouped <- group_by(dogs_data, Name, Day_of_Week, Activeness)

#Making the summary a data frame
summary <- as.data.frame(summarise(dogs_data_grouped, n()))
names(summary)[names(summary) == "n()"] <- "counts"

#group_by summarise data table and making the sum of counts by each dog each day
summary <- summary %>% group_by(Name, Day_of_Week) %>%
  mutate(sum = sum(counts))

#Merging the summary data frame into the dogs data frame to have the column "coutns"
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
  facet_grid(Name ~ .) 
  


##################For 3/3/2020

#Incorporate Day of Week into the data frame (Df with Activity Column)
dogs_data_comp <- dogs_data
dogs_data_comp <- dogs_data_comp %>% mutate(Day_of_Week = wday(Date, label = T))

#Incorporate Food into the comp data frame
dogs_data_comp <- cbind(dogs_data_comp, Food = dogs_data_comp$Day_of_Week) 
dogs_data_comp$Food <- as.character(dogs_data_comp$Food)

for(day in c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")){
  
  if(day == "Fri"){
    dogs_data_comp$Food[dogs_data_comp$Food == day] <- "Guinea Pig"
  }
  
  else if(day == "Wed" | day == "Sat"){
    dogs_data_comp$Food[dogs_data_comp$Food == day] <- "Bones"
  }
  
  else{
    dogs_data_comp$Food[dogs_data_comp$Food == day] <- "Ground Meat"
  }
}


#Percentage of observations (Time of day)
time_of_day_viz <- ggplot(data = dogs_data_comp, aes(x = Hour)) + 
  geom_bar(aes(y = ..count../nrow(dogs_data_comp)*100), fill = "steelblue", width = .75) + 
  scale_x_discrete(limits = 9:16) +
  scale_y_continuous(limits = c(0,25)) + 
  geom_hline(yintercept = (1/8)*100, color = "darkmagenta", alpha = .45, linetype = "longdash") +
  labs(title = "Percentage of Observations (Per Time of Day)", x = "Time of Day", y = "Percentage (%)") +
  annotate("text", x= 16.6, y = 13.5 , label = "12.5%", color = "darkmagenta", size = 3.25)


day_of_week_viz <- ggplot(data = dogs_data_comp, aes(x = Day_of_Week)) +
  geom_bar(aes(y = ..count../nrow(dogs_data_comp)*100), fill = "steelblue2", width = .75) +
  scale_x_discrete(limits=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  scale_y_continuous(limits = c(0,60)) +
  labs(title = "Percentage of Observations (Per Day of Week)", x = "Day of Week", y = "Percentage (%)") +
  geom_hline(yintercept = (1/7)*100, color = "darkmagenta", alpha = .45, linetype = "longdash") + 
  annotate("text", x= 1.5, y = 16.5 , label = "14.28%", color = "darkmagenta", size = 3.25)

grid.arrange(day_of_week_viz, time_of_day_viz,  nrow = 1)


#Association B/W Food and Dog Behavior

#Bones
ggplot(data = dogs_data_comp %>% filter(Food == "Bones"), aes(x = Activity)) + 
  geom_bar(aes(y = ..count.. /nrow(dogs_data_comp %>% filter(Food == "Bones"))*100, fill = Day_of_Week)) +
  labs(title = "Bar Plot of Dog Behavior", subtitle  = "Food: Bones"
       , y = "Percentage (%)")

#Ground Meat
ggplot(data = dogs_data_comp %>% filter(Food == "Ground Meat"), aes(x = Activity)) + 
  geom_bar(aes(y = ..count.. / nrow(dogs_data_comp %>% filter(Food == "Ground Meat")) *100), 
           fill = "steelblue2") +
  labs(title = "Bar Plot of Dog Behavior", subtitle  = "Food: Ground Meat"
       , y = "Percentage (%)")

#Guinea Pigs (Not a large enough sample)
ggplot(data = dogs_data_comp %>% filter(Food == "Guinea Pig"), aes(x = Activity)) + 
  geom_bar(aes(y = ..count.. / nrow(dogs_data_comp %>% filter(Food == "Guinea Pig")) *100))


ggplot(data = dogs_data_comp) + geom_bar(aes(x = Activity, fill = Day_of_Week))


