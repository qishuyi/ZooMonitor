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

#Creating an Activity Column for the data
dogs_data_visual <- dogs_data %>% unite("Activity", c(IC1_Value, IC2_Value), remove = T)                              
#dogs_data_visual <- dogs_data_visual %>% filter(str_detect(Activity, "NA") == TRUE) 
dogs_data_visual <- dogs_data_visual %>% mutate(Activity = gsub("NA_", "", Activity))
dogs_data_visual <- dogs_data_visual %>% mutate(Activity = gsub("_NA", "", Activity))                                  
dogs_data_visual <- dogs_data_visual %>% mutate(Activity = gsub("Dog interaction", "Dog Int", Activity))
dogs_data_visual <- dogs_data_visual %>% mutate(Activity = gsub("Interacting with object", "Obj Int", Activity))


#Creating an Activeness Column for the data 
dogs_data_visual <- dogs_data_visual %>% unite("Activeness", c(IC1_Name, IC2_Name), remove = T)
dogs_data_visual <- dogs_data_visual %>% mutate(Activeness = gsub("NA_", "", Activeness))
dogs_data_visual <- dogs_data_visual %>% mutate(Activeness = gsub("_NA", "", Activeness))       

#Subsetting Data for each Dog
dogs_data_visual_Hunter <- dogs_data_visual %>% filter(Name == "Hunter")
dogs_data_visual_Minzi <- dogs_data_visual %>% filter(Name == "Minzi")
dogs_data_visual_Akilah <- dogs_data_visual %>% filter(Name == "Akilah")
dogs_data_visual_Amara <- dogs_data_visual %>% filter(Name == "Amara")
dogs_data_visual_JT <- dogs_data_visual %>% filter(Name == "JT")


#Plot of Activity of Dogs in each Hour
ggplot(data = dogs_data_visual) + geom_bar(aes(x = Activity), fill = "salmon") + facet_grid(Name ~ Hour) + theme(axis.text.x = element_text(angle = 90)) + labs(title = "Bar Plots of Activity (Dog/Time)", y = "Frequency")

#Individual Plots of the Dogs from the above visualization
ggplot(data = dogs_data_visual_Hunter) + geom_bar(aes(x = Activity), fill = "salmon") + facet_grid(. ~ Hour) + theme(axis.text.x = element_text(angle = 90)) + labs(title = "Hunter: Barplots of Activity per Hour", y = "Frequency")
ggplot(data = dogs_data_visual_Minzi) + geom_bar(aes(x = Activity), fill = "salmon") + facet_grid(. ~ Hour) + theme(axis.text.x = element_text(angle = 90)) + labs(title = "Minzi: Barplots of Activity per Hour", y = "Frequency")
ggplot(data = dogs_data_visual_Akilah) + geom_bar(aes(x = Activity), fill = "salmon") + facet_grid(. ~ Hour) + theme(axis.text.x = element_text(angle = 90)) + labs(title = "Akilah: Barplots of Activity per Hour", y = "Frequency")
ggplot(data = dogs_data_visual_Amara) + geom_bar(aes(x = Activity), fill = "salmon") + facet_grid(. ~ Hour) + theme(axis.text.x = element_text(angle = 90)) + labs(title = "Amara: Barplots of Activity per Hour", y = "Frequency")
ggplot(data = dogs_data_visual_JT) + geom_bar(aes(x = Activity), fill = "salmon") + facet_grid(. ~ Hour) + theme(axis.text.x = element_text(angle = 90)) + labs(title = "JT: Barplots of Activity per Hour", y = "Frequency")


# Group by Inactive behavior
inactive_prop <- 
  dogs_data[!is.na(dogs_data$IC2_Value),] %>%
  group_by(IC2_Value) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n) * 100)

active_prop <- 
  dogs_data[!is.na(dogs_data$IC1_Value),] %>%
  group_by(IC1_Value) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n) * 100)

# Bar plots
ggplot(data=active_prop, aes(x=IC1_Value, y=freq)) + geom_bar(fill = "coral", alpha = 0.7, stat = "identity") + labs(title = "Percentage of active behaviors", x = "Dogs' behavior when active", y = "Percentage (%)")
ggplot(data=inactive_prop, aes(x=IC2_Value, y=freq)) + geom_bar(fill = "coral", alpha = 0.7, stat = "identity") + labs(title = "Percentage of inactive behaviors", x = "Dogs' behavior when inactive", y = "Percentage (%)")

#Adding the column "Day.of.Week"
dogs_dataH <- dogs_data
dogs_dataH$Date <- as.POSIXct(dogs_dataH$Date, format = "%m/%d/%y")
dogs_dataH <- mutate(dogs_dataH, Day.of.Week = wday(Date, label = TRUE))

#Omitting overlapping Active and Inactive
#(I don't think we need this anymore)
#dogs_dataH <- subset(dogs_dataH, (is.na(IC1_Name) | is.na(IC2_Name)))

#Creating the column "Activeness"
dogs_dataH$Activeness <- ifelse(is.na(dogs_dataH$IC1_Name), "Inactive", "Active")

#Grouping the data by name, day of week, and activeness
dogs_dataH_grouped <- group_by(dogs_dataH, Name, Day.of.Week, Activeness)

#Making the summary a data frame
summary <- as.data.frame(summarise(dogs_dataH_grouped, n()))
names(summary)[names(summary) == "n()"] <- "counts"

#group_by summarise data table and making the sum of counts by each dog each day
summary <- summary %>% group_by(Name, Day.of.Week) %>%
  mutate(sum = sum(counts))

#Merging the summary data frame into the dogs data frame to have the column "coutns"
dogs_dataH <- left_join(dogs_dataH, summary, by = c("Name", "Day.of.Week", "Activeness"))

#Creating a percentage column
dogs_dataH <- mutate(dogs_dataH, percent = dogs_dataH$counts/dogs_dataH$sum*100)
dogs_dataH$percent <- round(dogs_dataH$percent, digits = 1)
dogs_dataH$percent <- paste(dogs_dataH$percent, "%")

##Plots of activeness by dogs by day
ggplot(dogs_dataH, aes(x = Day.of.Week, y = counts, fill = Activeness)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x = "Day of Week", y="Counts") +
  geom_text(aes(label = percent), position = position_dodge(width = 0.9), size = 4) +
  facet_grid(~ Name) 
  


#For 3/3/2020

#Incorporate Day of Week into the data frame (Df with Activity Column)
dogs_data_comp <- dogs_data_visual
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


table(dogs_data_comp$Food)

