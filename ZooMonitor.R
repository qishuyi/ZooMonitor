#Importing Libraries
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(ggplot2)

#Run Data cleaning script
source("cleaning.R")

#Structuring Data to Create Dog Activity Visualization
dogs_data_visual <- dogs_data %>% unite("Activity", c(IC1_Value, IC2_Value), remove = T)                              

dogs_data_visual <- dogs_data_visual %>% filter(str_detect(Activity, "NA") == TRUE) 
dogs_data_visual <- dogs_data_visual %>% mutate(Activity = gsub("NA_", "", Activity))
dogs_data_visual <- dogs_data_visual %>% mutate(Activity = gsub("_NA", "", Activity))                                  
dogs_data_visual <- dogs_data_visual %>% mutate(Activity = gsub("_NA", "", Activity)) 
dogs_data_visual <- dogs_data_visual %>% mutate(Activity = gsub("Dog interaction", "Dog Int", Activity))
dogs_data_visual <- dogs_data_visual %>% mutate(Activity = gsub("Interacting with object", "Obj Int", Activity))

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