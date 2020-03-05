#Importing Libraries
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

#Importing Dogs Data
dogs_data <- read_csv("https://raw.githubusercontent.com/qishuyi/ZooMonitor/master/report_study_1579790635.csv", col_types = cols(Notes = col_character()))


#Removing Unnecessary Columns
dogs_data <- dogs_data %>% select(-`Configuration Name`, -Observer,-DeviceID,
                                  -DateTime, -`Grid Size`, -`Image Size`,-`Channel Type`,
                                  -`Project Animals`, -Duration) 
#Renaming Columns
dogs_data <- dogs_data %>% rename(Session_Start_Time = `Session Start Time`,
                                  Session_End_Time = `Session End Time`, 
                                  Name = `Focal Name`,
                                  Frame_Number = `Frame Number`,
                                  IC1_Name = `Interval Channel 1 Name`,
                                  IC1_Value = `Interval Channel 1 Value`,
                                  IC1_Social_Modifier = `Interval Channel 1 Social Modifier`,
                                  IC2_Name = `Interval Channel 2 Name`,
                                  IC2_Value = `Interval Channel 2 Value`)

#Uniting all IC Names columns to a single column named Activeness  
dogs_data <- dogs_data %>% unite("Activeness", c(IC1_Name, IC2_Name), remove = T)

#Uniting all IC Values columns to a single column named Activity  
dogs_data <- dogs_data %>% unite("Activity", c(IC1_Value, IC2_Value), remove = T)


#Filtering out rows without behavioral observations
dogs_data <- dogs_data %>% filter(!(Activeness == "NA_NA")) 


#Fixing NA Labels for Activeness Column
dogs_data <- dogs_data %>% mutate(Activeness = gsub("NA_", "", Activeness))
dogs_data <- dogs_data %>% mutate(Activeness = gsub("_NA", "", Activeness))       


#Fixing NA Labels for Activity Column
dogs_data <- dogs_data %>% mutate(Activity = gsub("NA_", "", Activity))
dogs_data <- dogs_data %>% mutate(Activity = gsub("_NA", "", Activity))                                  


#Splitting rows with multiple behavioral observations                                
dogs_data <- separate_rows(dogs_data, Activeness, Activity, sep  = "_")


#Relabeling Behaviors in Activity Column
dogs_data <- dogs_data %>% mutate(Activity = gsub("Dog interaction", "Dog Int", Activity))
dogs_data <- dogs_data %>% mutate(Activity = gsub("Interacting with object", "Object Int", Activity))
dogs_data <- dogs_data %>% mutate(Activity = gsub("Walking Around", "Walking", Activity))
dogs_data <- dogs_data %>% mutate(Activity = gsub("Out of view", "Out of View", Activity))












