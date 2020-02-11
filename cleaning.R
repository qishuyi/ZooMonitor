#Importing Libraries
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

#Importing Dogs Data
dogs_data <- read_csv("https://raw.githubusercontent.com/qishuyi/ZooMonitor/master/report_study_1579790635.csv", col_types = cols(Notes = col_character()))


#Data Cleaning
dogs_data <- dogs_data %>% select(-`Configuration Name`, -Observer,-DeviceID,
                                  -DateTime, -`Grid Size`, -`Image Size`,-`Channel Type`,
                                  -`Project Animals`, -Duration) 

dogs_data <- dogs_data %>% rename(Session_Start_Time = `Session Start Time`,
                                  Session_End_Time = `Session End Time`, 
                                  Name = `Focal Name`,
                                  Frame_Number = `Frame Number`,
                                  IC1_Name = `Interval Channel 1 Name`,
                                  IC1_Value = `Interval Channel 1 Value`,
                                  IC1_Social_Modifier = `Interval Channel 1 Social Modifier`,
                                  IC2_Name = `Interval Channel 2 Name`,
                                  IC2_Value = `Interval Channel 2 Value`)

dogs_data <- dogs_data %>% filter(!(is.na(IC1_Value) & is.na(IC2_Value)))