library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(janitor)

sq_monkey_data <- read_csv("https://raw.githubusercontent.com/qishuyi/ZooMonitor/master/report_study_1583445158.csv", 
                           col_types = cols(.default = col_character(),
                                            SessionID = col_double(), 
                                            `Session Start Time` = col_datetime(format = ""),
                                            `Session End Time` = col_datetime(format = ""),
                                            DateTime = col_datetime(format = ""),
                                            Date = col_date(format = ""),
                                            Time = col_time(format = ""),
                                            Year = col_double(),
                                            Month = col_double(),
                                            Hour = col_double(),
                                            Duration = col_double(),
                                            `Frame Number` = col_double()))
#Renaming Columns
sq_monkey_data <- sq_monkey_data %>% rename(Session_Start_Time = `Session Start Time`,
                                      Session_End_Time = `Session End Time`, 
                                      Name = `Focal Name`,
                                      Frame_Number = `Frame Number`,
                                      Channel_Type = `Channel Type`,
                                      IC1_Name = `Interval Channel 1 Name`,
                                      IC1_Value = `Interval Channel 1 Value`,
                                      IC2_Name = `Interval Channel 2 Name`,
                                      IC2_Value = `Interval Channel 2 Value`,
                                      IC2_Social_Modifier = `Interval Channel 2 Social Modifier`,
                                      AOV = `All Occurrence Value`,
                                      AOV_Social_Modifier = `All Occurrence Behavior Social Modifier`)


#Creating a Duplicate Channel Type Column
sq_monkey_data$Occurence_Duplicate <- sq_monkey_data$Channel_Type
sq_monkey_data$Occurence_Duplicate[sq_monkey_data$Occurence_Duplicate == "Interval"] <- NA


#Uniting all IC Names + Duplicated All Occurence columns to a single column named Category                       
sq_monkey_data <- sq_monkey_data %>% unite("Category", c(IC1_Name, IC2_Name,
                                                         Occurence_Duplicate), remove = T)

#Uniting all IC Value + Occurence Value columns to a single column named Activity   
sq_monkey_data <- sq_monkey_data %>% unite("Activity", c(IC1_Value, IC2_Value, AOV), remove = T)                                                                 


#Filtering out rows without behavioral observations
sq_monkey_data <- sq_monkey_data %>% filter(!(Category == "NA_NA_NA"))
  

#Fixing NA labels for Category Column
sq_monkey_data <- sq_monkey_data %>% mutate(Category = gsub("NA_", "", Category)) %>%  
  mutate(Category = gsub("_NA", "", Category)) 


#Fixing NA labels for Activity Column
sq_monkey_data <- sq_monkey_data %>% mutate(Activity= gsub("NA_", "", Activity)) %>%  
  mutate(Activity = gsub("_NA", "", Activity)) 


#Splitting rows with multiple behavioral observations                                
sq_monkey_data <- separate_rows(sq_monkey_data, Category, Activity, sep  = "_")


#Removing Unnecessary Columns
sq_monkey_data <- sq_monkey_data %>% select(-`Configuration Name`, -Observer,-DeviceID,
                                            -DateTime, -`Grid Size`, -`Image Size`,
                                            -`Project Animals`, -Duration, -Channel_Type)






