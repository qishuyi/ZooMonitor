library(readr)
library(dplyr)
library(tidyr)

#Importing Cattle Data
cattle_data <- read_csv("https://raw.githubusercontent.com/qishuyi/ZooMonitor/master/report_study_1582236321.csv", col_types = cols(Notes = col_character()))


#Removing Unnecessary Columns
cattle_data <- cattle_data %>% select(-`Configuration Name`, -Observer,-DeviceID,
                                  -DateTime, -`Grid Size`, -`Image Size`,-`Channel Type`,
                                  -`Project Animals`, -Duration, -`All Occurrence Value`)
  
#Renaming Columns
cattle_data <- cattle_data %>% rename(Session_Start_Time = `Session Start Time`,
                                  Session_End_Time = `Session End Time`, 
                                  Name = `Focal Name`,
                                  Frame_Number = `Frame Number`,
                                  IC1_Name = `Interval Channel 1 Name`,
                                  IC1_Value = `Interval Channel 1 Value`,
                                  IC2_Name = `Interval Channel 2 Name`,
                                  IC2_Value = `Interval Channel 2 Value`,
                                  IC3_Name = `Interval Channel 3 Name`,
                                  IC3_Value = `Interval Channel 3 Value`,
                                  IC3_Social_Modifier = `Interval Channel 3 Social Modifier`,
                                  IC4_Name = `Interval Channel 4 Name`,
                                  IC4_Value = `Interval Channel 4 Value`,
                                  IC4_Social_Modifier = `Interval Channel 4 Social Modifier`,
                                  IC5_Name = `Interval Channel 5 Name`,
                                  IC5_Value = `Interval Channel 5 Value`,
                                  IC6_Name = `Interval Channel 6 Name`,
                                  IC6_Value = `Interval Channel 6 Value`,
                                  IC7_Name = `Interval Channel 7 Name`,
                                  IC7_Value = `Interval Channel 7 Value`,
                                  IC8_Name = `Interval Channel 8 Name`,
                                  IC8_Value = `Interval Channel 8 Value`,
                                  IC9_Name = `Interval Channel 9 Name`,
                                  IC9_Value = `Interval Channel 9 Value`)
   
#Uniting all IC Names columns to a single column named Category                       
cattle_data <- cattle_data %>% unite("Category", c(IC1_Name, IC2_Name, IC3_Name, 
                                                     IC4_Name, IC5_Name, IC6_Name,
                                                     IC7_Name, IC8_Name, IC9_Name), remove = T)

#Uniting all IC Value columns to a single column named Activity   
cattle_data <- cattle_data %>% unite("Activity", c(IC1_Value, IC2_Value, IC3_Value, 
                                                   IC4_Value, IC5_Value, IC6_Value,
                                                   IC7_Value, IC8_Value, IC9_Value), remove = T)                                                                 


#Filtering out rows without behavioral observations
cattle_data <- cattle_data %>% filter(!(Category == "NA_NA_NA_NA_NA_NA_NA_NA_NA")) 
  

#Fixing NA labels for Category Column
cattle_data <- cattle_data %>% mutate(Category = gsub("NA_", "", Category)) %>%  
  mutate(Category = gsub("_NA", "", Category)) 


#Fixing NA labels for Activity Column
cattle_data <- cattle_data %>% mutate(Activity= gsub("NA_", "", Activity)) %>%  
  mutate(Activity = gsub("_NA", "", Activity)) 
 
 
#Splitting rows with multiple behavioral observations                                
cattle_data <- separate_rows(cattle_data, Category, Activity, sep  = "_")

                   
                                  
                                
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  