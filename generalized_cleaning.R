#Loading in Libraries
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(tools)

#Loading in Data
animal_data <- read_csv("https://raw.githubusercontent.com/qishuyi/ZooMonitor/master/Data/report_study_1579790635.csv", 
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

#Removing spaces and adding underscore
names(animal_data) <- gsub(" ", "_", names(animal_data))


#Renaming Columns
animal_data <- animal_data %>% rename(Name = Focal_Name)


#Creating a Duplicate Channel Type Column if All_Occurrence_Value exists as a column
#If All_Occurrence Value exists as a column, also adds "Occurrence_Duplicate" into the IC_Name_Vector
#If All_Occurrence Value exists as a column, also adds "All_Occurrence_Value" into the IC_Value_Vector

#Creating vectors used to unite to create a "Category" and "Activity" Column
IC_Name_Vector <- character()
IC_Value_Vector <- character()

if("All_Occurrence_Value" %in% names(animal_data)){
  animal_data$Channel_Type[animal_data$Channel_Type == "Interval"] <- NA
  IC_Name_Vector <- append(IC_Name_Vector, "Channel_Type")
  IC_Value_Vector <- append(IC_Value_Vector, "All_Occurrence_Value")
  
} else{
    animal_data <- animal_data %>% select(-Channel_Type)
  
  }

#Uniting all IC Names + Duplicated All Occurrence columns to a single column named Category    

for(i in names(animal_data)){
  
  if(str_detect(i, "^Interval_Channel.+Name$")){ 
    
    IC_Name_Vector <- append(IC_Name_Vector, i)
  }
} 

animal_data <- animal_data %>% unite("Category", IC_Name_Vector , remove = T)                                                                 


#Uniting all IC Value + Occurrence Value columns to a single column named Activity   

for(j in names(animal_data)){
  
  if(str_detect(j, "^Interval_Channel.+Value$")){ 
    
    IC_Value_Vector <- append(IC_Value_Vector, j)
  }
}

animal_data <- animal_data %>% unite("Behavior", IC_Value_Vector, remove = T)

#Social Modifier Editing

#Do nothing if there is no social modifier column
#If there is only one social modifier column, rename the column to Social_Modifier
#If there are more than one social modifier column, unite it into a single column 

Social_Modifier_Vector <- character()

for(k in names(animal_data)){
  
  if(str_detect(k, "Social_Modifier$")){
    
    Social_Modifier_Vector <- append(Social_Modifier_Vector, k)
  }
     
}


if(length(Social_Modifier_Vector) == 1){
  animal_data <- animal_data %>% rename(Social_Modifier = Social_Modifier_Vector[1])
  } else if(length(Social_Modifier_Vector) > 1){
  animal_data <- animal_data %>% unite("Social_Modifier", Social_Modifier_Vector, remove = TRUE)
  }else {
    animal_data$Social_Modifier <- NA
  }


#Filtering out rows without behavioral observations
animal_data <- animal_data %>% filter(!str_detect(animal_data$Behavior, "^NA(_NA)*$")) 


#Fixing NA labels for Category Column
animal_data <- animal_data %>% mutate(Category = gsub("NA_", "", Category)) %>%  
  mutate(Category = gsub("_NA", "", Category)) 

#Fixing NA labels for Behavior Column
animal_data <- animal_data %>% mutate(Behavior = gsub("NA_", "", Behavior)) %>%  
  mutate(Behavior = gsub("_NA", "", Behavior)) 

#Fixing NA labels for Social Modifier Column
animal_data <- animal_data %>% mutate(Social_Modifier = gsub("NA_", "", Social_Modifier)) %>%
  mutate(Social_Modifier = gsub("_NA", "", Social_Modifier)) 

if(length(Social_Modifier_Vector) > 1){
animal_data$Social_Modifier[animal_data$Social_Modifier == "NA"] <- NA
}

#Splitting rows with multiple behavioral observations                                
animal_data <- separate_rows(animal_data, Category, Behavior, sep  = "_")


#Removing Unnecessary Columns
animal_data <- animal_data %>% select(-Configuration_Name, -Observer,-DeviceID,
                                            -DateTime, -Grid_Size, -Image_Size,
                                            -Project_Animals, -Duration)



#Combining potential same behaviors with slightly different names
animal_data$Behavior <- toTitleCase(animal_data$Behavior)

#Combining potential same categories with slightly
animal_data$Category <- toTitleCase(animal_data$Category)


####################### Addition

#Adding Day of Week
animal_data <- mutate(animal_data, Day_of_Week = wday(Date, label = TRUE))
