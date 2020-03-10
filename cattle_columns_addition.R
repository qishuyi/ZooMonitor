#Day of Week
cattle_data <- mutate(cattle_data, Day_of_Week = wday(Date, label = TRUE))

#Behavior Type Column (Positive/Non-Positive)
cattle_data <- cbind(cattle_data, Behavior_Type = cattle_data$Category)
cattle_data$Behavior_Type <- as.character(cattle_data$Behavior_Type)

for(type in c("Inactive", "Locomotion", "Social", "Agonistic",
              "Maintenance", "Exploratory & Feeding", "Stereotypic",
              "Enrichment-based", "Other")){
  
  if(type == "Inactive" | type == "Agonistic" | type == "Exploratory & Feeding" |
     type == "Stereotypic" | type == "Other"){
    cattle_data$Behavior_Type[cattle_data$Behavior_Type == type] <- "Non-Positive"
  }
  
  else{
    cattle_data$Behavior_Type[cattle_data$Behavior_Type == type] <- "Positive"
  }  
}


