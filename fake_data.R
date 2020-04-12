library(manip)

#Function that generates fake data
create_fake_data <- function(data){
  data <- read.csv("https://raw.githubusercontent.com/qishuyi/ZooMonitor/master/Data/report_study_1582236321.csv")
  
  #length of data
  data_length <- nrow(data)
  
  #Vector of fake names
  fake_names <- c("Lincoln", "Abigail", "Zion", "Wilmer", "Dolly", "Strike", "Kale", "Amor", 
                  "Gertrude", "Jaws", "Autumn", "Pluto", "Remington", "Cherry", "Pumpkin", 
                  "Rose", "Azure", "Fluff", "Leroy", "Remy", "Nova", "Bucky", "Tofu", "Elton",
                  "Mini", "Ollie", "Eddie", "Paris", "Argentina") 
   
  #Actual animal names in the data
  actual_names <- unique(data$Focal.Name)               
  
  #Number of animals in the data
  name_length <- length(actual_names)
  
  #Vector of fake names to replace the real names
  new_names <- sample(fake_names, name_length)
  
  #Create a vector of indices for the columns where we want to manipulate names
  social_modifier_column_indicies <- which(str_detect(colnames(data), "Social.Modifier"))
  social_modifier_column_indicies <- c(which(colnames(data) == "Focal.Name"), social_modifier_column_indicies)
  
  #Create fake names for each animal name in the dataset
  for (i in 1:length(actual_names)) {
    for (j in social_modifier_column_indicies) {
      data[,j] <- as.character(data[,j])
      indices <- which(data[,j] == actual_names[i])
      data[,j] <- replace(data[,j], indices, rep.int(c(new_names[i]), length(indices))) 
      data[,j] <- as.factor(data[,j])
    }
    #Change project animals too
    data$Project.Animals <- gsub(actual_names[i], new_names[i], data$Project.Animals)
  }
  
  #Actual Observer names in the data
  observer_names <- unique(data$Observer)   
  
  #Number of observers in the data
  observer_name_length <- length(observer_names)
  
  #Vector of fake names to replace the real names
  new_observer_names <- sample(fake_names, observer_name_length)
  
  #Create fake names for each observer name in the dataset
  for (i in 1:length(observer_names)) {
    data$Observer <- as.character(data$Observer)
    indices <- which(data$Observer == observer_names[i])
    data$Observer <- replace(data$Observer, indices, rep.int(c(new_observer_names[i]), length(indices))) 
    data$Observer <- as.factor(data$Observer)
  }
  
  #Eliminate animal species
  data$Configuration.Name <- as.character(data$Configuration.Name)
  data$Configuration.Name <- replace(data$Configuration.Name, 1:length(data$Configuration.Name), rep.int(c("Animal"), length(data$Configuration.Name)))
  data$Configuration.Name <- as.factor(data$Configuration.Name)
 
  #Subsetting the 80% of the data randomly
  selected_rows <- sort(sample(1:data_length, ceiling(.8 * data_length), replace = F))
  
  data <- data[selected_rows,]
    
  return(data)
 
}

test <- create_fake_data(read.csv("https://raw.githubusercontent.com/qishuyi/ZooMonitor/master/Data/report_study_1582236321.csv"))