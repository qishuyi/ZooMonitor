
#Function that generates fake data
create_fake_data <- function(data){
  
  #length of data
  data_length <- nrow(data)
  
  #This is just to make the code easier to write, we will revert it back after we are done
  data <- data %>% rename(Name = `Focal Name`)
  
  
  #Vector of fake names
  fake_names <- c("Lincoln", "Abigail", "Zion", "Wilmer", "Dolly", "Strike", "Kale", "Amor", 
                  "Gertrude", "Jaws", "Autumn", "Pluto", "Remington", "Cherry", "Pumpkin", 
                  "Rose", "Azure", "Fluff", "Leroy", "Remy", "Nova", "Bucky", "Tofu", "Elton",
                  "Mini", "Ollie", "Eddie", "Paris", "Argentina") 
   
  #Actual names in the data
  actual_names <- unique(data$Name)               
  
  #Number of animals in the data
  name_length <- length(actual_names)
  
  #Vector of fake names to replace the real names
  new_names <- sample(fake_names, name_length)
  
  #Count to access new_names in the loop
  count <- 0
  
  #Replacing names
  for(i in actual_names){
    count <- count + 1
    
    for(j in 1:data_length){
      
      if(i == data$Name[j]){
        
        data$Name[j] <- new_names[count]
      }
      
    }
    
  }
  
  
  #Renaming it back to Focal Name
  data <- data %>% rename(`Focal Name` = Name)
 

  #Subsetting the 80% of the data randomly
  selected_rows <- sort(sample(1:data_length, ceiling(.8 * data_length), replace = F))
  
  data <- data[selected_rows,]
    
  return(data)
 
}


