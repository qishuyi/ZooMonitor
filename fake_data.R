
create_fake_data <- function(data){
  
  
  fake_names <- c("Lincoln", "Abigail", "Zion", "Wilmer", "Dolly", "Strike", "Kale", "Amor", 
                  "Gertrude", "Jaws", "Autumn", "Pluto", "Remington", "Cherry", "Pumpkin", 
                  "Rose", "Azure", "Fluff", "Leroy", "Remy", "Nova", "Bucky", "Tofu", "Elton",
                  "Mini", "Ollie", "Eddie", "Paris", "Argentina") 
   
  actual_names <- unique(data$Names)               
  name_length <- length(actual_names)
  
  new_names <- sample(fake_names, name_length)
  replacement <- sample(new_names, nrow(data), replace = T)
  
  data$Names <- replacement
    
    
    
  
  
}


