#Day of week
sq_monkey_data <- mutate(sq_monkey_data, Day_of_Week = wday(Date, label = TRUE))

#Fixing Misrecorded Observations (Damian -> Squirt)

 #for(i in which(sq_monkey_data$Notes == "Recorded Squirt's behavior under Damian")){
  
  #if(sq_monkey_data$Name[i] == "Damian"){
    
   # sq_monkey_data$Name[i] <- "Squirt"
  #}
#}
