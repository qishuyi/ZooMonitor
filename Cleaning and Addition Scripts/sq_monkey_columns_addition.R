library(zoo)

#Day of week
sq_monkey_data <- mutate(sq_monkey_data, Day_of_Week = wday(Date, label = TRUE))

#Fixing Misrecorded Observations (Squirt is Damian Part 1)
rm_indicies1 <- which(sq_monkey_data$Notes == "Recorded Squirt's behavior under Damian" &
                          sq_monkey_data$Name == "Damian")

sq_monkey_data[rm_indicies1, which(names(sq_monkey_data) == "Name")] <- "Squirt"

#Fixing Misrecorded Observations (Damian is Squirt Part 2)
rm_indicies2 <- which(sq_monkey_data$Notes == "Used Damien's name for Squirt's behavior." &
                          sq_monkey_data$Name == "Damian")

sq_monkey_data[rm_indicies2, which(names(sq_monkey_data) == "Name")] <- "Squirt"
