library(ggplot2)
library(dplyr)
library(scales)

zoo.data = read.csv("report_study_1579790635.csv")
table(zoo.data$Year)
table(zoo.data$Focal.Name)
table(zoo.data$Interval.Channel.1.Name)
table(zoo.data$Interval.Channel.2.Name)
table(zoo.data$Interval.Channel.1.Value)
table(zoo.data$Interval.Channel.2.Value)
table(zoo.data$Hour)
table(zoo.data$Month)
table(zoo.data$Project.Animals)
table(zoo.data$Duration)
table(zoo.data$Configuration.Name)
table(zoo.data$DeviceID)
table(zoo.data$Observer)
table(zoo.data$Notes)
table(zoo.data$Channel.Type)

# Get rid of columns with unuseful or repetitive information (Configuration.Name, Observer, DeviceID, 
# DateTime, Project.Animals, Grid.Size, Image.Size, Channel.Type)
zoo.data.truncated <- zoo.data[-c(2:4, 7, 15:16, 18)]

# Get rid of rows without a channel 1 value or a channel 2 value
zoo.data.cleaned <- zoo.data.truncated[!zoo.data.truncated$Interval.Channel.1.Value=="" | !zoo.data.truncated$Interval.Channel.2.Value=="", ]

# Group by Inactive behavior
inactive.data.grouped <- group_by(zoo.data.cleaned[!zoo.data.cleaned$Interval.Channel.2.Name == "",], Interval.Channel.2.Value)
inactive.group.count <- count(inactive.data.grouped, Interval.Channel.2.Name)
inactive.group.prop <- inactive.group.count %>% mutate(freq = n / sum(n))
  

# Bar plots
active.behaviors <- table(zoo.data.cleaned$Interval.Channel.1.Value)
inactive.behaviors <- table(zoo.data.cleaned$Interval.Channel.2.Value)
active.plot <- qplot(data=zoo.data.cleaned[!zoo.data.cleaned$Interval.Channel.1.Name == "",], x=Interval.Channel.1.Value, geom="bar", fill = "coral") + labs(x = "Dogs' behavior when active")


ggplot(data=inactive.summary, aes(Interval.Channel.2.Value)) + geom_bar(fill = "coral", alpha = 0.7) + labs(x = "Dogs' behavior when inactive")

active.plot
inactive.plot

# Bar plot per dog per hour
