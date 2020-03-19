source("generalized_cleaning.R")

#General Plots: (Day of Week, Time of Day, Animal's name )

################## Percentage of observations (Time of day)
time_of_day_viz <- ggplot(data = dogs_data, aes(x = Hour)) + 
  geom_bar(aes(y = ..count../nrow(dogs_data)*100), fill = "steelblue", width = .75) + 
  scale_x_discrete(limits = 9:16) +
  scale_y_continuous(limits = c(0,25)) + 
  geom_hline(yintercept = (1/8)*100, color = "darkmagenta", alpha = .45, linetype = "longdash") +
  labs(title = "Percentage of Observations (Per Time of Day)", x = "Time of Day", y = "Percentage (%)") +
  annotate("text", x= 16.6, y = 13.5 , label = "12.5%", color = "darkmagenta", size = 3.25)


day_of_week_viz <- ggplot(data = dogs_data, aes(x = Day_of_Week)) +
  geom_bar(aes(y = ..count../nrow(dogs_data)*100), fill = "steelblue2", width = .75) +
  scale_x_discrete(limits=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  scale_y_continuous(limits = c(0,60)) +
  labs(title = "Percentage of Observations (Per Day of Week)", x = "Day of Week", y = "Percentage (%)") +
  geom_hline(yintercept = (1/7)*100, color = "darkmagenta", alpha = .45, linetype = "longdash") + 
  annotate("text", x= 1.5, y = 16.5 , label = "14.28%", color = "darkmagenta", size = 3.25)

grid.arrange(day_of_week_viz, time_of_day_viz,  nrow = 1)

#Single Bar Plot: (Category)

#Single Bar Plot: (Behaviors)

#Faceted Bar Plot: (Time of day, Day of week )(Animal's name (including "All animals"))


#Single Pie Chart: (Date Range)

JT_before <- subset(dogs_data, Date < "2018-10-15")
JT_before <- JT_before %>% group_by(Activity)
summary_JT_before <- as.data.frame(summarise(JT_before, n()))
names(summary_JT_before)[names(summary_JT_before) == "n()"] <- "counts"
summary_JT_before <- summary_JT_before %>%
  mutate(percent = round(counts/268*100, 1)) %>%
  mutate(Period = "Before")

JT_after <- subset(dogs_data, Date > "2018-10-15")
JT_after <- JT_after %>% group_by(Activity)
summary_JT_after <- as.data.frame(summarise(JT_after, n()))
names(summary_JT_after)[names(summary_JT_after) == "n()"] <- "counts"
summary_JT_after <- summary_JT_after %>%
  mutate(percent = round(counts/2996*100, 1)) %>%
  mutate(Period = "After")

summary_JT <- rbind(summary_JT_before, summary_JT_after)
summary_JT$Period <- factor(summary_JT$Period, levels = c("Before", "After"))

ggplot(summary_JT, aes(x="", y=percent, fill=fct_reorder(Activity, desc(percent)))) + geom_bar(stat="identity", width=1) +
  facet_grid(.~ Period) +
  coord_polar("y", start=0) + 
  labs(x = NULL, y = NULL, fill = NULL, title = "JT's Birth and Activity", subtitle = "Raw Counts: Before = 268, After = 2996") +
  guides(fill = guide_legend(reverse = TRUE, override.aes = list(size = 1))) +
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5, face = "bold"),
                          plot.subtitle = element_text(face = "italic"),
                          legend.position="bottom") +
  scale_fill_manual(values = rainbow(10)[sample(1:10)])
