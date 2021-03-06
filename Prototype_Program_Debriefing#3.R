#Loading in Libraries
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(shiny)
library(ggplot2)
library(forcats)
library(DT)

### Note: 
### Currently, if no data file is uploaded, the APP will be using the generalized cleaning script by default,
### which uses dogs' data (line 97). To use another dataset, either upload the data file or change the 
### filepath in generalized_cleaning.R.

# Helper functions to create plots

############################### General Observations ###############################
generalplot <- function(input, output, animal_data) {
  ##Bar plot of observation distribution
  output$general_plot <- renderPlot({
    #Time of Day plot
    if(input$select_general == "Hour of Day"){
      ggplot(data = animal_data, aes(x = Hour, y = ..count../nrow(animal_data))) + 
        geom_bar(fill = "steelblue", width = .5) + 
        scale_x_discrete(limits = 9:16) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1L), 
                           limits = c(0,1)) +
        labs(title = "Barplot of Observations per Hour of Day", x = "Hour of Day", y = "Percentage") + 
        geom_hline(yintercept = 1/8, color = "darkmagenta", alpha = .45, linetype = "longdash") +
        theme(plot.title = element_text(size = 12, face = "bold"))
      
      
      
    } 
    # Day of Week Plot 
    else if (input$select_general == "Day of Week"){
      ggplot(data = animal_data, aes(x = Day_of_Week, y = ..count../nrow(animal_data))) +
        geom_bar(fill = "steelblue2", width = .5) +
        scale_x_discrete(limits=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1L), 
                           limits = c(0,1)) +
        
        labs(title = "Barplot of Observations per Day of Week", x = "Day of Week", y = "Percentage") +
        geom_hline(yintercept = 1/7, color = "darkmagenta", alpha = .45, linetype = "longdash") +
        theme(plot.title = element_text(size = 12, face = "bold"))
      
    }
    #Animal Plot
    else {
      a <- length(unique(animal_data$Name))
      ggplot(data = animal_data, aes(x = Name, y = ..count../nrow(animal_data))) +
        geom_bar(fill = "aquamarine3", width = .5) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1L), 
                           limits = c(0,1)) +
        
        labs(title = "Barplot of Observations per Animal", x = "Animal Name", y = "Percentage") +
        geom_hline(yintercept = 1/a, color = "darkmagenta", alpha = .45, linetype = "longdash") +
        theme(plot.title = element_text(size = 12, face = "bold"))
      
    }
  })
}

############################### Category ###############################
category <- function(input, output, animal_data) {
  
  #Reactive Category Visual
  output$category_visual <- renderPlot({
    
    #Data frame to create visualization
    animal_data$Category <- as.factor(animal_data$Category)
    animal_category <- animal_data %>% 
      group_by(Name, Category, .drop = FALSE) %>%
      summarize(Count = n()) %>%
      filter(Category %in% input$category_input) %>%
      arrange(Name)
    
    #Counts total observations per animal
    animal_count <- numeric()
    for(i in sort(unique(animal_data$Name))){
      count <- sum(animal_data$Name == i)
      animal_count <- append(animal_count, count)
    }
    
    #Adding percentages column to the "visualization" data frame 
    animal_category <- animal_category %>% 
      cbind(Percentage = animal_category$Count/ rep(animal_count,
                                                    times = rep(length(input$category_input), length(animal_count))))
    
    #Creating the visualization
    ggplot(data = animal_category) +
      geom_bar(aes(x = Name, y = Percentage, fill = Category), stat = "identity", width = .4) +
      labs(title = "Barplot of Selected Categories per Animal",
           subtitle = "Percentages based on each animal's total number of observations",
           x = "Animal Name", y = "Percentage") +
      theme(plot.title = element_text(size = 12, face = "bold"),
            plot.subtitle = element_text(size = 9, face = "italic"),
            legend.title = element_text(size = 10),
            legend.text = element_text(size = 8)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1L), 
                         limits = c(0,1))
    
    
  })
  
  
  #Reactive Category Table
  output$category_table <- renderTable({
    
    if(length(input$category_input) == 0){
      animal_category_table <- data.frame()
      
    } else {
      
      animal_data$Category <- as.factor(animal_data$Category)
      animal_category_table <- animal_data %>% 
        group_by(Name, Category, .drop = FALSE) %>%
        summarize(Count = n()) %>%
        filter(Category %in% input$category_input) %>%
        arrange(Name)
      
      
      animal_count <- numeric()
      for(i in sort(unique(animal_data$Name))){
        count <- sum(animal_data$Name == i)
        animal_count <- append(animal_count, count)
      }
      
      animal_category_table <- animal_category_table %>% 
        cbind(Percentage = (animal_category_table$Count/ rep(animal_count,
                                                             times = rep(length(input$category_input), length(animal_count)))) * 100)
      
      animal_category_table$Percentage <- format(round(animal_category_table$Percentage, 2), nsmall = 2)
      
      animal_category_table <- rename(animal_category_table, `Percentage (%)` = "Percentage")
      
      return(animal_category_table)
      
    }
    
  })
  
  #Reactive Raw Category Table
  output$raw_category_table <- renderTable({
    
    total_observations <- 0
    
    for(i in input$category_input){
      total_observations <- total_observations + sum(animal_data$Category == i)
    }
    
    if(total_observations <= 15 & total_observations != 0){
      
      output$category_text <- renderText({
        ""
      })
      
      animal_raw_category_table <- animal_data %>% filter(Category %in% input$category_input) %>%
        select(Name, Category, Behavior, Date, Time) %>%
        mutate(Time = str_sub(Time, 1,5))
      
      
      
      animal_raw_category_table$Date <- format(animal_raw_category_table$Date, format = "%B %d, %Y")
      
      return(animal_raw_category_table)
      
    } else {
      
      output$category_text <- renderUI(HTML(paste(
        em("Table appears only if there are 15 or less observations in total for the selected categories")
      )))
      
      animal_raw_category_table <- data.frame()
      return(animal_raw_category_table)
      
    }
    
  })
  
  
  
}


############################### Behavior ###############################
behavior <- function(input, output, animal_data) {
  
  #Reactive Behavior Visual
  output$behavior_visual <- renderPlot({
    
    #Data frame to create visualization
    animal_data$Behavior <- as.factor(animal_data$Behavior)
    animal_behavior <- animal_data %>% 
      group_by(Name, Behavior, .drop = FALSE) %>%
      summarize(Count = n()) %>%
      filter(Behavior %in% input$behavior_input) %>%
      arrange(Name)
    
    
    #Counts total observations per animal
    animal_count <- numeric()
    for(i in sort(unique(animal_data$Name))){
      count <- sum(animal_data$Name == i)
      animal_count <- append(animal_count, count)
    }
    
    #Adding percentages column to the "visualization" data frame 
    animal_behavior <- animal_behavior %>% 
      cbind(Percentage = animal_behavior$Count/ rep(animal_count,
                                                    times = rep(length(input$behavior_input), length(animal_count))))
    
    
    #Creating the visualization
    ggplot(data = animal_behavior) +
      geom_bar(aes(x = Name, y = Percentage, fill = Behavior), stat = "identity", width = .4) +
      labs(title = "Barplot of Selected Behaviors per Animal",
           subtitle = "Percentages based on each animal's total number of observations",
           x = "Animal Name", y = "Percentage") +
      theme(plot.title = element_text(size = 12, face = "bold"),
            plot.subtitle = element_text(size = 9, face = "italic"),
            legend.title = element_text(size = 10),
            legend.text = element_text(size = 8)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1L), 
                         limits = c(0,1))
    
    
  })
  
  #Reactive Behavior Table
  output$behavior_table <- renderTable({
    
    if(length(input$behavior_input) == 0){
      animal_behavior_table <- data.frame()
      
    } else{
      
      animal_data$Behavior <- as.factor(animal_data$Behavior)
      animal_behavior_table <- animal_data %>% 
        group_by(Name, Behavior, .drop = FALSE) %>%
        summarize(Count = n()) %>%
        filter(Behavior %in% input$behavior_input) %>%
        arrange(Name)
      
      animal_count <- numeric()
      for(i in sort(unique(animal_data$Name))){
        count <- sum(animal_data$Name == i)
        animal_count <- append(animal_count, count)
      }
      
      animal_behavior_table <- animal_behavior_table %>% 
        cbind(Percentage = (animal_behavior_table$Count/ rep(animal_count,
                                                             times = rep(length(input$behavior_input), length(animal_count))))*100)
      
      
      animal_behavior_table$Percentage <- format(round(animal_behavior_table$Percentage, 2), nsmall = 2)
      
      
      animal_behavior_table <- rename(animal_behavior_table, `Percentage (%)` = "Percentage")
      
      return(animal_behavior_table)
    }
    
  })
  
  
  #Reactive Raw Behavior Table
  
  output$raw_behavior_table <- renderTable({
    
    total_observations <- 0
    
    for(i in input$behavior_input){
      total_observations <- total_observations + sum(animal_data$Behavior == i)
    }
    
    
    if(total_observations <= 15 & total_observations != 0){
      
      output$behavior_text <- renderText({""})
      
      animal_raw_behavior_table <- animal_data %>% filter(Behavior %in% input$behavior_input) %>%
        select(Name, Category, Behavior, Date, Time) %>%
        mutate(Time = str_sub(Time, 1,5))
      
      animal_raw_behavior_table$Date <- format(animal_raw_behavior_table$Date, format = "%B %d, %Y")
      
      return(animal_raw_behavior_table)
      
    } else {
      
      output$behavior_text <- renderUI(HTML(paste(
        em("Table appears only if there are 15 or less observations in total for the selected behaviors")
      )))
      
      animal_raw_behavior_table <- data.frame()
      return(animal_raw_behavior_table)
      
    }
    
  })
  
  
}

############################### Faceted Barplots ###############################
facetedBarplots <- function(input, output, animal_data) {
  output$faceted_barplot <- renderPlot({
    # Wait until the program loads up
    req(input$daterange4)
    
    # Choose the date range of the data we want to work with
    start_date <- input$daterange4[1]
    end_date <- input$daterange4[2]
    animal_data <- animal_data %>% filter(Date >= start_date & Date <= end_date)
    
    # Choose the animal(s) whose behaviors we will display
    animal_name <- input$names4
    if (animal_name != "All animals") {
      animal_data <- animal_data %>% filter(Name == animal_name)
    }
    
    # Choose x-axis of the barplots
    if(animal_name == "All animals") animal_name <- "All Animals"
    
    plot_caption <- paste(animal_name, ": Barplots of Behavior per", sep = "")
    if (input$select_faceted_barplot == "Hour of Day") {
      # Change caption of the plot
      plot_caption <- paste(plot_caption, "Hour of Day")
      ggplot(data = animal_data) + geom_bar(aes(x = Behavior), fill = "salmon") + 
        facet_wrap(~ Hour, ncol = 2, dir = "v") + 
        theme(axis.text.x = element_text(angle = 90, size = 10),
              plot.title = element_text(size = 12, face = "bold")) + 
        labs(title = plot_caption, y = "Frequency")
    }else {# Day of Week
      # Change caption of the plot
      plot_caption <- paste(plot_caption, "Day of Week")
      ggplot(data = animal_data) + geom_bar(aes(x = Behavior), fill = "salmon") + 
        facet_wrap(~ Day_of_Week, ncol = 2, dir = "v") + 
        theme(axis.text.x = element_text(angle = 90, size = 10),
              plot.title = element_text(size = 12, face = "bold")) + 
        labs(title = plot_caption, y = "Frequency")
    }
  }, height = 600)
}

############################### Pie Charts ###############################
piechart <- function(input, output, animal_data) {
  output$event_pie_plot <- renderPlot({
    
    #Calls the input
    req(input$date)
    
    #If the minimum date was selected (The very first date of the dataset)
    if(input$date == min(animal_data$Date)){
      animal_data <- animal_data %>% group_by(Behavior)
      summary_only_after <- as.data.frame(summarise(animal_data, n()))
      names(summary_only_after)[names(summary_only_after) == "n()"] <- "counts"
      a_summary_only_after <- sum(summary_only_after$counts)
      summary_only_after <- summary_only_after %>%
        mutate(Percent = round(counts/a_summary_only_after*100, 1))
      
      #Creates a pie chart for only after
      ggplot(summary_only_after, aes(x="", y=Percent, fill=fct_reorder(Behavior, desc(Percent)))) + 
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) + 
        labs(x = NULL, y = NULL, fill = NULL, title = "Pie Chart of Behavior After an Event",
             subtitle = paste("Raw Counts: Before = 0", ", After = ", a_summary_only_after),
             caption = "*This plot shows the behavior proportion for only the period after the selected date.") +
        guides(fill = guide_legend(reverse = TRUE, override.aes = list(size = 1))) +
        theme_classic() + theme(axis.line = element_blank(),
                                axis.text = element_blank(),
                                axis.ticks = element_blank(),
                                plot.title = element_text(hjust = 0.5, face = "bold"),
                                plot.subtitle = element_text(hjust = 0.5, face = "italic"),
                                plot.caption = element_text(hjust = 0.5, face = "bold"),
                                legend.position="bottom") +
        scale_fill_manual(values = rainbow(length(unique(animal_data$Behavior)))[sample(1:length(unique(animal_data$Behavior)))])
    } 
    
    #If the maximum date was selected (The very last date)
    else if(input$date == max(animal_data$Date)){
      animal_data <- animal_data %>% group_by(Behavior)
      summary_only_before <- as.data.frame(summarise(animal_data,n()))
      names(summary_only_before)[names(summary_only_before) == "n()"] <- "counts"
      a_summary_only_before <- sum(summary_only_before$counts)
      summary_only_before <- summary_only_before %>%
        mutate(Percent = round(counts/a_summary_only_before*100, 1))
      
      #Creates a pie chart for only before
      ggplot(summary_only_before, aes(x="", y=Percent, fill=fct_reorder(Behavior, desc(Percent)))) + 
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) + 
        labs(x = NULL, y = NULL, fill = NULL, title = "Pie Chart of Behavior Before an Event",
             subtitle = paste("Raw Counts: Before = ", a_summary_only_before, ", After = 0"),
             caption = "*This plot shows the behavior proportion for only the period before the selected date.") +
        guides(fill = guide_legend(reverse = TRUE, override.aes = list(size = 1))) +
        theme_classic() + theme(axis.line = element_blank(),
                                axis.text = element_blank(),
                                axis.ticks = element_blank(),
                                plot.title = element_text(hjust = 0.5, face = "bold"),
                                plot.subtitle = element_text(hjust = 0.5, face = "italic"),
                                plot.caption = element_text(hjust = 0.5, face = "bold"),
                                legend.position="bottom") +
        scale_fill_manual(values = rainbow(length(unique(animal_data$Behavior)))[sample(1:length(unique(animal_data$Behavior)))])
      
    }
    #If the date in-between the max and min dates was selected
    else{
      
      #Creates a before dataset
      before <- subset(animal_data, Date < input$date)
      before <- before %>% group_by(Behavior)
      summary_before <- as.data.frame(summarise(before, n()))
      names(summary_before)[names(summary_before) == "n()"] <- "counts"
      a_before <- sum(summary_before$counts)
      summary_before <- summary_before %>%
        mutate(Percent = round(counts/a_before*100, 1)) %>%
        mutate(Period = "Before")
      
      #Creates an after dataset 
      after <- subset(animal_data, Date > input$date)
      after <- after %>% group_by(Behavior)
      summary_after <- as.data.frame(summarise(after, n()))
      names(summary_after)[names(summary_after) == "n()"] <- "counts"
      a_after <- sum(summary_after$counts)
      summary_after <- summary_after %>%
        mutate(Percent = round(counts/a_after*100, 1)) %>%
        mutate(Period = "After")
      
      #Combines two summaries
      summary <- rbind(summary_before, summary_after)
      summary$Period <- factor(summary$Period, levels = c("Before", "After"))
      
      #Creates pie charts for both before and after
      ggplot(summary, aes(x="", y=Percent, fill=fct_reorder(Behavior, desc(Percent)))) + 
        geom_bar(stat="identity", width=1) +
        facet_grid(.~ Period) +
        coord_polar("y", start=0) + 
        labs(x = NULL, y = NULL, fill = NULL, title = "Pie Chart of Behavior Before/After an Event", 
             subtitle = paste("Raw Counts: Before = ", a_before, ", After = ", a_after)) +
        guides(fill = guide_legend(reverse = TRUE, override.aes = list(size = 1))) +
        theme_classic() + theme(axis.line = element_blank(),
                                axis.text = element_blank(),
                                axis.ticks = element_blank(),
                                plot.title = element_text(hjust = 0.5, face = "bold"),
                                plot.subtitle = element_text(hjust = 0.5, face = "italic"),
                                legend.position="bottom") +
        scale_fill_manual(values = rainbow(length(unique(animal_data$Behavior)))[sample(1:length(unique(animal_data$Behavior)))])
      
    }
  })
}


############### UI and Server ###############
ui <- navbarPage("ZooMonitor",
                 ############################### Upload Data ###############################
                 tabPanel("Data",
                          
                          titlePanel(h3("Choose Dataset")),
                          
                          sidebarLayout(
                            # Add filters to take user inputs
                            sidebarPanel(
                              # Allow users to choose a dataset
                              selectInput("dataset", "Select Animal Dataset", 
                                          c("Cattle" = "https://raw.githubusercontent.com/qishuyi/ZooMonitor/master/Data/report_study_1582236321.csv", 
                                            "Painted African Dog" = "https://raw.githubusercontent.com/qishuyi/ZooMonitor/master/Data/report_study_1579790635.csv", 
                                            "Squirrel Monkey" = "https://raw.githubusercontent.com/qishuyi/ZooMonitor/master/Data/report_study_1583445158.csv"))
                            ),
                            # Display the data file
                            mainPanel(
                              dataTableOutput("contents")
                            ))),
                 
                 ############################### General Observations ###############################
                 tabPanel("Observations",
                          
                          titlePanel(h3("Distribution of Observations")),
                          
                          sidebarLayout(
                            # Add filters to take user inputs
                            sidebarPanel(
                              # Allow users to choose the x-axis
                              radioButtons("select_general", "Show Observations by:",
                                           choices = list("Hour of Day", "Day of Week", "Animal")
                              )
                            ),
                            mainPanel(
                              # Show the plot of general obervations
                              plotOutput("general_plot"),
                              textOutput("selected_general"),
                              h6("The dash line represents equally distributed observations.", align = "center")
                              
                            ))),
                 
                 
                 ############################### Faceted Barplot ###############################
                 tabPanel("Daily/Weekly",
                          
                          titlePanel(h3("Frequency of Behaviors")),
                          sidebarPanel(
                            uiOutput("dateControls4"),
                            radioButtons("select_faceted_barplot", "Show Behavior by:",
                                         choices = list("Hour of Day", "Day of Week")),
                            uiOutput("nameControls4")
                          ),
                          mainPanel(
                            # Create the faceted barplot for frequency of behaviors
                            plotOutput("faceted_barplot")
                          )),
                 
                 ############################### Pie Chart ###############################
                 tabPanel("Events",
                          
                          titlePanel(h3("Impact of Events on Behavior")),
                          
                          sidebarLayout(
                            # Add filters to take user inputs
                            sidebarPanel(
                              # Allow users to choose the x-axis
                              uiOutput("dateControls"),
                              helpText("Selectable dates depend on the uploaded data set")
                            ),
                            mainPanel(
                              #Removing the warning message that appears for a second 
                              #This is a warning for not having either pie chart of before or after on the min/max date
                              tags$style(type="text/css",
                                         ".shiny-output-error { visibility: hidden; }",
                                         ".shiny-output-error:before { visibility: hidden; }"),
                              # Show the plot of general obervations
                              plotOutput("event_pie_plot"),
                              h6("The colors of slices will change every time you change the date.", align = "center")
                            ))),
                 
                 ############################### Category/Behavior ###############################
                 navbarMenu("Activities",
                            
                            tabPanel("Categories",
                                     
                                     titlePanel(h3("Infographics of Selected Categories")),
                                     sidebarPanel(
                                       uiOutput("select_category")
                                       
                                     ),
                                     mainPanel(
                                       tabsetPanel(
                                         
                                         tabPanel("Visual", plotOutput(outputId = "category_visual")),
                                         tabPanel("Summary Table", tableOutput(outputId = "category_table")),
                                         tabPanel("Raw Table", tableOutput(outputId = "raw_category_table"), 
                                                  uiOutput(outputId = "category_text"))
                                         
                                         
                                       ))),
                            
                            
                            tabPanel("Behaviors",
                                     
                                     titlePanel(h3("Infographics of Selected Behaviors")),
                                     sidebarPanel(
                                       uiOutput("select_behavior")
                                       
                                     ),
                                     mainPanel(
                                       tabsetPanel(
                                         
                                         tabPanel("Visual", plotOutput(outputId = "behavior_visual")),
                                         tabPanel("Summary Table", tableOutput(outputId = "behavior_table")),
                                         tabPanel("Raw Table", tableOutput(outputId = "raw_behavior_table"),
                                                  uiOutput(outputId = "behavior_text"))
                                         
                                         
                                       ))))
                 
                 
                 
)

# Define server logic
server <- function(input, output) {
  ############################### Upload Data ###############################
  output$contents <- renderDT({
    #Loading in Data
    animal_data <- read_csv(input$dataset, 
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
      animal_data <- cbind(animal_data, Occurrence_Duplicate = animal_data$Channel_Type)
      animal_data$Occurrence_Duplicate[animal_data$Occurrence_Duplicate == "Interval"] <- NA
      IC_Name_Vector <- append(IC_Name_Vector, "Occurrence_Duplicate")
      IC_Value_Vector <- append(IC_Value_Vector, "All_Occurrence_Value")
      
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
                                          -Project_Animals, -Duration, -Channel_Type)
    
    ####################### Addition
    #Adding Day of Week
    animal_data <- mutate(animal_data, Day_of_Week = wday(Date, label = TRUE))
    
    
    ############################### General Observations ###############################
    generalplot(input, output, animal_data)
    
    ############################### Category ###############################
    output$select_category <- renderUI({
      category <- sort(unique(animal_data$Category))
      checkboxGroupInput(inputId = "category_input", 
                         label= "Select Categories",
                         choices = category)
      
    })
    
    
    #Creates Infographics based on Chosen Categories
    category(input, output, animal_data)
    
    ############################### Behavior ###############################
    output$select_behavior <- renderUI({
      behavior_options <- sort(unique(animal_data$Behavior))
      checkboxGroupInput(inputId = "behavior_input",
                         label = "Select Behaviors",
                         choices = behavior_options)
      
    })
    
    #Creates Infographics based on Chosen Behaviors
    behavior(input, output, animal_data)
    
    ############################### Faceted Barplots ###############################
    #Let user select an animal name
    output$nameControls4 <- renderUI({
      prefix <- c('All animals')
      names <- sort(unique(animal_data$Name))
      names <- c(prefix, names)
      radioButtons('names4', "Select Animal", names)
    })
    #Let user select a date range
    output$dateControls4 <- renderUI({
      date <- animal_data$Date
      dateRangeInput("daterange4", "Select Date Range",
                     start = min(animal_data$Date),
                     end = max(animal_data$Date),
                     min = min(animal_data$Date),
                     max = max(animal_data$Date))
    })
    #Create the faceted barplots
    facetedBarplots(input, output, animal_data)
    
    ############################### Pie Charts ###############################
    output$dateControls <- renderUI({
      date <- animal_data$Date
      dateInput("date", "Select Event Date",
                value = min(animal_data$Date),
                min = min(animal_data$Date),
                max = max(animal_data$Date))
    })
    piechart(input, output, animal_data)
    
    return(animal_data)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
