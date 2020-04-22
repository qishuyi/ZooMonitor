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
library(RColorBrewer)
library(viridis)
library(tools)
library(janitor)
library(RColorBrewer) 
library(shinythemes)

### Note: 
### Currently, if no data file is uploaded, the APP will be using the generalized cleaning script by default,
### which uses dogs' data (line 97). To use another dataset, either upload the data file or change the 
### filepath in generalized_cleaning.R.

############### UI and Server ###############
ui <- navbarPage("ZooMonitor", theme = shinytheme("yeti"),
                 ############################### Upload Data ###############################
                 tabPanel("Upload Data",
                          
                          titlePanel(h3("Upload a CSV File")),
                          
                          sidebarLayout(
                            # Add filters to take user inputs
                            sidebarPanel(
                              # Allow users to upload a csv file
                              fileInput("file1", h4("Choose a CSV File"),
                                        multiple = FALSE,
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv"))
                            ),
                            # Display the data file
                            mainPanel(
                              # If there is an error
                              uiOutput("uploadError"),
                              
                              dataTableOutput("contents")
                            ))),
                 
                 ############################### General Observations ###############################
                 tabPanel("Observations",
                          
                          titlePanel(h3("Distribution of Observations")),
                          
                          sidebarLayout(
                            # Add filters to take user inputs
                            sidebarPanel(
                              # Allow users to choose the x-axis
                              radioButtons("select_general", h4("Show Observations by:"),
                                           choices = list("Day of Week", "Hour of Day", "Animal")
                              )
                            ),
                            mainPanel(
                              # Show the plot of general obervations
                              plotOutput("general_plot"),
                              textOutput("selected_general")
                            ))),
                 
                 
                 ############################### Faceted Barplot ###############################
                 tabPanel("Daily/Weekly",
                          
                          titlePanel(h3("Frequency of Behaviors")),
                          sidebarPanel(
                            uiOutput("dateControls4"),
                            radioButtons("select_faceted_barplot", h4("Show Behavior by:"),
                                         choices = list("Day of Week", "Hour of Day")),
                            uiOutput("nameControls4")
                          ),
                          mainPanel(
                            # If there are no available data, show helptext
                            uiOutput("no_data_barplot"),
                            
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
                              radioButtons("select_exclusion", h4("Use:"),
                                           choices = list("All Data", "Data Without the Subject Animal")),
                              helpText(HTML("Subject animal is the individual that caused the event. <br/> e.g., death, birth, joining etc.")),
                              uiOutput("exclusionControls")
                            ),
                            mainPanel(
                              textOutput("plz_select"),
                              textOutput("no_plot"),
                              #Removing the warning message that appears for a second 
                              #This is a warning for not having either pie chart of before or after on the min/max date
                              tags$style(type="text/css",
                                         ".shiny-output-error { visibility: hidden; }",
                                         ".shiny-output-error:before { visibility: hidden; }"),
                              # Show the plot of general obervations
                              plotOutput("event_pie_plot")
                            ))),
                 
                 ############################### Activity ###############################
                 
                 tabPanel("Activities",
                          
                          titlePanel(h3("Infographics of Selected Activities")),
                          sidebarPanel(
                            radioButtons(inputId = "filter_type", label = h4("Filter Activities by:"),
                                         c("Category", "Behavior"), selected = "Category"),
                            actionButton(inputId = "select_all", label = "Select All"),
                            actionButton(inputId = "deselect_all", label = "Deselect All"),
                            uiOutput("select_activity")
                            
                            
                          ),
                          mainPanel(
                            tabsetPanel(
                              
                              tabPanel("Visual", plotOutput(outputId = "activity_visual")),
                              tabPanel("Summary Table", tableOutput(outputId = "activity_table")),
                              tabPanel("Raw Table", tableOutput(outputId = "raw_activity_table"), 
                                       uiOutput(outputId = "activity_text")),
                              tabPanel("Information Table", tableOutput(outputId = "information_table"),
                                       uiOutput(outputId = "info_text"))
                              
                              
                              
                            )))
                 
                 
)


# Define server logic
server <- function(input, output) {
  
  data_input <- reactive({
    # input$file1 will be NULL initially, the req function ensures the value of input$file1 is available.
    # If input$file1 is unavailable, req will throw an exception.
    req(input$file1)
    
    #Loading in Data
    tryCatch(
      {
        animal_data <- read_csv(input$file1$datapath,
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
      },
      warning = function(w) {
        if (str_detect(w$message, "column names")) {
          output$uploadError <- renderUI(HTML(paste(
            em("Data format not compatible")
          )))  
        }
      }
    )
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
    } else {
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
    
    return(animal_data)
  })
  
  ############################### Upload Data ###############################
  output$contents <- renderDT({
    data_input()
  })
  
  ############################### General Observations ###############################
  ##Bar plot of observation distribution
  output$general_plot <- renderPlot({
    animal_data <- data_input()
    
    # Day of Week Plot 
    if (input$select_general == "Day of Week"){
      ggplot(data = animal_data, aes(x = Day_of_Week, y = ..count../nrow(animal_data))) +
        geom_bar(fill = "steelblue2", width = .5) +
        scale_x_discrete(limits=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1L), 
                           limits = c(0,1)) +
        labs(title = "Barplot of Observations per Day of Week", 
             subtitle = "The numbers above each bar represent the raw count of observations.",
             caption = "The dashed line represents equally distributed observations.",
             x = "Day of Week", y = "Percentage") + 
        theme(plot.caption = element_text(size = 12, hjust = 0.5, face = "italic")) +
        geom_hline(yintercept = 1/length(unique(animal_data$Day_of_Week)), color = "darkmagenta", alpha = .45, linetype = "longdash") +
        theme(plot.title = element_text(size = 12, face = "bold"),
              plot.subtitle = element_text(size = 12, face = "italic"),
              plot.caption = element_text(size = 12, face = "italic")) +
        geom_text(stat='count', aes(label=..count..), vjust=-1)
      
    }
    
    #Time of Day plot
    else if(input$select_general == "Hour of Day"){
      ggplot(data = animal_data, aes(x = Hour, y = ..count../nrow(animal_data))) + 
        geom_bar(fill = "steelblue", width = .5) + 
        scale_x_discrete(limits = min(animal_data$Hour) : max(animal_data$Hour)) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1L), 
                           limits = c(0,1)) +
        labs(title = "Barplot of Observations per Hour of Day", 
             subtitle = "The numbers above each bar represent the raw count of observations.",
             caption = "The dashed line represents equally distributed observations.",
             x = "Hour of Day", y = "Percentage") + 
        theme(plot.caption = element_text(size = 12, hjust = 0.5, face = "italic")) +
        geom_hline(yintercept = 1/length(unique(animal_data$Hour)), color = "darkmagenta", alpha = .45, linetype = "longdash") +
        theme(plot.title = element_text(size = 12, face = "bold"),
              plot.subtitle = element_text(size = 12, face = "italic"),
              plot.caption = element_text(size = 12, face = "italic")) +
        geom_text(stat='count', aes(label=..count..), vjust=-1)
    } 
    
    #Animal Plot
    else {
      ggplot(data = animal_data, aes(x = Name, y = ..count../nrow(animal_data))) +
        geom_bar(fill = "aquamarine3", width = .5) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1L), 
                           limits = c(0,1)) +
        labs(title = "Barplot of Observations per Animal Name", 
             subtitle = "The numbers above each bar represent the raw count of observations.",
             caption = "The dashed line represents equally distributed observations.",
             x = "Animal Name", y = "Percentage") + 
        theme(plot.caption = element_text(size = 12, hjust = 0.5, face = "italic")) +
        geom_hline(yintercept = 1/length(unique(animal_data$Name)), color = "darkmagenta", alpha = .45, linetype = "longdash") +
        theme(plot.title = element_text(size = 12, face = "bold"),
              plot.subtitle = element_text(size = 12, face = "italic"),
              plot.caption = element_text(size = 12, face = "italic")) +
        geom_text(stat='count', aes(label=..count..), vjust=-1)
      
    }
  })  
  
  ############################### Activity ###############################
  
  
  #Creative Reactive Input for either Categories/Behaviors
  
  #Making sure that everything is deselected as well, when the filter input changes
  #Making sure that everything is deselected when a new data set is selected
  #Also incorporates the Deselect All Functionality
  
  
  observeEvent(c(input$filter_type, input$deselect_all, input$file1),  {
    
    output$select_activity <- renderUI({
      
      #Get updated data
      animal_data <- data_input()
      
      req(input$filter_type)
      
      
      if(input$filter_type == "Category"){
        
        category <- sort(unique(animal_data$Category))
        checkboxGroupInput(inputId = "category_input", 
                           label= h4("Select Categories"),
                           choices = category)
      } else {
        
        behavior_options <- sort(unique(animal_data$Behavior))
        checkboxGroupInput(inputId = "behavior_input",
                           label = h4("Select Behaviors"),
                           choices = behavior_options)
        
        
      }
      
      
    })
    
  })
  
  
  #Select All Functionality
  
  observeEvent(input$select_all, {
    
    output$select_activity <- renderUI({  
      
      animal_data <- data_input()
      
      
      if(input$filter_type == "Category"){
        
        
        category <- sort(unique(animal_data$Category))
        checkboxGroupInput(inputId = "category_input", 
                           label= h4("Select Categories"),
                           choices = category, 
                           selected = category)
      } else { 
        
        behavior_options <- sort(unique(animal_data$Behavior))
        checkboxGroupInput(inputId = "behavior_input",
                           label = h4("Select Behaviors"),
                           choices = behavior_options,
                           selected = behavior_options)
      }
      
      
    })
  })
  
  
  
  
  #Creates Infographics based on Chosen Categories/Behaviors
  
  #Reactive Category Visual
  output$activity_visual <- renderPlot({
    
    #Get updated data
    animal_data <- data_input()
    
    req(input$filter_type)
    
    
    if(input$filter_type == "Category"){
      
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
      
      # Assign colors from a palette to each behavior so that colors remain unchanged in the stacked barplots
      # regardless of the order in which we select them.
      colors <- c(brewer.pal(8, "Set2"), brewer.pal(12, "Paired"), brewer.pal(8, "Dark2"))
      names(colors) = levels(animal_category$Category)
      colors <- colors[1:length(levels(animal_category$Category))]
      
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
                           limits = c(0,1)) +
        scale_fill_manual(values = colors)
      
    } else {
      
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
      
      # Assign colors from a palette to each behavior so that colors remain unchanged in the stacked barplots
      # regardless of the order in which we select them.
      colors <- c(brewer.pal(8, "Set2"), brewer.pal(12, "Paired"), brewer.pal(8, "Dark2"))
      names(colors) = levels(animal_behavior$Behavior)
      colors <- colors[1:length(levels(animal_behavior$Behavior))]
      
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
                           limits = c(0,1)) +
        scale_fill_manual(values = colors)
      
    }
    
    
    
  })
  
  
  #Reactive Category/Behavior Table
  output$activity_table <- renderTable({
    
    #Get updated data
    animal_data <- data_input()
    
    req(input$filter_type)
    
    if(input$filter_type == "Category"){
      
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
      
    } else {
      
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
      
    }
    
    
  })
  
  #Reactive Raw Category/Behavior Table
  output$raw_activity_table <- renderTable({
    
    #Get updated data
    animal_data <- data_input()
    
    req(input$filter_type)
    
    if(input$filter_type == "Category"){
      
      total_observations <- 0
      
      for(i in input$category_input){
        total_observations <- total_observations + sum(animal_data$Category == i)
      }
      
      if(total_observations <= 15 & total_observations != 0){
        
        output$activity_text <- renderUI({
          ""
        })
        
        animal_raw_category_table <- animal_data %>% filter(Category %in% input$category_input) %>%
          select(Name, Category, Behavior, Date, Time) %>%
          mutate(Time = str_sub(Time, 1,5))
        
        
        
        animal_raw_category_table$Date <- format(animal_raw_category_table$Date, format = "%B %d, %Y")
        
        return(animal_raw_category_table)
        
      } else {
        
        output$activity_text <- renderUI(HTML(paste(
          em("Table appears only if there are 15 or less observations in total for the selected categories")
        )))
        
        animal_raw_category_table <- data.frame()
        return(animal_raw_category_table)
        
      }
      
    } else {
      
      total_observations <- 0
      
      for(i in input$behavior_input){
        total_observations <- total_observations + sum(animal_data$Behavior == i)
      }
      
      
      if(total_observations <= 15 & total_observations != 0){
        
        output$activity_text <- renderText({""})
        
        animal_raw_behavior_table <- animal_data %>% filter(Behavior %in% input$behavior_input) %>%
          select(Name, Category, Behavior, Date, Time) %>%
          mutate(Time = str_sub(Time, 1,5))
        
        animal_raw_behavior_table$Date <- format(animal_raw_behavior_table$Date, format = "%B %d, %Y")
        
        return(animal_raw_behavior_table)
        
      } else {
        
        output$activity_text <- renderUI(HTML(paste(
          em("Table appears only if there are 15 or less observations in total for the selected behaviors")
        )))
        
        animal_raw_behavior_table <- data.frame()
        return(animal_raw_behavior_table)
        
      }
      
      
    } 
    
  })
  
  
  #Reactive Information Table
  output$information_table <- renderTable({
    #Get updated data
    animal_data <- data_input()
    
    
    req(input$filter_type)
    
    
    if(input$filter_type == "Category"){
      
      #Info text  
      output$info_text <- renderUI({""})  
      
      #Requring input
      req(input$category_input)
      
      
      #Vector of selected inputs
      selected_categories <- input$category_input
      
      #Finding the maximum number of behaviors from the selected categories
      max_len <- 0
      for(i in 1:length(selected_categories)){
        select_data <- animal_data %>% filter(Category == selected_categories[i]) %>%
          select(Category, Behavior)
        check <- length(unique(select_data$Behavior))
        
        if(check > max_len){
          max_len <- check
          
        }
      }      
      
      #Empty matrix
      info_matrix <- matrix(nrow = max_len + 1, ncol = length(selected_categories))
      
      
      #Adding to the matrix
      for(i in 1:length(selected_categories)){
        select_data <- animal_data %>% filter(Category == selected_categories[i]) %>%
          select(Category, Behavior)
        b <- selected_categories[i]
        b <- append(b, sort(unique(select_data$Behavior)))  
        need <- max_len + 1 - length(b)
        add <- rep("", times = need)
        b <- append(b, add)
        
        info_matrix[ , i] <- b
        
      }
      
      #Converting matrix to dataframe and using first row as column names
      info_data <- data.frame(info_matrix)
      info_data <- info_data %>% row_to_names(row_number = 1)
      
      return(info_data)
      
      
    } else {
      
      output$info_text <- renderUI(HTML(paste(
        em("Information Table appears only if you are filtering by Category")
      )))
      
      empty_table <- data.frame()
      return(empty_table)
      
      
    }
    
    
    
  })
  
  
  ############################### Faceted Barplots ###############################
  #Let user select an animal name
  output$nameControls4 <- renderUI({
    #Get updated data
    animal_data <- data_input()
    
    prefix <- c('All animals')
    names <- sort(unique(animal_data$Name))
    names <- c(prefix, names)
    radioButtons('names4', h4("Select Animal"), names)
  })
  #Let user select a date range
  output$dateControls4 <- renderUI({
    #Get updated data
    animal_data <- data_input()
    
    date <- animal_data$Date
    dateRangeInput("daterange4", h4("Select Date Range"),
                   start = min(animal_data$Date),
                   end = max(animal_data$Date),
                   min = min(animal_data$Date),
                   max = max(animal_data$Date))
  })
  
  #Create the faceted barplots
  output$faceted_barplot <- renderPlot({
    # Get updated data
    animal_data <- data_input()
    
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
    
    if (nrow(animal_data) == 0) return()
    
    # Choose x-axis of the barplots
    if(animal_name == "All animals") animal_name <- "All Animals"
    
    plot_caption <- paste(animal_name, ": Barplots of Behavior per", sep = "")
    if (input$select_faceted_barplot == "Day of Week") {
      # Day of Week
      # Change caption of the plot
      plot_caption <- paste(plot_caption, "Day of Week")
      ggplot(data = animal_data) + geom_bar(aes(x = Behavior), fill = "salmon") + 
        facet_wrap(~ Day_of_Week, ncol = 2, dir = "v") + 
        theme(axis.text.x = element_text(angle = 90, size = 10),
              plot.title = element_text(size = 12, face = "bold")) + 
        labs(title = plot_caption, y = "Frequency")
    } else {
      # Change caption of the plot
      plot_caption <- paste(plot_caption, "Hour of Day")
      
      # Change hour format (e.g., from 9 to "9:00")
      animal_data_hour <- animal_data[order(as.integer(animal_data$Hour)),]
      animal_data_hour$Hour <- sapply(animal_data_hour$Hour, function(x) as.factor(sprintf("%d:00", x)))
      
      ggplot(data = animal_data_hour) + geom_bar(aes(x = Behavior), fill = "salmon") + 
        facet_wrap(~ Hour, ncol = 2, dir = "v") + 
        theme(axis.text.x = element_text(angle = 90, size = 10),
              plot.title = element_text(size = 12, face = "bold")) + 
        labs(title = plot_caption, y = "Frequency")
    }
  }, height = 600)
  
  output$no_data_barplot <- renderUI({
    # Get updated data
    animal_data <- data_input()
    
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
    
    # Indicate if there are no data available for the animal and date range selected
    if (nrow(animal_data) == 0) {
      error_msg <- character()
      if (start_date > end_date) {
        error_msg <- HTML(paste("Invalid date range: start date (", start_date, 
                                ") must be no later than end date (", end_date, ").", sep = ""))
        error_msg <- HTML(paste(em(error_msg)))
      }else {
        error_msg <- HTML(paste("There is no data for", animal_name, 
                                "in your selected date range:", start_date, "to", end_date))
        error_msg <- HTML(paste(em(error_msg), ".", sep = ""))
      }
      return(error_msg)
    }
  })
  
  ############################### Pie Charts ###############################
  #Let users choose the event date
  output$dateControls <- renderUI({
    
    #Get updated data
    animal_data <- data_input()
    date <- animal_data$Date
    
    #Date input
    dateInput("date", h4("Select Event Date"),
              value = min(animal_data$Date),
              min = min(animal_data$Date),
              max = max(animal_data$Date))
  })
  
  #Let user choose the subject animal to exclude
  output$exclusionControls <- renderUI({
    
    #Show checkbox group with subject animal(s)
    if(input$select_exclusion == "Data Without the Subject Animal") {
      #Get updated data
      animal_data <- data_input()
      subject_animal <- sort(unique(animal_data$Name))
      checkboxGroupInput("subject_animal", h4("Select Animal to Exclude"), 
                         choices = subject_animal)
    }
  })
  
  ##### Pie Charts
  output$event_pie_plot <- renderPlot({
    
    #Get updated data
    animal_data <- data_input()
    #Calls the input
    req(input$date)
    req(input$select_exclusion)
    
    #If "All Data" was selected 
    if(input$select_exclusion == "All Data") {
      
      #Creates a before dataset (it will contain 0 observation when the first date of the data was selected)
      before <- subset(animal_data, Date < input$date)
      #Creates an after dataset (it will contain 0 observation when the last date of the data was selected)
      after <- subset(animal_data, Date > input$date)
      
    }
    
    #If "Use data without the subject animal" was selected
    if(input$select_exclusion == "Data Without the Subject Animal") {
      #Calls the subject animal(s)
      req(input$subject_animal)
      
      #No plot will be shown if zero or all subject animals were selected
      if(length(input$subject_animal) == 0 & input$select_exclusion == "Data Without the Subject Animal") return()
      if(length(input$subject_animal) == length(unique(animal_data$Name)) & input$select_exclusion == "Data Without the Subject Animal") return()
      
      #If one or more and less than all subject animals were selected
      if(length(input$subject_animal) > 0 & length(input$subject_animal) < length(unique(animal_data$Name)) &
       input$select_exclusion == "Data Without the Subject Animal") {
      
      #Exclude the selected subject animal(s) first
      animal_remain <- sort(unique(animal_data$Name))
      for (a in animal_remain) {
        if (a %in% input$subject_animal) {
          animal_data <- filter(animal_data, Name != a)}}

      #Creates before and after datasets 
      before <- subset(animal_data, Date < input$date)
      after <- subset(animal_data, Date > input$date)
      
      #If the selected date was in-between the first and the last day of the data
      if(input$date > min(animal_data$Date) & input$date < max(animal_data$Date)) {
        
        #Creates vectors with the animals inside each period
        before_name <- sort(unique(before$Name))
        after_name <- sort(unique(after$Name))
        not_mutual_name <- sort(unique(animal_data$Name))
        
        #Finds animals that are NOT in both before and after
        for(i in before_name) {
          for(j in after_name) {
            if(i == j) {
              not_mutual_name <- not_mutual_name[not_mutual_name != i]}}}
        
        #Subsets both before and after without the animals we found in the last part
        for(k in not_mutual_name) {
          before <- filter(before, Name != k)
          after <- filter(after, Name != k)}
        
        #Find the closest date to the selected date in the past when another event happened
        before_name <- sort(unique(before$Name))
        last_event <- 0
        last_event_date <- numeric()
        
        for (i in before_name) {
          for (j in nrow(before):1) {
            if (before$Name[j] == i) {
              last_event <- j}}
          last_event_date <- append(last_event_date, last_event)}
        
        #Find the closest date to the selected date in the futre that another event happened
        after_name <- sort(unique(after$Name))
        next_event <- 0
        next_event_date <- numeric()
        
        for (i in after_name) {
          for (j in 1:nrow(after)) {
            if (after$Name[j] == i) {
              next_event <- j}}
          next_event_date <- append(next_event_date, next_event)}
        
        #Slice the before and after subsets
        before <- slice(before, max(last_event_date):nrow(before))
        after <- slice(after, 1:min(next_event_date))
      }
      
      else if(input$date == min(animal_data$Date)) {
        #If the selceted date was the first day of the data
        after_name <- sort(unique(after$Name))
        next_event <- 0
        next_event_date <- numeric()
        
        #Slice the after subset before the next event happens
        for (i in after_name) {
          for (j in 1:nrow(after)) {
            if (after$Name[j] == i) {
              next_event <- j}}
          next_event_date <- append(next_event_date, next_event)}
        after <- slice(after, 1:min(next_event_date))
      }
      
      else {
        #If the selceted date was the last day of the data
        before_name <- sort(unique(before$Name))
        last_event <- 0
        last_event_date <- numeric()
        
        #Slice the before subset after the last event happened
        for (i in before_name) {
          for (j in nrow(before):1) {
            if (before$Name[j] == i) {
              last_event <- j}}
          last_event_date <- append(last_event_date, last_event)}
        before <- slice(before, max(last_event_date):nrow(before))
      }
    }
  }
    
    #####From here, it applies to every case
    #Creates summary data set for before
    before <- before %>% group_by(Behavior)
    summary_before <- as.data.frame(summarise(before, n()))
    names(summary_before)[names(summary_before) == "n()"] <- "counts"
    summary_before <- summary_before %>%
      mutate(Percent = round(counts/sum(counts)*100, 1)) %>%
      mutate(Period = "Before")
    
    #Creates summary data set for after
    after <- after %>% group_by(Behavior)
    summary_after <- as.data.frame(summarise(after, n()))
    names(summary_after)[names(summary_after) == "n()"] <- "counts"
    summary_after <- summary_after %>%
      mutate(Percent = round(counts/sum(counts)*100, 1)) %>%
      mutate(Period = "After")
    
    #Combines two summaries
    summary <- rbind(summary_before, summary_after)
    summary$Period <- factor(summary$Period, levels = c("Before", "After"))
    
    #Defining color palette
    summary$Behavior <- as.factor(summary$Behavior)
    colors2 <- c(brewer.pal(8, "Set2"), brewer.pal(12, "Paired"), brewer.pal(8, "Dark2"))
    names(colors2) = levels(summary$Behavior)
    colors2 <- colors2[1:length(levels(summary$Behavior))]
    
    #Creates a pie chart for only after
    ggplot(summary, aes(x="", y=Percent, fill=fct_reorder(Behavior, desc(Percent)))) + 
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) + 
      facet_grid(.~ Period) +
      labs(x = NULL, y = NULL, fill = NULL, title = "Pie Charts of Behavior Before/After an Event",
           subtitle = paste("Raw Counts: Before = ", sum(summary_before$counts), " After = ", sum(summary_after$counts))) +
      guides(fill = guide_legend(reverse = TRUE, override.aes = list(size = 1))) +
      theme_classic() + theme(axis.line = element_blank(),
                              axis.text = element_blank(),
                              axis.ticks = element_blank(),
                              plot.title = element_text(hjust = 0.5, face = "bold"),
                              plot.subtitle = element_text(hjust = 0.5, face = "italic"),
                              legend.position="bottom") +
      scale_fill_manual(values = colors2)
  })
  
  output$plz_select <- renderText({
    #If ZERO animal was selected, then a message will appear
    if (length(input$subject_animal) == 0 & input$select_exclusion == "Data Without the Subject Animal") {
      "There is no plot to display. Please select a animal/animals to exclude."}
  })
  
  output$no_plot <- renderText({
    #If ALL animals were selected, then no plot will appear
    if(length(input$subject_animal) == length(unique(animal_data$Name)) & input$select_exclusion == "Data Without the Subject Animal") {
      "There is no plot to display. Please select a different animal/different animals to exclude."}
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)