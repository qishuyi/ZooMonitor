#Installing Packages 
#Uncomment and install packages that are not already installed
#Make sure to recomment after installing necessary packages

#install.packages("dplyr")
#install.packages("DT")
#install.packages("forcats")
#install.packages("ggplot2")
#install.packages("janitor")
#install.packages("lubridate")
#install.packages("RColorBrewer")
#install.packages("readr")
#install.packages("shiny")
#install.packages("shinybusy")
#install.packages("shinythemes")
#install.packages("stringr")
#install.packages("tidyr")
#install.packages("tools")


#Loading in Libraries
library(dplyr)
library(DT)
library(forcats)
library(ggplot2)
library(janitor)
library(lubridate)
library(RColorBrewer) 
library(readr)
library(shiny)
library(shinybusy)
library(shinythemes)
library(stringr)
library(tidyr)
library(tools)


################# UI #################
ui <- navbarPage("ZooMonitor", theme = shinytheme("yeti"),
                 
                 ############################### Upload Data ###############################
                 tabPanel("Upload Data",
                          
                          titlePanel(h3("Upload a CSV File")),
                          
                          sidebarLayout(
                            # Add filters to take user inputs
                            sidebarPanel(
                              # Allow users to upload a csv file (users cannot upload multiple files)
                              fileInput("file1", h4("Choose a CSV File"),
                                        multiple = FALSE,
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv"))
                            ),
                            # Display the data file
                            mainPanel(
                              dataTableOutput("contents"),
                              # Add busy spinner for data upload
                              use_busy_spinner(spin = "fading-circle",
                                               position = "bottom-right",
                                               spin_id = "upload_busy")
                            ))),
                 
                 ############################### General Observations ###############################
                 tabPanel("Observations",
                          
                          titlePanel(h3("Distribution of Observations")),
                          
                          sidebarLayout(
                            # Add filters to take user inputs
                            sidebarPanel(
                              # Allow users to choose the x-axis of the bar plot
                              radioButtons("select_general", h4("Show Observations by:"),
                                           choices = list("Day of Week", "Hour of Day", "Animal")
                              )
                            ),
                            mainPanel(
                              # Show the plot of general obervations
                              plotOutput("general_plot"),
                              
                              # Add a download button
                              uiOutput("save_general")
                            ))),
                 
                 
                 ############################### Faceted Barplot ###############################
                 tabPanel("Daily/Weekly",
                          
                          titlePanel(h3("Frequency of Behaviors")),
                          
                          sidebarPanel(
                            # Add date range
                            uiOutput("dateControls4"),
                            
                            # Allow users to show behaviors by day of week or hour of day
                            radioButtons("select_faceted_barplot", h4("Show Behavior by:"),
                                         choices = list("Day of Week", "Hour of Day")),
                            
                            # Provide a list of checkboxes for animal names and 'Select All' and 'Deselect All' buttons
                            actionButton(inputId = "select_all_faceted", label = "Select All"),
                            actionButton(inputId = "deselect_all_faceted", label = "Deselect All"),
                            uiOutput("nameControls4")
                          ),
                          
                          mainPanel(
                            # If there are no available data, show helptext
                            uiOutput("no_data_barplot"),
                            
                            # Create the faceted barplot for frequency of behaviors
                            plotOutput("faceted_barplot", height = 600),
                            
                            # Add a download button
                            uiOutput("save_daily")
                          )),
                 
                 ############################### Pie Chart ###############################
                 tabPanel("Events",
                          
                          titlePanel(h3("Impact of Events on Behavior")),
                          
                          sidebarLayout(
                            # Add filters to take user inputs
                            sidebarPanel(
                              # Allow users to choose the event date
                              uiOutput("dateControls"),
                              # Allow users to choose the entire data set or without the subject animal(s)
                              radioButtons("select_exclusion", h4("Use:"),
                                           choices = list("All Data", "Data Without the Subject Animal")),
                              helpText(HTML("Subject animal is the individual that caused the event. <br/> e.g., death, birth, joining etc.")),
                              # Allow users to choose the subject animal(s)
                              uiOutput("exclusionControls"),
                              uiOutput("select_all_Pie"),
                              uiOutput("deselect_all_Pie")
                            ),
                            mainPanel(
                              # Texts shown when zero or all subject animals were selected
                              uiOutput("plz_select"),
                              uiOutput("no_plot"),
                              # Pie Chart(s)
                              plotOutput("event_pie_plot"),
                              # Provide download link
                              uiOutput("save_piechart")
                            ))),
                 
                 ############################### Activity ###############################
                 
                 tabPanel("Activities",
                          
                          titlePanel(h3("Infographics of Activities")),
                          
                          #Filters
                          sidebarPanel(
                            
                            #Filter by Category or Behavior
                            radioButtons(inputId = "filter_type", label = h4("Filter by:"),
                                         c("Category", "Behavior"), selected = "Category"),
                            
                            #Select All Button
                            actionButton(inputId = "select_all", label = "Select All"),
                            
                            #Deselct All Button
                            actionButton(inputId = "deselect_all", label = "Deselect All"),
                            
                            #Checkboxes
                            uiOutput("select_activity")),
                          
                          
                          mainPanel(
                            tabsetPanel(
                              
                              #Visual Sub Tab
                              tabPanel("Visual", plotOutput(outputId = "activity_visual"),
                                       uiOutput("save_activities")),
                              
                              #Summary Table Sub Tab
                              tabPanel("Summary Table", tableOutput(outputId = "activity_table")),
                              
                              #Raw Table Sub Tab
                              tabPanel("Raw Table", tableOutput(outputId = "raw_activity_table"), 
                                       uiOutput(outputId = "activity_text")),
                              
                              #Category Information Sub Tab
                              tabPanel("Category Information", tableOutput(outputId = "information_table"),
                                       uiOutput(outputId = "info_text"))
                              
                              
                            )))   
)


################# Server #################

# Define server logic
server <- function(input, output) {
  
  data_input <- reactive({
    # input$file1 will be NULL initially, the req function ensures the value of input$file1 is available.
    # If input$file1 is unavailable, req will throw an exception.
    req(input$file1)
    
    # A value to keep track of whether calling read_csv on the uploaded data generated a warning or not.
    assign("has_warning", FALSE, env=globalenv())
    
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
          # If a warning occured, set has_warning to TRUE
          assign("has_warning", TRUE, env=globalenv())
          
          # And show a pop-up window asking for another dataset with the correct format
          showModal(modalDialog(
            title = "Incompatible Dataset",
            "The format of the dataset is not compatible with the program, please try again!",
            easyClose = TRUE
          ))
        }
      }
    )
    
    # If dataset format is compatible, proceed to data cleaning and show a spinner while the data is processing.
    if (!get("has_warning", env=globalenv())) {
      # Show spinner
      show_spinner(spin_id = "upload_busy")
      
      
      ######### Generalized Cleaning Script #########      
      
      # Removing spaces and adding underscore
      names(animal_data) <- gsub(" ", "_", names(animal_data))
      
      # Renaming Columns
      animal_data <- animal_data %>% rename(Name = Focal_Name)
      
      
      # Creating a Duplicate Channel Type Column if All_Occurrence_Value exists as a column
      # If All_Occurrence Value exists as a column, also adds "Occurrence_Duplicate" into the IC_Name_Vector
      # If All_Occurrence Value exists as a column, also adds "All_Occurrence_Value" into the IC_Value_Vector
      
      # Creating vectors used to unite to create a "Category" and "Activity" Column
      IC_Name_Vector <- character()
      IC_Value_Vector <- character()
      
      if("All_Occurrence_Value" %in% names(animal_data)){
        animal_data$Channel_Type[animal_data$Channel_Type == "Interval"] <- NA
        IC_Name_Vector <- append(IC_Name_Vector, "Channel_Type")
        IC_Value_Vector <- append(IC_Value_Vector, "All_Occurrence_Value")
        
      } else{
        animal_data <- animal_data %>% select(-Channel_Type)
        
      }
      
      # Uniting all IC Names + Duplicated All Occurrence columns to a single column named Category    
      
      for(i in names(animal_data)){
        
        if(str_detect(i, "^Interval_Channel.+Name$")){ 
          
          IC_Name_Vector <- append(IC_Name_Vector, i)
        }
      }
      
      animal_data <- animal_data %>% unite("Category", IC_Name_Vector , remove = T)                                                                 
      
      
      # Uniting all IC Value + Occurrence Value columns to a single column named Activity   
      
      for(j in names(animal_data)){
        
        if(str_detect(j, "^Interval_Channel.+Value$")){ 
          
          IC_Value_Vector <- append(IC_Value_Vector, j)
        }
      }
      
      animal_data <- animal_data %>% unite("Behavior", IC_Value_Vector, remove = T)
      
      # Social Modifier Editing
      
      # If there is only one social modifier column, rename the column to Social_Modifier
      # If there are more than one social modifier column, unite it into a single column 
      # If there is no social modifier column, create an empty social modifier column
      
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
      
      
      # Filtering out rows without behavioral observations
      animal_data <- animal_data %>% filter(!str_detect(animal_data$Behavior, "^NA(_NA)*$")) 
      
      
      # Fixing NA labels for Category Column
      animal_data <- animal_data %>% mutate(Category = gsub("NA_", "", Category)) %>%  
        mutate(Category = gsub("_NA", "", Category)) 
      
      # Fixing NA labels for Behavior Column
      animal_data <- animal_data %>% mutate(Behavior = gsub("NA_", "", Behavior)) %>%  
        mutate(Behavior = gsub("_NA", "", Behavior)) 
      
      # Fixing NA labels for Social Modifier Column
      animal_data <- animal_data %>% mutate(Social_Modifier = gsub("NA_", "", Social_Modifier)) %>%
        mutate(Social_Modifier = gsub("_NA", "", Social_Modifier)) 
      
      if(length(Social_Modifier_Vector) > 1){
        animal_data$Social_Modifier[animal_data$Social_Modifier == "NA"] <- NA
      }
      
      # Splitting rows with multiple behavioral observations                                
      animal_data <- separate_rows(animal_data, Category, Behavior, sep  = "_")
      
      
      # Removing Unnecessary Columns
      animal_data <- animal_data %>% select(-Configuration_Name, -Observer,-DeviceID,
                                            -DateTime, -Grid_Size, -Image_Size,
                                            -Project_Animals, -Duration,
                                            -Notes, -Frame_Number)
      
      # Combining potential same behaviors with slightly different names 
      #(Using title case for Behavior)
      animal_data$Behavior <- toTitleCase(animal_data$Behavior)
      
      # Combining potential same categories with slightly
      #(Using title case for Category)
      animal_data$Category <- toTitleCase(animal_data$Category)
      
      # Using title case for animal names
      animal_data$Name <- toTitleCase(animal_data$Name)
      
      
      # Adding Day of Week
      animal_data <- mutate(animal_data, Day_of_Week = wday(Date, label = TRUE))
      
      hide_spinner(spin_id = "upload_busy")
      
      return(animal_data) 
    }
  })
  
  ############################### Upload Data ########################################
  
  output$contents <- renderDT({
    data_input()
  })
  
  ############################### General Observations ###############################
  
  ####Bar plot of observation distribution
  output$general_plot <- renderPlot({ .observations() })
  
  .observations <- reactive({
    
    #Get updated data
    animal_data <- data_input()
    
    ###Day of Week Plot 
    if (input$select_general == "Day of Week"){
      ggplot(data = animal_data, aes(x = Day_of_Week, y = ..count../nrow(animal_data))) +
        geom_bar(fill = "steelblue2", width = .5) +
        #Fix x-axis
        scale_x_discrete(limits=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1L), 
                           limits = c(0,1)) +
        labs(title = "Barplot of Observations per Day of Week", 
             subtitle = "The numbers above each bar represent the raw observation count.",
             caption = "The dashed line represents equally distributed observations.",
             x = "Day of Week", y = "Percentage") + 
        geom_hline(yintercept = 1/7, color = "darkmagenta", alpha = .45, linetype = "longdash") +
        geom_text(stat='count', aes(label=..count..), vjust= - 0.5, fontface = "italic") +
        theme(plot.title = element_text(size = 14, face = "bold"),
              plot.subtitle = element_text(size = 12, face = "italic"),
              plot.caption = element_text(size = 12, hjust = 0.5, vjust = -0.5, face = "italic"),
              axis.title = element_text(size = 12, face = "bold"),
              axis.text = element_text(size = 10),
              legend.title = element_blank(),
              legend.text = element_text(size = 10))
      
    }
    
    ###Time of Day plot
    else if(input$select_general == "Hour of Day"){
      
      #Fix the x-axis
      hour_breaks <- c(7:17)
      #Used to draw a dashed segment line
      df <- data.frame(x1 = 8.75, x2 = 16.25, y1 = 1/8, y2 = 1/8)
      
      #Plot with animal_data
      a <- ggplot(data = animal_data, aes(x = Hour, y = ..count../nrow(animal_data))) + 
        geom_bar(fill = "steelblue", width = .5) + 
        #Fix x-axis
        scale_x_continuous(breaks = hour_breaks,
                           labels = as.character(hour_breaks),
                           limits = c(7,17)) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1L), 
                           limits = c(0,1)) +
        labs(title = "Barplot of Observations per Hour of Day", 
             subtitle = "The numbers above each bar represent the raw observation count.",
             caption = "The dashed line represents equally distributed observations.\nHours outside 9am to 4pm rarely have observations.",
             x = "Hour of Day", y = "Percentage") +
        geom_text(stat='count', aes(label=..count..), vjust= - 0.5, fontface = "italic") +
        theme(plot.title = element_text(size = 14, face = "bold"),
              plot.subtitle = element_text(size = 12, face = "italic"),
              plot.caption = element_text(size = 12, hjust = 0.5, vjust = -0.5, face = "italic"),
              axis.title = element_text(size = 12, face = "bold"),
              axis.text = element_text(size = 10),
              legend.title = element_blank(),
              legend.text = element_text(size = 10))
      
      #Add a dashed segment line
      a + geom_segment(data = df, aes(x = x1, xend = x2, y = y1, yend = y2),
                       linetype = "longdash", alpha = .45, colour = "darkmagenta")
    } 
    
    ###Animal Plot
    else {
      ggplot(data = animal_data, aes(x = Name, y = ..count../nrow(animal_data))) +
        geom_bar(fill = "aquamarine3", width = .5) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1L), 
                           limits = c(0,1)) +
        labs(title = "Barplot of Observations per Animal Name", 
             subtitle = "The numbers above each bar represent the raw observation count.",
             caption = "The dashed line represents equally distributed observations.",
             x = "Animal Name", y = "Percentage") + 
        geom_hline(yintercept = 1/length(unique(animal_data$Name)), color = "darkmagenta", alpha = .45, linetype = "longdash") +
        geom_text(stat='count', aes(label=..count..), vjust= - 0.5, fontface = "italic") +
        theme(plot.title = element_text(size = 14, face = "bold"),
              plot.subtitle = element_text(size = 12, face = "italic"),
              plot.caption = element_text(size = 12, hjust = 0.5, vjust = -0.5, face = "italic"),
              axis.title = element_text(size = 12, face = "bold"),
              axis.text = element_text(size = 10),
              legend.title = element_blank(),
              legend.text = element_text(size = 10))
      
    }
  })  
  
  # Add download link to UI
  output$save_general <- renderUI({
    req(.observations())
    downloadLink("download_general", "Download Visual")
  })
  
  # Specify download link
  output$download_general <- downloadHandler(
    filename = function() {
      paste("observation.jpg")
    }, 
    content = function(file) {
      jpeg(file, width = 900, height = 400)
      plot(.observations())
      dev.off()
    }
  )
  
  ############################### Activity ###############################
  
  #Creating Reactive Input for the Activity Tab
  
    #Making sure that everything is deselected when the filter input changes
    #Making sure that everything is deselected when a new data set is uploaded
    #Incorporating the Deselect All Functionality
  
  observeEvent(c(input$filter_type, input$deselect_all, input$file1),  {
    
    output$select_activity <- renderUI({
      
      #Get updated data
      animal_data <- data_input()
      
      #Require filter type input
      req(input$filter_type)
      
      #When the filter option is Category
      if(input$filter_type == "Category"){
        
        #Create checkboxes for Categories
        category <- sort(unique(animal_data$Category))
        checkboxGroupInput(inputId = "category_input", 
                           label= h4("Select Categories"),
                           choices = category)
        
      #When the filter option is Behavior  
      } else {
        
        #Create checkboxes for Behaviors
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
      
      #Get updated data
      animal_data <- data_input()
      
      #When the filter option is Category
      if(input$filter_type == "Category"){
        
        
        #Ensure that all Category checkboxes are selected
        category <- sort(unique(animal_data$Category))
        checkboxGroupInput(inputId = "category_input", 
                           label= h4("Select Categories"),
                           choices = category, 
                           selected = category)
        
      #When the filter option is Behavior  
      } else { 
        
        #Ensure that all Behavior checkboxes are selected
        behavior_options <- sort(unique(animal_data$Behavior))
        checkboxGroupInput(inputId = "behavior_input",
                           label = h4("Select Behaviors"),
                           choices = behavior_options,
                           selected = behavior_options)
      }
    })
  })
  
  
  ######## Visual Sub Tab ########
  output$activity_visual <- renderPlot({ .activities_visual() })
  
  .activities_visual <- reactive({
    
    #Get updated data
    animal_data <- data_input()
    
    #Require filter type input
    req(input$filter_type)
    
    #When the filter option is Category
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
      
      #Assign colors from a palette to each behavior so that colors remain unchanged in the stacked barplots
      #regardless of the order in which we select them.
      colors <- c(brewer.pal(8, "Set2"), brewer.pal(12, "Paired"), brewer.pal(8, "Dark2"))
      names(colors) = levels(animal_category$Category)
      colors <- colors[1:length(levels(animal_category$Category))]
      
      
      #Creating the visualization
      ggplot(data = animal_category) +
        geom_bar(aes(x = Name, y = Percentage, fill = Category), stat = "identity", width = .4) +
        labs(title = "Barplot of Selected Categories per Animal",
             caption = "Percentages are relative to each animal's total number of observations.",
             x = "Animal Name", y = "Percentage") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1L), 
                           limits = c(0,1)) +
        scale_fill_manual(values = colors) +
        theme(plot.title = element_text(size = 14, face = "bold"),
              plot.subtitle = element_text(size = 12, face = "italic"),
              plot.caption = element_text(size = 12, hjust = 0.5, vjust = -0.5, face = "italic"),
              axis.title = element_text(size = 12, face = "bold"),
              axis.text = element_text(size = 10),
              legend.title = element_blank(),
              legend.text = element_text(size = 10))
      
      
    #When the filter option is Behavior  
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
      
      #Assign colors from a palette to each behavior so that colors remain unchanged in the stacked barplots
      #regardless of the order in which we select them.
      colors <- c(brewer.pal(8, "Set2"), brewer.pal(12, "Paired"), brewer.pal(8, "Dark2"))
      names(colors) = levels(animal_behavior$Behavior)
      colors <- colors[1:length(levels(animal_behavior$Behavior))]
      
      #Creating the visualization
      ggplot(data = animal_behavior) +
        geom_bar(aes(x = Name, y = Percentage, fill = Behavior), stat = "identity", width = .4) +
        labs(title = "Barplot of Selected Behaviors per Animal",
             caption = "Percentages are relative to each animal's total number of observations.",
             x = "Animal Name", y = "Percentage") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1L), 
                           limits = c(0,1)) +
        scale_fill_manual(values = colors) +
        theme(plot.title = element_text(size = 14, face = "bold"),
              plot.subtitle = element_text(size = 12, face = "italic"),
              plot.caption = element_text(size = 12, hjust = 0.5, vjust = -0.5, face = "italic"),
              axis.title = element_text(size = 12, face = "bold"),
              axis.text = element_text(size = 10),
              legend.title = element_blank(),
              legend.text = element_text(size = 10))
      
    }
  })
  
  #Add download link to UI
  output$save_activities <- renderUI({
    req(.activities_visual())
    downloadLink("download_activities_graph", "Download Visual")
  })
  
  #Specify download link
  output$download_activities_graph <- downloadHandler(
    filename = function() {
      paste("activities.jpg")
    }, 
    content = function(file) {
      jpeg(file, width = 900, height = 400)
      plot(.activities_visual())
      dev.off()
    }
  )
  
  
  ######## Summary Table Sub Tab ########
  output$activity_table <- renderTable({
    
    #Get updated data
    animal_data <- data_input()
    
    #Require filter type input
    req(input$filter_type)
    
    #When the filter option is Category
    if(input$filter_type == "Category"){
      
      
      #Display nothing if no Category checkboxes are selected
      if(length(input$category_input) == 0){
        animal_category_table <- data.frame()
        
      #If any of the Category checkboxes are selected, proceed to create a table
      } else {
        
        #Data frame to create summary table for categories
        animal_data$Category <- as.factor(animal_data$Category)
        animal_category_table <- animal_data %>% 
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
        
        
        #Adding a Percentages Column
        animal_category_table <- animal_category_table %>% 
          cbind(Percentage = (animal_category_table$Count/ rep(animal_count,
                                                               times = rep(length(input$category_input), length(animal_count)))) * 100)
        
        #Formatting the Percentage Column
        animal_category_table$Percentage <- format(round(animal_category_table$Percentage, 2), nsmall = 2)
        
        
        #Renaming the Percentage column
        animal_category_table <- rename(animal_category_table, `Percentage (%)` = "Percentage")
        
      }
      
      #Returning the table
      return(animal_category_table)
      
      
    #When the filter option is Behavior
    } else {
      
      
      #Display nothing if no Behavior checkboxes are selected
      if(length(input$behavior_input) == 0){
        animal_behavior_table <- data.frame()
        
      #If any of the Category checkboxes are selected, proceed to create a table  
      } else {
        
        
        #Data frame to create summary table for behaviors
        animal_data$Behavior <- as.factor(animal_data$Behavior)
        animal_behavior_table <- animal_data %>% 
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
        
        
        #Adding a Percentages Column 
        animal_behavior_table <- animal_behavior_table %>% 
          cbind(Percentage = (animal_behavior_table$Count/ rep(animal_count,
                                                               times = rep(length(input$behavior_input), length(animal_count))))*100)
        
        #Formatting the Percentage Column  
        animal_behavior_table$Percentage <- format(round(animal_behavior_table$Percentage, 2), nsmall = 2)
        
        #Renaming the Percentage column 
        animal_behavior_table <- rename(animal_behavior_table, `Percentage (%)` = "Percentage")
      }
      
      #Returning the table 
      return(animal_behavior_table)
      
    }
  })
  
  
  ######## Raw Table Sub Tab ########
  output$raw_activity_table <- renderTable({
    
    #Get updated data
    animal_data <- data_input()
    
    #Require filter type input
    req(input$filter_type)
    
    #When the filter option is Category
    if(input$filter_type == "Category"){
      
      #Checking how many observations there are in total for selected categories
      total_observations <- 0
      
      for(i in input$category_input){
        total_observations <- total_observations + sum(animal_data$Category == i)
      }
      
      #If there are less than 15 observations in total, and at least one category is selected
      if(total_observations <= 15 & total_observations != 0){
        
        #Make the help text disappear
        output$activity_text <- renderUI({
          ""
        })
        
        #Create raw table
        animal_raw_category_table <- animal_data %>% filter(Category %in% input$category_input) %>%
          select(Name, Category, Behavior, Social_Modifier, Date, Time) %>%
          mutate(Time = str_sub(Time, 1,5))
        
        #Format the Date Column
        animal_raw_category_table$Date <- format(animal_raw_category_table$Date, format = "%B %d, %Y")
        
        
        #If Social Modifier Column is empty, remove it from the raw table
        if(sum(is.na(animal_raw_category_table$Social_Modifier)) == nrow(animal_raw_category_table)){
          animal_raw_category_table <- animal_raw_category_table %>% select(-Social_Modifier)
          
        #If Social Modifer Column is not empty, rename the column
        } else {
          animal_raw_category_table <- animal_raw_category_table %>% rename(`Social Modifier` = Social_Modifier)
        }
        
        
      #If there are more than 15 observations in total, or no categories selected
      } else {
        
        #Display help text
        output$activity_text <- renderUI(HTML(paste(
          em("Table appears only if there are 15 or less observations in total for the selected categories")
        )))
        
        #Create an empty table
        animal_raw_category_table <- data.frame()
      }
      
      #Return raw table
      return(animal_raw_category_table)
      
      
    #When the filter option is Behavior   
    } else {
      
      #Checking how many observations there are in total for selected behaviors
      total_observations <- 0
      
      for(i in input$behavior_input){
        total_observations <- total_observations + sum(animal_data$Behavior == i)
      }
      
      #If there are less than 15 observations in total, and at least one behavior is selected  
      if(total_observations <= 15 & total_observations != 0){
        
        #Make the help text dissapear
        output$activity_text <- renderUI({""})
        
        #Create raw table
        animal_raw_behavior_table <- animal_data %>% filter(Behavior %in% input$behavior_input) %>%
          select(Name, Category, Behavior, Social_Modifier, Date, Time) %>%
          mutate(Time = str_sub(Time, 1,5))
        
        #Format the Date Column
        animal_raw_behavior_table$Date <- format(animal_raw_behavior_table$Date, format = "%B %d, %Y")
        
        
        #If Social Modifier Column is empty, remove it from the raw table
        if(sum(is.na(animal_raw_behavior_table$Social_Modifier)) == nrow(animal_raw_behavior_table)){
          animal_raw_behavior_table <- animal_raw_behavior_table %>% select(-Social_Modifier)
          
        #If Social Modifer Column is not empty, rename the column  
        } else {
          animal_raw_behavior_table <- animal_raw_behavior_table %>% rename(`Social Modifier` = Social_Modifier)
        }
        
        
      #If there are more than 15 observations in total, or no behaviors selected  
      } else {
        
        #Display help text
        output$activity_text <- renderUI(HTML(paste(
          em("Table appears only if there are 15 or less observations in total for the selected behaviors")
        )))
        
        #Create an empty table
        animal_raw_behavior_table <- data.frame()
        
      }
      
      #Return raw table
      return(animal_raw_behavior_table)
      
    } 
  })
  
  
  ######## Category Information Sub Tab ########
  output$information_table <- renderTable({
    
    #Get updated data
    animal_data <- data_input()
    
    #Require filter type input
    req(input$filter_type)
    
    
    #When the filter option is Category
    if(input$filter_type == "Category"){
      
      #Make help text disappear
      output$info_text <- renderUI({""})  
      
      #Requring category input
      req(input$category_input)
      
      #Vector of selected categories
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
      
      #Initiating an empty matrix
      info_matrix <- matrix(nrow = max_len + 1, ncol = length(selected_categories))
      
      #Adding the selected categories and behaviors associated with those categories 
      #as columns to the matrix
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
      
      #Converting matrix to a dataframe and using the first row as the column names
      info_data <- data.frame(info_matrix)
      info_data <- info_data %>% row_to_names(row_number = 1)
      
      #Returning the information table
      return(info_data)
      
      
    #When the filter option is Behavior
    } else {
      
      #Display help text
      output$info_text <- renderUI(HTML(paste(
        em("Information Table appears only if you are filtering by Category")
      )))
      
      #Create an empty table
      empty_table <- data.frame()
      
      #Return empty table so only the help text appears
      return(empty_table)
    }
  })
  
  
  ############################### Faceted Barplots ###############################
  # Let user select names of the animals to include in the faceted barplots
  # When a new data file is uploaded, or when the 'deselect all' button is clicked, all checkboxes will be unchecked
  observeEvent(c(input$deselect_all_faceted, input$file1), {
    
    output$nameControls4 <- renderUI({  
      
      animal_data <- data_input()
      
      names <- sort(unique(animal_data$Name))
      checkboxGroupInput(inputId = "names4", 
                         label= h4("Select Animals"),
                         choices = names, 
                         selected = c()) #selected is an empty column
    })
  })
  
  # Select all animal names
  observeEvent(input$select_all_faceted, {
    
    output$nameControls4 <- renderUI({  
      
      animal_data <- data_input()
      
      names <- sort(unique(animal_data$Name))
      checkboxGroupInput(inputId = "names4", 
                         label= h4("Select Animals"),
                         choices = names, 
                         selected = names)
    })
  })
  
  # Let user select a date range
  output$dateControls4 <- renderUI({
    # Get updated data
    animal_data <- data_input()
    
    date <- animal_data$Date
    dateRangeInput("daterange4", h4("Select Date Range"),
                   start = min(animal_data$Date),
                   end = max(animal_data$Date),
                   min = min(animal_data$Date),
                   max = max(animal_data$Date))
  })
  
  # Create the faceted barplots
  output$faceted_barplot <- renderPlot({ .daily_weekly() }, height = 600)
  
  .daily_weekly <- reactive({
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
    animal_data <- animal_data %>% filter(Name %in% animal_name)   
    
    if (nrow(animal_data) == 0) {
      # If filtered dataset is empty, we display an appropriate error message and do not continue to the plot creation
      output$no_data_barplot <- renderUI ({
        error_msg <- character()
        if (is.null(animal_name)) {
          error_msg <- HTML(paste(em("Please select animals from the list")))
        }else if (start_date > end_date) {
          error_msg <- HTML(paste("Invalid date range: start date (", start_date, 
                                  ") must be no later than end date (", end_date, ").", sep = ""))
          error_msg <- HTML(paste(em(error_msg)))
        }else {
          error_msg <- HTML(paste("There is no data for", animal_name, 
                                  "in your selected date range:", start_date, "to", end_date))
          error_msg <- HTML(paste(em(error_msg), ".", sep = ""))
        }
        
        return(error_msg)
      })
      
      return()
    }
    
    # If the dataset is not empty, clear the error messages and continue to plot creation
    output$no_data_barplot <- renderUI ({ })
    
    # Concatenate all animal names to include in the plot caption
    name_in_subtitle <- ""
    for (i in 1:length(animal_name)) {
      name_in_subtitle <- paste(name_in_subtitle, animal_name[i], sep = "")
      if (i != length(animal_name)) name_in_subtitle <- paste(name_in_subtitle, ", ", sep = "")
    }
    
    # Create the plot title and subtitle
    plot_title <- "Barplots of Behavior per"
    plot_subtitle <- paste(start_date, " ~ ", end_date, " (", name_in_subtitle, ")", sep = "")
    
    if (input$select_faceted_barplot == "Day of Week") {
      # Day of Week
      # Change caption of the plot
      plot_title <- paste(plot_title, "Day of Week")
      
      ggplot(data = animal_data) + geom_bar(aes(x = Behavior), fill = "salmon") + 
        facet_wrap(~ Day_of_Week, ncol = 2, dir = "v") + 
        labs(title = plot_title, subtitle = plot_subtitle, y = "Frequency") +
        theme(plot.title = element_text(size = 14, face = "bold"),
              plot.subtitle = element_text(size = 12, face = "italic"),
              plot.caption = element_text(size = 12, hjust = 0.5, vjust = -0.5, face = "italic"),
              axis.title = element_text(size = 12, face = "bold"),
              legend.title = element_blank(),
              legend.text = element_text(size = 10),
              axis.text.x = element_text(size = 10, angle = 90),
              axis.text.y = element_text(size = 10))
      
      
    } else {
      # Hour of Day
      # Change caption of the plot
      plot_title <- paste(plot_title, "Hour of Day")
      
      # Change hour format (e.g., from 9 to "9:00")
      animal_data_hour <- animal_data[order(as.integer(animal_data$Hour)),]
      animal_data_hour$Hour <- sapply(animal_data_hour$Hour, function(x) as.factor(sprintf("%d:00", x)))
      
      ggplot(data = animal_data_hour) + geom_bar(aes(x = Behavior), fill = "salmon") + 
        facet_wrap(~ Hour, ncol = 2, dir = "v") + 
        labs(title = plot_title, subtitle = plot_subtitle, y = "Frequency") +
        theme(plot.title = element_text(size = 14, face = "bold"),
              plot.subtitle = element_text(size = 12, face = "italic"),
              plot.caption = element_text(size = 12, hjust = 0.5, vjust = -0.5, face = "italic"),
              axis.title = element_text(size = 12, face = "bold"),
              legend.title = element_blank(),
              legend.text = element_text(size = 10),
              axis.text.x = element_text(size = 10, angle = 90),
              axis.text.y = element_text(size = 10))
      
    }
  })
  
  # If there is data to generate a valid plot, show the download link
  output$save_daily <- renderUI({
    req(.daily_weekly())
    downloadLink("download_daily", "Download Visual")
  })
  
  # Specify download link
  output$download_daily <- downloadHandler(
    filename = function() {
      paste("daily-weekly.jpg")
    }, 
    content = function(file) {
      jpeg(file, width = 900, height = 600)
      plot(.daily_weekly())
      dev.off()
    }
  )
  
  ############################### Pie Charts ##################################
  
  ##Let users choose the event date
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
  
  #Show Deselect All button only when "Data Without the Subject Animal" is selected
  output$deselect_all_Pie <- renderUI({
    if(input$select_exclusion == "Data Without the Subject Animal") {
      actionButton("deselect_all_Pie", "Deselect All")}
  })
  
  #Show Select All button only when "Data Without the Subject Animal" is selected 
  output$select_all_Pie <- renderUI({
    if(input$select_exclusion == "Data Without the Subject Animal") {
      actionButton("select_all_Pie", "Select All")}
  })
  
  ##Let users choose the subject animal(s) to exclude (with the deselect all button)
  observeEvent(c(input$select_exclusion, input$deselect_all_Pie, input$file1), {
    req(input$select_exclusion)
    
    #Show checkbox group with animal name(s) when "Data Without the Subject Animal" is selected
    output$exclusionControls <- renderUI({
      if(input$select_exclusion == "Data Without the Subject Animal") {
        #Get updated data
        animal_data <- data_input()
        subject_animal <- sort(unique(animal_data$Name))
        checkboxGroupInput("subject_animal", h4("Select Animals to Exclude"), 
                           choices = subject_animal)
      }
    })})
  
  ##Let users choose the subject animal(s) to exclude (with the select all button)
  observeEvent(input$select_all_Pie, {
    req(input$select_exclusion)
    
    #Show checkbox group with animal name(s) when "Data Without the Subject Animal" is selected 
    output$exclusionControls <- renderUI({
      if(input$select_exclusion == "Data Without the Subject Animal") {
        #Get updated data
        animal_data <- data_input()
        subject_animal <- sort(unique(animal_data$Name))
        checkboxGroupInput("subject_animal", h4("Select Animals to Exclude"), 
                           choices = subject_animal,
                           selected = subject_animal)
      }
    })})
  
  #########Pie Chart Plots#########
  
  output$event_pie_plot <- renderPlot({ .events() })
  
  .events <- reactive({
    
    #Get updated data
    animal_data <- data_input()
    #Calls inputs
    req(input$date)
    req(input$select_exclusion)
    
    #####If "All Data" was selected 
    if(input$select_exclusion == "All Data") {
      
      #Create a before dataset (it will be an empty data set when the first date of the data was selected)
      before <- subset(animal_data, Date < input$date)
      #Create an after dataset (it will an empty data set when the last date of the data was selected)
      after <- subset(animal_data, Date > input$date)
    }
    
    #####If "Data Without the Subject Animal" was selected
    if(input$select_exclusion == "Data Without the Subject Animal") {
      #Calls the subject animal(s)
      req(input$subject_animal)
      
      ####No plot if ZERO OR ALL subject animals were selected when "Data Without the Subject Animal" selected
      if(length(input$subject_animal) == 0 & input$select_exclusion == "Data Without the Subject Animal") return()
      if(length(input$subject_animal) == length(unique(animal_data$Name)) & input$select_exclusion == "Data Without the Subject Animal") return()
      
      ####If mutiple (but less than all) subject animals were selected WITH "Data Without the Subject Animal" selected
      if(length(input$subject_animal) > 0 & length(input$subject_animal) < length(unique(animal_data$Name)) &
         input$select_exclusion == "Data Without the Subject Animal") {
        
        #Exclude the selected subject animal(s)
        animal_remain <- sort(unique(animal_data$Name))
        for (a in animal_remain) {
          if (a %in% input$subject_animal) {
            animal_data <- filter(animal_data, Name != a)}}
        
        #Create before and after datasets 
        before <- subset(animal_data, Date < input$date)
        after <- subset(animal_data, Date > input$date)
        
        ###If the selected date was in-between the first and the last day of the data
        if(input$date > min(animal_data$Date) & input$date < max(animal_data$Date)) {
          
          #Create vectors of the animals inside each period ("befor_name" and "after_name")
          #Create a vector of all animals in the original data (eventually this will contain animals that are not in both periods)
          before_name <- sort(unique(before$Name))
          after_name <- sort(unique(after$Name))
          not_mutual_name <- sort(unique(animal_data$Name))
          
          #Find animals that are NOT in both before and after and keep them in the "not_mutual_name" vector
          for(i in before_name) {
            for(j in after_name) {
              if(i == j) {
                not_mutual_name <- not_mutual_name[not_mutual_name != i]}}}
          
          #No plot if all the animals in the original data were NOT in both periods
          if(length(not_mutual_name) == length(unique(animal_data$Name))) return()
          
          #Filter both before and after without the animals we found in the last part
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
          
          #Slice the before subset by the preceding event date and the selected date 
          before <- slice(before, max(last_event_date):nrow(before))
          #Slice the after subset by the selected date and the next event date
          after <- slice(after, 1:min(next_event_date))
        }
        
        ###If the selceted date was the first day of the data
        else if(input$date == min(animal_data$Date)) {
          
          #Create a vector with the animal names in the original data
          after_name <- sort(unique(after$Name))
          next_event <- 0
          next_event_date <- numeric()
          
          #Slice the after subset by after the selected date (the first day of the data) and the next event date
          for (i in after_name) {
            for (j in 1:nrow(after)) {
              if (after$Name[j] == i) {
                next_event <- j}}
            next_event_date <- append(next_event_date, next_event)}
          after <- slice(after, 1:min(next_event_date))
        }
        
        ###If the selceted date was the last day of the data
        else {
          
          #Create a vector with the animal names in the original data
          before_name <- sort(unique(before$Name))
          last_event <- 0
          last_event_date <- numeric()
          
          #Slice the before subset by the last event date and the selected date (the last day of the data)
          for (i in before_name) {
            for (j in nrow(before):1) {
              if (before$Name[j] == i) {
                last_event <- j}}
            last_event_date <- append(last_event_date, last_event)}
          before <- slice(before, max(last_event_date):nrow(before))
        }
      }
    }
    
    ######All processes from here apply to any case
    
    #Create summary data set for before
    before <- before %>% group_by(Behavior)
    summary_before <- as.data.frame(summarise(before, n()))
    names(summary_before)[names(summary_before) == "n()"] <- "counts"
    summary_before <- summary_before %>%
      mutate(Percent = round(counts/sum(counts)*100, 1)) %>%
      mutate(Period = "Before")
    
    #Create summary data set for after
    after <- after %>% group_by(Behavior)
    summary_after <- as.data.frame(summarise(after, n()))
    names(summary_after)[names(summary_after) == "n()"] <- "counts"
    summary_after <- summary_after %>%
      mutate(Percent = round(counts/sum(counts)*100, 1)) %>%
      mutate(Period = "After")
    
    #Combine before and after summary data sets 
    summary <- rbind(summary_before, summary_after)
    summary$Period <- factor(summary$Period, levels = c("Before", "After"))
    
    #Define the color palette
    summary$Behavior <- as.factor(summary$Behavior)
    colors2 <- c(brewer.pal(8, "Set2"), brewer.pal(12, "Paired"), brewer.pal(8, "Dark2"))
    names(colors2) = levels(summary$Behavior)
    colors2 <- colors2[1:length(levels(summary$Behavior))]
    
    #Create pie chart(s)
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
                              plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
                              plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 12),
                              legend.text = element_text(size = 10),
                              legend.position="bottom") +
      scale_fill_manual(values = colors2)
  })
  
  #A text shown when ZERO animal was selected
  output$plz_select <- renderText({
    if (length(input$subject_animal) == 0 & input$select_exclusion == "Data Without the Subject Animal") {
      "There is no plot to display. Please select at least one animal from the list."
    }
  })
  
  #A text shown when ALL animals were selected
  output$no_plot <- renderText({
    #Get updated data
    animal_data <- data_input()
    if(length(input$subject_animal) == length(unique(animal_data$Name)) & input$select_exclusion == "Data Without the Subject Animal") {
      "There is no plot to display. Please unselect at least one animal from the list."
    }
  })
  
  #If there is no data to generate a valid plot, do not show the download link
  output$save_piechart <- renderUI({
    req(.events())
    downloadLink("download_piechart", "Download Visual")
  })
  
  #Specify download link
  output$download_piechart <- downloadHandler(
    filename = function() {
      paste("events.jpg")
    }, 
    content = function(file) {
      jpeg(file, width = 900, height = 400)
      plot(.events())
      dev.off()
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)