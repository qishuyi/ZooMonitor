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
  # TODO: Create plot here
}

############################### Category ###############################
category <- function(input, output, animal_data) {
  # TODO: Create plot here
}

############################### Behavior ###############################
behavior <- function(input, output, animal_data) {
  # TODO: Create plot here
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
    plot_caption <- paste(animal_name, ": Barplots of Behaviors per")
    if (input$select_faceted_barplot == "Time of Day") {
      # Change caption of the plot
      plot_caption <- paste(plot_caption, "Hour")
      ggplot(data = animal_data) + geom_bar(aes(x = Behavior), fill = "salmon") + 
        facet_wrap(~ Hour, ncol = 3) + 
        theme(axis.text.x = element_text(angle = 90, size = 10)) + 
        labs(title = plot_caption, y = "Frequency")
    }else {# Day of Week
      # Change caption of the plot
      plot_caption <- paste(plot_caption, "Day of Week")
      ggplot(data = animal_data) + geom_bar(aes(x = Behavior), fill = "salmon") + 
        facet_wrap(~ Day_of_Week, ncol = 3) + 
        theme(axis.text.x = element_text(angle = 90, size = 10)) + 
        labs(title = plot_caption, y = "Frequency")
    }
    }, height = 600)
}

############################### Pie Charts ###############################
piechart <- function(input, output, animal_data) {
  # TODO: Create plot here
}


############### UI and Server ###############
ui <- navbarPage("ZooMonitor",
                 ############################### Upload Data ###############################
                 tabPanel("Upload Data",
                          
                          titlePanel("Upload a csv file"),
                          
                          sidebarLayout(
                            # Add filters to take user inputs
                            sidebarPanel(
                              # Allow users to upload a csv file
                              fileInput("file1", "Choose CSV File",
                                        multiple = TRUE,
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv"))
                            ),
                            # Display the data file
                            mainPanel(
                              dataTableOutput("contents")
                            ))),
                 
                 ############################### General Observations ###############################
                 tabPanel("General Observations",
                          
                          titlePanel("Distribution of Observations"),
                          
                          sidebarLayout(
                            sidebarPanel(
                              # TODO: Add filters here to get user inputs
                            ),
                            mainPanel(
                              # TODO: Show plots here
                            ))),
                 
                 ############################### Category ###############################
                 tabPanel("Category",
                          
                          titlePanel("Placeholder for title 2"),
                          sidebarPanel(
                            # TODO: Add filters here to get user inputs
                          ),
                          mainPanel(
                            # TODO: Show plots here
                          )),
                 
                 ############################### Behavior ###############################
                 tabPanel("Behavior",
                          
                          titlePanel("Placeholder for title 3"),
                          sidebarPanel(
                            # TODO: Add filters here to get user inputs
                          ),
                          mainPanel(
                            # TODO: Create plots here
                          )),
                 
                 ############################### Faceted Barplot ###############################
                 tabPanel("Faceted Barplot",
                          
                          titlePanel("Frequency of Animal Behaviors"),
                          sidebarPanel(
                            uiOutput("dateControls4"),
                            radioButtons("select_faceted_barplot", "Show behavior by:",
                                         choices = list("Time of Day", "Day of Week")),
                            uiOutput("nameControls4")
                          ),
                          mainPanel(
                            # Create the faceted barplot for frequency of behaviors
                            plotOutput("faceted_barplot")
                          )),
                 
                 ############################### Pie Chart ###############################
                 tabPanel("Pie Chart",
                          
                          titlePanel("Event's Impact on Behaviors"),
                          
                          sidebarLayout(
                            # Add filters to take user inputs
                            sidebarPanel(
                              # TODO: Add filters here to get user inputs
                            ),
                            mainPanel(
                              # TODO: Create plots here
                            ))))

# Define server logic
server <- function(input, output) {
  
  source("generalized_cleaning.R")
  
  ############################### Upload Data ###############################
  output$contents <- renderDT({
    
    # input$file1 will be NULL initially, the req function ensures the value of input$file1 is available.
    # If input$file1 is unavailable, req will throw an exception.
    req(input$file1)
    
    #Loading in Data
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
    
    animal_data <- animal_data %>% unite("Category", IC_Name_Vector, remove = T)                                                                 
    
    
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
    # TODO: Create the reactive filter options here
    category(input, output, animal_data)
    
    ############################### Behavior ###############################
    # TODO: Create the reactive filter options here
    behavior(input, output, animal_data)
    
    ############################### Faceted Barplots ###############################
    #Let user select an animal name
    output$nameControls4 <- renderUI({
      prefix <- c('All animals')
      names <- unique(animal_data$Name)
      names <- c(prefix, names)
      radioButtons('names4', "Choose Animal", names)
    })
    #Let user select a date range
    output$dateControls4 <- renderUI({
      date <- animal_data$Date
      dateRangeInput("daterange4", "Select a date range:",
                start = min(animal_data$Date),
                end = max(animal_data$Date),
                min = min(animal_data$Date),
                max = max(animal_data$Date))
    })
    #Create the faceted barplots
    facetedBarplots(input, output, animal_data)
    
    ############################### Pie Charts ###############################
    piechart(input, output, animal_data)
    
    return(animal_data)
    
  })
  
  ## Note: Everything below that's within the server function might be deleted in the final version,
  ##       depending on whether we want to provide a default data file for the users.
  
  ############################### General Observations ###############################
  generalplot(input, output, animal_data)
  
  ############################### Category ###############################
  # TODO: Create the reactive filter options here
  category(input, output, animal_data)
  
  ############################### Behavior ###############################
  # TODO: Create the reactive filter options here
  behavior(input, output, animal_data)
  
  ############################### Faceted Barplots ###############################
  # This function is here because it will be easier to not have to upload data files every time we try to
  # run the APP during the prototyping process. It probably will be removed in the final APP.
  output$nameControls4 <- renderUI({
    prefix <- c('All animals')
    names <- unique(animal_data$Name)
    names <- c(prefix, names)
    radioButtons('names4', "Choose Animal", names)
  })
  #Let user select a date range
  output$dateControls4 <- renderUI({
    date <- animal_data$Date
    dateRangeInput("daterange4", "Select a date range:",
                   start = min(animal_data$Date),
                   end = max(animal_data$Date),
                   min = min(animal_data$Date),
                   max = max(animal_data$Date))
  })
  #Create the faceted barplots
  facetedBarplots(input, output, animal_data)
  
  ############################### Pie Charts ###############################
  # TODO: Create the reactive filter options here
  piechart(input, output, animal_data)
}

# Run the application 
shinyApp(ui = ui, server = server)
