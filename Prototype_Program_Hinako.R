#Loading in Libraries
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(shiny)
library(ggplot2)

### Note: 
### Currently, if no data file is uploaded, the APP will be using the generalized cleaning script by default,
### which uses dogs' data (line 97). To use another dataset, either upload the data file or change the 
### filepath in generalized_cleaning.R.

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
                              tableOutput("contents")
                            ))),
                 
                 ############################### General Observations ###############################
                 tabPanel("General Observations",
                          
                          titlePanel("Distribution of Observations"),
                          
                            sidebarLayout(
                              # Add filters to take user inputs
                              sidebarPanel(
                                # Allow users to choose the x-axis
                                radioButtons("select_general", "Choose a period",
                                             choices = c("Time of Day" = "Hour", "Day of Week" = "Day_of_Week")
                                             )
                              ),
                          mainPanel(
                            # Show the plot of general obervations
                            plotOutput("general_plot"),
                            textOutput("selected_select_general")
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
                          )
                 ),
                 
                 ############################### Faceted Barplot ###############################
                 tabPanel("Faceted Barplot",
                          
                          titlePanel("Placeholder for title 4"),
                          sidebarPanel(
                            
                            uiOutput("nameControls")
                            
                            # TODO: If necessary, add more filters here to get user inputs
                          ),
                          mainPanel(
                            # TODO: Show plots here
                          )),
                 
                 ############################### Pie Chart ###############################
                 tabPanel("Pie Chart",
                          
                          titlePanel("Event's Impact on Behaviors"),
                          
                          sidebarLayout(
                            # Add filters to take user inputs
                            sidebarPanel(
                              # Allow users to choose the x-axis
                              dateInput("date", "Date of the event:",
                                        value = NULL #TODO:add min/max
                            )),
                            mainPanel(
                              # Show the plot of general obervations
                              plotOutput("event_pie_plot")
                            ))))

# Define server logic
server <- function(input, output) {
  
  source("generalized_cleaning.R")
  
  ############################### Upload Data ###############################
  output$contents <- renderTable({
    
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
    animal_data <- animal_data %>% 
      mutate(Social_Modifier = gsub("NA_", "", Social_Modifier)) %>%
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
    
    ################### Add filter options for animal names based on the input data file
    output$nameControls <- renderUI({
      names <- unique(animal_data$Name)
      checkboxGroupInput('names', "Choose Animal", names)
    })
    
    return(animal_data)
    
  })
  
  ############################### General Observations ###############################
  ##Bar plot of observation distribution
  output$general_plot <- renderPlot({
    
    ggplot(data = animal_data) + 
    geom_bar(aes(x = input$select_general, y = ..count../nrow(animal_data)*100), fill = "steelblue", width = .75) +
    labs(title = "Percentage of Observations", x = "Period", y = "Percentage (%)") 
    
  })
  ############################### Category ###############################
  
  ############################### Behavior ###############################
  
  ############################### Faceted Barplots ###############################
  # This function is here because it will be easier to not have to upload data files every time we try to
  # run the APP during the prototyping process. It probably will be removed in the final APP.
  output$nameControls <- renderUI({
    names <- unique(animal_data$Name)
    checkboxGroupInput('names', "Choose Animal", names)
  })
  
  ############################### Pie Charts ###############################
  
}

# Run the application 
shinyApp(ui = ui, server = server)



