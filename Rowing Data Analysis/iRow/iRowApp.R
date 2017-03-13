library(tidyverse)
library(DT)
library(shiny)
library(shinythemes)

dataURL <- "https://docs.google.com/spreadsheets/d/1-iNICFCtUNMGkTd_U7iHB3gInfwnazdGHzkk3PxhUxw/pub?gid=1473113774&single=true&output=csv"
Row_Times <- read_csv(url(dataURL),
                      col_types = cols(Avg.Split = col_time(format = "%M:%OS"),
                                       Date = col_date(format = "%m/%d/%Y"),
                                       Distance = col_number(), Goal.Split = col_time(format = "%M:%OS"),
                                       Last.Split = col_time(format = "%M:%OS"),
                                       SPM = col_integer(), Split = col_time(format = "%M:%OS"),
                                       Exercise = col_character()))
# Row_Times <- read_csv(".csv",
#                        col_types = cols(Avg.Split = col_time(format = "%M:%OS"),
#                                                   Date = col_date(format = "%m/%d/%Y"),
#                                                   Distance = col_number(), Goal.Split = col_time(format = "%M:%OS"),
#                                                   Last.Split = col_time(format = "%M:%OS"),
#                                                   SPM = col_integer(), Split = col_time(format = "%M:%OS"),
#                                                   Exercise = col_character()))

ExerciseTime_long <- Row_Times %>%
  mutate(Name = factor(Name)) %>%
  mutate(Exercise = factor(Exercise)) 

# Individual Rower Averages
rowerAverages <- ExerciseTime_long %>%
  group_by(Name, Date, Exercise) %>%
  select(-Distance) %>%
  summarise_all(mean) %>%
  group_by(Name, Date) %>%
  mutate(Goal.Difference = round(Goal.Split - Split, 3))

# # Individual Rower Variances in Split Time
rowerSplitVar <- ExerciseTime_long %>%
  group_by(Name, Date, Exercise) %>%
  select(c(Name, Date, Exercise, Split)) %>%
  summarise_all(var) %>%
  rename(`Split Variance` = Split)

# # Grand Means for the group
avgGroupGoal  <- mean(rowerAverages$Goal.Split, na.rm = T)
avgGroupSplit <- mean(rowerAverages$Split, na.rm = T)
avgGroupLast  <- mean(rowerAverages$Last.Split, na.rm = T)


# Define UI for application that draws a histogram
ui = fluidPage(
    theme = "spacelab",
    
    titlePanel("iRow App"),
    
    sidebarLayout(
      sidebarPanel(fluidRow(
      selectInput("Name",
                            "Rower(s):",
                            c(
                              "All", unique(as.character(rowerAverages$Name))
                            ), 
                  multiple = TRUE),
       selectInput("Date",
                            "Date(s):",
                            c(
                              "All",
                              unique(as.character(rowerAverages$Date))
                            ),
                   multiple = TRUE),
      
      selectInput("Exercise",
        "Exercise or Test:",
        c("All", unique(as.character(
          rowerAverages$Exercise
        )))
      ))
    ),
    
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Table", DT::dataTableOutput("table")),
                  tabPanel("Group Average", plotOutput("ASplot")),
                  tabPanel("Individual", plotOutput("Indiplot")),
                  tabPanel("Goal - Split Diff.", plotOutput("GSDplot")),
                  tabPanel("Split Distro", plotOutput("Densplot")),
                  tabPanel("Rower Variance", plotOutput("Varplot"))
      ))
    )
)
  
       
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  # Row_Times <- read_csv("/mnt/StorageCell/Dropbox/Rowing Data Analysis/klong.csv", 
  #                       col_types = cols(Avg.Split = col_time(format = "%M:%OS"), 
  #                                        Date = col_date(format = "%m/%d/%Y"), 
  #                                        Distance = col_number(), Goal.Split = col_time(format = "%M:%OS"), 
  #                                        Last.Split = col_time(format = "%M:%OS"), 
  #                                        SPM = col_integer(), Split = col_time(format = "%M:%OS"), 
  #                                        Exercise = col_character()))
  
  # format the data so it looks nice
 
  # Output a filterable data table of Rower Averages
    output$table <- DT::renderDataTable(DT::datatable({
      data <- ExerciseTime_long
      
      if (input$Name != "All") {
        data <- filter(data, Name %in% input$Name)
      }
      
      if (input$Date != "All") {
        data <- filter(data, Date %in% input$Date)
      }
      
      if (input$Exercise != "All") {
        data <- data[data$Exercise == input$Exercise,]
      }
    
    RowerAveragesUserLimited <- data %>%
        group_by(Name, Date, Exercise) %>%
        select(-Distance) %>%
        summarise_all(mean) %>%
        group_by(Name, Date) %>%
        mutate(Goal.Difference = round(Goal.Split - Split, 3)) 
    
    RowerAveragesUserLimited
  }))
  
  
  
  # Output the desired Graph(s?)
  
  #Group Average
  output$ASplot <- renderPlot({
    data <- ExerciseTime_long
    
    if (input$Name != "All") {
      data <- data[data$Name %in% input$Name,]
    }
    
    if (input$Date != "All") {
      data <- data[data$Date %in% input$Date,]
    }
    
    if (input$Exercise != "All") {
      data <- data[data$Exercise == input$Exercise,]
    }
     ggplot(data = data, aes(x = Distance, y = Split)) +
          #Show each rower as a point
          # geom_point() +
          # Show Overall Average Split by Distance
          stat_summary(fun.data = mean_se, geom = "pointrange", na.rm = T) +
          stat_summary(fun.y = mean, geom = "line", na.rm = T, size = 1.5) +
          # Show Additional Goals, Progress, and Global Average
          stat_summary(aes(x = Distance, y = Goal.Split), fun.y = mean, geom = "line", na.rm = T, color = "Green") +
          stat_summary(aes(x = Distance, y = Last.Split), fun.y = mean, geom = "line", na.rm = T, color = "Red"  ) +
          stat_summary(aes(x = Distance, y = Avg.Split),  fun.y = mean, geom = "line", na.rm = T, color = "Blue" ) +
          labs(title = "Group Average - Green line is Goal; Red is Last Exercise; Blue is Global Average") +
          theme_bw()
     })
      
      #"Individual Plot" 
      output$Indiplot <-  renderPlot({
        data <- ExerciseTime_long
        
        if (input$Name != "All") {
          data <- data[data$Name %in% input$Name,]
        }
        
        if (input$Date != "All") {
          data <- data[data$Date %in% input$Date,]
        }
        
        if (input$Exercise != "All") {
          data <- data[data$Exercise == input$Exercise,]
        }
        RowerAveragesUserLimited <- data %>%
          group_by(Name, Date, Exercise) %>%
          select(-Distance) %>%
          summarise_all(mean) %>%
          group_by(Name, Date) %>%
          mutate(Goal.Difference = round(Goal.Split - Split, 3))
        
        ggplot(data, aes(x = Distance, y = Split, group = Name)) +
          facet_wrap( ~ Name, scales = "free") +
          geom_line(size = 1.2) +
          # Show Goal Split
          geom_hline(data = RowerAveragesUserLimited, aes(yintercept = Goal.Split), na.rm = T, color = "Green", alpha = .8) +
          #Show Last Split
          geom_hline(data = RowerAveragesUserLimited, aes(yintercept = Last.Split), na.rm = T, color = "Red", alpha = .8) +
          #Show Average (Mean) Split
          geom_hline(data = RowerAveragesUserLimited, aes(yintercept = Avg.Split),  na.rm = T, color = "Blue",alpha = .8) +
          labs(title = "Rower Splits - Green line is Goal; Red is Last Exercise; Blue is Rower Average") +
          theme_bw()})
      
      #"Group Split Distribution" 
     
      output$Densplot <-  renderPlot({
        data <- ExerciseTime_long
        
        if (input$Name != "All") {
          data <- data[data$Name %in% input$Name,]
        }
        
        if (input$Date != "All") {
          data <- data[data$Date %in% input$Date,]
        }
        
        if (input$Exercise != "All") {
          data <- data[data$Exercise == input$Exercise,]
        }
        
      ggplot(data, aes(x = Split, group = Date, color = Date)) +
        geom_density() +
        # geom_vline(xintercept = avgGroupSplit, color = "Blue") +
        # geom_vline(xintercept = avgGroupGoal, color = "Green") +
        # geom_vline(xintercept = avgGroupLast, color = "Red") +
        labs(title = "Split time distribution by Date") +
        theme_bw()})
      
      #"Goal - Split Difference" 
      
      output$GSDplot <-  renderPlot({
        
      ggplot(data = rowerAverages, aes(x = Name, y = Goal.Difference, fill = Goal.Difference)) +
        geom_bar(stat = "identity") +
        scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) + 
        labs(title = "Goal Split minus Average Split (Positive scores are Good!)", y = "Time Difference") +
        theme_bw()})
      
      #"Rower Split Variance" 
      output$Varplot <-  renderPlot({
        ggplot(data = rowerSplitVar, 
                                      aes(x = Name, y = `Split Variance`, fill = `Split Variance`)) +
        geom_bar(stat = "identity") +
        labs(title = "Variance of Rower's Splits within a Exercise/Date", y = "Split Variance (in seconds)") +
        theme_bw()})

}

# Run the application
shinyApp(ui = ui, server = server)
