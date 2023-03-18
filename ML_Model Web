library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(randomForest)
library(flexdashboard)
library(caret)

#read data

# Read in the RandomForest model
model <- readRDS("model.rds")

TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]


####################################
# User interface                   #
####################################

ui <- fluidPage(theme = shinytheme("united"),
                  
                navbarPage(
                  # theme = "cerulean, superh",  # <--- To use a theme, uncomment this
                  "My CSCI 5010 Research Project",
                  tabPanel("Project Manager Access",
                           sidebarPanel(
                             tags$h3("Login:"),
                             textInput("txt1", "FirstName:", ""),
                             textInput("txt2", "LastName:", ""),
                             
                           ), # sidebarPanel
                           mainPanel(
                             h1("Welcome, Lekan!"),
                             
                             h4("Kindly login to see the updated project success"),
                             verbatimTextOutput("txtout"),
                             
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                
                #page Header
                tabPanel("Project Plan Prediction", 
                
                #input value
                sidebarPanel(
                  fileInput("file","upload the Training Data"), #fileinput() function is used to get file upload control option
                  helpText("Please select the training data"),
                  
                  HTML("<h5>Input parameters</h5>"),
                  
                selectInput("country", label = "project country:", 
                            choices = list("US"="US", "GB"="GB", "AT"="AT", "AU"="AU", "BE"="BE", "CA"="CA", "CH"="CH", "DE"="DE", "DK"="DK", "ES"="ES", "FR"="FR", 
                                           "HK"="HK", "IE"="IE", "IT"="IT", "LU"="LU", "MX"="MX", "NL"="NL", "NO"="NO", "NZ"="NZ", "SE"="SE", "SG"="SG"),
                            selected = "US"),
                sliderInput("usd_pledge", label = "Amount pledged in usd:",
                            value = 1000, min=min(TrainSet$usd_pledge), 
                            max=max(TrainSet$usd_pledge)),
                sliderInput("pledged", label = "Amount pledged:",
                             value = 10000, min=min(TrainSet$pledged), 
                             max=max(TrainSet$pledged)),
                sliderInput("goal", label = "project goal amount:",
                             value = 10000, min=min(TrainSet$goal), 
                             max=max(TrainSet$goal)),
                selectInput("main_category", label = "project category:", 
                            choices = list("Art"="Art", "Comics"="Comics", "Crafts"="Crafts", "Dance"="Dance", "Design"="Design", "Fashion"="Fashion", "FilmVideo"="FilmVideo", "Food"="Food", 
                                           "Games"="Games", "Journalism"="Journalism", "Music"="Music", "Photography"="Photography", "Publishing"="Publishing", "Technology"="Technology", "Theater"="Theater"),
                            selected = "Art"),
                sliderInput("backers", label = "project backers:",
                            value = 7000, min=min(TrainSet$backers), 
                            max=max(TrainSet$backers)),
               
                submitButton("submit")
              
              ),
              
              mainPanel(
                tags$label(h3('Status/Output')),
                verbatimTextOutput('contents'),
                tableOutput('tabledata'), #Prediction results tables
                #output like Histogram
                gaugeOutput('Scale')
              )
          ),
    )
                
)


####################################
# Server                           #
####################################

server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("country",
               "usd_pledge",
               "pledged",
               "goal",
               "main_category",
               "backers"),
      Value = as.character(c(input$country,
                             input$usd_pledge,
                             input$pledged,
                             input$goal,
                             input$main_category,
                             input$backers)),
      stringsAsFactors = FALSE)
    
    state <- "state"
    
    df <- rbind(df, state)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    #write.table(input,"input4.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    test$country <- factor(test$country, levels = c("US", "GB", "AT", "AU", "BE", "CA", "CH", "DE", "DK", "ES", "FR", 
                                                    "HK", "IE", "IT", "LU", "MX", "NL", "NO", "NZ", "SE", "SG"))
    test$main_category <- factor(test$main_category, levels = c("Art", "Comics", "Crafts", "Dance", "Design", "Fashion", "FilmVideo", "Food", 
                                                                "Games", "Journalism", "Music", "Photography", "Publishing", "Technology", "Theater"))
    
    Output <- data.frame(Prediction=predict(model, test), round(predict(model,test,type="prob"), 3)*100)
    print(Output)
    
  })
  
  my_plot <- reactive({
      gauge(datasetInput()$successful, min = 0, max = 100, label= 'success', symbol = "%", 
            sectors = gaugeSectors(
                                  success = c(80, 100), 
                                  warning = c(40, 79), 
                                  danger = c(0, 39), colors = c('green', 'yellow', 'red')))
      
  })
    
  #datasetInput()
    output$Scale <- renderGauge({
      my_plot()
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    "Forecast Results"
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    datasetInput()
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)
