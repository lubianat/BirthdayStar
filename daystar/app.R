#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
db <- read.csv('./hygdata_v3.csv')

db_2 <- db %>% select(c('hip','proper','dist', 'mag','con'))

db_2$days <- db_2$dist * 3.262 * 365



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Star of the Day"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("mag",
                     "Max magnitude (the lower, the brighter!)",
                     min = -1.5,
                     max = 20,
                     step = 0.1,
                     value = 6.5),
      dateInput(inputId = 'date',
                label = 'Date of Birth (dd-mm-yyyy)', 
                format = "dd-mm-yyyy")
        ),
      
      # Show a plot of the generated distribution
      mainPanel(
        textOutput('lifetime'),
        tableOutput("STAR")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  days_ <- eventReactive(input$date,{
    birthdate <- input$date
    today <- Sys.Date()
    as.numeric(today - birthdate)
    
  })
  
  data_STAR <- eventReactive(input$mag,{
    birthdate <- input$date
    
    db_2 <- db_2[db_2$mag < input$mag,]
    
    today <- Sys.Date()
    
    days_of_life <- as.numeric(today - birthdate)
    
    star_index <- which(abs(db_2$days-days_of_life)==min(abs(db_2$days-days_of_life)))
    
    return(db_2[star_index,])
  })
  output$lifetime <- renderText(paste0('You had already ', days_(), ' of life'))
  output$STAR <- renderTable({data_STAR() })
}

# Run the application 
shinyApp(ui = ui, server = server)

