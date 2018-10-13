#

# This is a Shiny web application. You can run the application by clicking

# the 'Run App' button above.

#

# Find out more about building applications with Shiny here:

#

#    http://shiny.rstudio.com/

#



library(shiny)



# Define UI for application that draws a histogram

#shinyUI(fluidPage(

ui <- fluidPage(
  

  # Application title
  
  titlePanel("Beer ABV to Rating Correlation based on number of Reviewers, adjustable and dynamic."),
  
  # Sidebar with a slider input for number of bins
  
  sidebarLayout(
    
    
    sidebarPanel(
      
      helpText("The range 'slider' controls the input range of the raters as a 3rd variable."),
      
      sliderInput("raters",
                  
                  label = "Raters Range Slider:",
                  
                  min = 45,
                  
                  max = 10000,
                  
                  value = c(100,2000)),
      
      
      helpText("The 'Ratings' allows you to select the Ratings data range you want to see on the Y axis."),
      
      sliderInput("rating",
                  
                  label = "Ratings:",
                  
                  min = 0,
                  
                  max = 5,
                  
                  value = c(0,5))
      
      
      
      #              numericInput("rating",
      
      #                            label = "Ratings:", 3, min = 0, max = 4)
      
      
      
      ),
    
    
    
    # Show a plot of the generated distribution
    
    mainPanel( plotOutput("distPlot") )
    
    
    
    
    
    )
  
)





# Define server logic required to draw a histogram

server <- function(input, output) {
  
  dataset <- read.csv("dataset.csv") # read the dataset into the shinyapps.io cloud
  
  library("ggpubr")
  
  output$distPlot <- renderPlot({
  
    # write a correlation R Code here
    
    ggscatter(subset(dataset, raters > input$raters[1] & raters < input$raters[2] & rating > input$rating[1] & rating < input$rating[2]),
              
              color = "orange", shape = 21,
              
              x = "ABV", y = "rating",
              
              add = "reg.line", add.params = list(color = "blue", fill = "lightgrey"),
              
              
              
              conf.int = TRUE,
              
              cor.coef = TRUE, cor.method = "pearson",
              
              xlab = "ABV", ylab = "Rating")    
    
    
    
  })
  
}



# Run the application

shinyApp(ui = ui, server = server)