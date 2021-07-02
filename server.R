# This file defines the back-end of the application
# server-side functionality

# importing shiny library
library(shiny)

# importing HACSim library
library(HACSim)

# importing ggplot2
library(ggplot2)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$run, {
    # Main interface variables
    permutations <- input$perms
    p <- input$p
    conf.level <- input$conf.level
    
    # find if simulation type is real or hypothetical
    if(input$switch == TRUE){  # Real
      print("REAL")
    }else{ # Hypothetical
      N <- input$N
      Hstar <- input$Hstar
      #probs <- input$probs
      probs <- rep(1/Hstar, Hstar)
      HACSObj <- HACHypothetical(N = N,Hstar = Hstar,probs = probs,perms = permutations, p = p, 
                                   conf.level = conf.level)
      
      output$plot <- renderPlot({
        HAC.simrep(HACSObj)
      })
    }
  })
}
