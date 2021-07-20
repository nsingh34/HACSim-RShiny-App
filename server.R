# This file defines the back-end of the application
# server-side functionality

# importing shiny library
library(shiny)

# importing HACSim library
library(HACSim)

# importing ggplot2
library(ggplot2)

library(shinymeta)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$run, {
    # Main interface variables
    permutations <- input$perms
    p <- input$p
    conf.level <- input$conf.level
    
    # find if simulation type is real or hypothetical
    if(input$switch == TRUE){  # Real
      
      if(input$Id015 == TRUE){ # Pre loaded example
        N <- input$N_load
        Hstar <- input$Hstar_load
        x <- input$probs_load
        split_str <- strsplit(x, ",")
        probs <- as.numeric(unlist(split_str))
        output$plot <- renderPlot({
          validate(
            need(N >= Hstar,'N must be greater than or equal to Hstar'),
            need(N > 1,'N must be greater than 1'),
            need(Hstar > 1,'H* must be greater than 1'),
            need(isTRUE(all.equal(1, sum(probs), tolerance = .Machine$double.eps^0.25)),'probs must sum to 1'),
            need(length(probs) == Hstar,'probs must have Hstar elements'),
            need((permutations > 1),'perms must be greater than 1'),
            need((p > 0) && (p <= 1),'p must be greater than 0 and less than or equal to 1')
          )
          HACSObj <- HACHypothetical(N = N,Hstar = Hstar,probs = probs,perms = permutations, p = p, 
                                     conf.level = conf.level,
                                     subsample = FALSE,
                                     progress = TRUE, num.iters = NULL, filename = NULL)
          
        })
      }else{
        filename <- input$file
        subsample <- input$subsampleseqs
        prop = input$prop
        if(subsample == FALSE){
          prop = NULL
        }
        # creating a HACSObj object by running HACReal()
        HACSObj <- HACReal(perms = permutations, p = p ,conf.level = 0.95,
                           subsample = subsample, prop = prop, progress = TRUE,
                           num.iters = NULL, 
                           filename = "output")
        output$plot <- renderPlot({
          HAC.simrep(HACSObj)
        })
      }
      
      
    }else{ # Hypothetical
      N <- input$N
      Hstar <- input$Hstar
      x <- input$probs
      split_str <- strsplit(x, ",")
      probs <- as.numeric(unlist(split_str))
      subsample <- input$subsampleseqs_2
      prop = input$prop
      if(subsample == FALSE){
        prop = NULL
      }
      result <- metaRender(renderPrint,{
        validate(
          need(N >= Hstar,'N must be greater than or equal to Hstar'),
          need(N > 1,'N must be greater than 1'),
          need(Hstar > 1,'H* must be greater than 1'),
          need(isTRUE(all.equal(1, sum(probs), tolerance = .Machine$double.eps^0.25)),'probs must sum to 1'),
          need(length(probs) == Hstar,'probs must have Hstar elements'),
          need((permutations > 1),'perms must be greater than 1'),
          need((p > 0) && (p <= 1),'p must be greater than 0 and less than or equal to 1')
        )
        HACSObj <- HACHypothetical(N = N,Hstar = Hstar,probs = probs,perms = permutations, p = p, 
                                   conf.level = conf.level,
                                   subsample = subsample, prop = prop,
                                   progress = TRUE, num.iters = NULL, filename = NULL)
        HAC.simrep(HACSObj)
        
      })
      output$plot <-renderPlot({
        result()
      })
      output$text <- renderPrint({
        result()
      })
    }
  })
}
