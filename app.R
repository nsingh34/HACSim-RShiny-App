#
# This is a HACSim R Shiny Web Application. You can run the application by clicking
# the 'Run App' button above.
#

# importing shiny library
library(shiny)

# load ui elements
source("ui.R")
# load server function
source("server.R")

# Run the application 
shinyApp(ui = ui, server = server)
