library(shiny)
library(shinyjs)

fluidPage(
  actionButton("addDisplay", "Add Display"),
  uiOutput("eventDisplayer")
)