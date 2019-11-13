library(dplyr)
library(ggplot2)
library(shiny)
library(shinyjs)


function(input, output, session){
  displayCount = reactiveVal(0)
  currentEventId = reactive({
    paste0("eventsDisplay", displayCount())
  })
  
  eventDisplays = reactiveValues()
  
  observeEvent(input$addDisplay, {
    displayCount(displayCount() + 1)
    callModule(eventsDisplayServer, currentEventId())
    eventDisplays[[currentEventId()]] = eventsDisplayUI(currentEventId())
  })
  
  output$eventDisplayer = renderUI({
    div(
      reactiveValuesToList(eventDisplays)
    )
  })
}
