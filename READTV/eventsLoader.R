library(shiny)

eventsLoaderUI = function(id) {
  ns <- NS(id)
  fileWellUI(ns("filewell"))
}


eventsLoader = function(input, output, session) {
  ns = session$ns
  
  eventDataF = callModule(fileWellServer, "filewell", "Event Data", '../data/tc_prepped_events.csv')
  
  return(reactive({
    req(eventDataF())
    
    if(config.testing) {
      eventDataF()$datapath %>% loadEventsClean
    } else {
      eventDataF()$datapath %>% read.csv %>% relativeTimes
    }
  })
  )
}