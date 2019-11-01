library(shiny)

testing = config.testing

eventsLoaderUI = function(id) {
  ns <- NS(id)
  uiOutput(ns("loadData"))
}


eventsLoader = function(input, output, session) {
  ns = session$ns
  
  dataFileTest = eventReactive(input$loadDataT, {
    "../data/tc_prepped_events.csv"
  })
  
  output$loadData = renderUI({
    if(testing)
      actionButton(inputId = ns("loadDataT"), label = "Load Data")
    else
      fileInput(ns("loadData"), "Load Events", accept = '.csv')
  })
  
  return(reactive({
    if(testing) {
      req(dataFileTest())
      
      return(
        dataFileTest() %>% loadEventsClean)
    }
    else {
      req(input$loadData)
      
      return(
        input$loadData$datapath %>% read.csv %>% relativeTimes)
    }
  })
  )
}