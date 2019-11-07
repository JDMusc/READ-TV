library(shiny)
library(shinyjs)

testing = config.testing

eventsLoaderUI = function(id) {
  ns <- NS(id)
  uiOutput(ns("loadData"))
}


eventsLoader = function(input, output, session) {
  ns = session$ns
  
  eventDataF = callModule(fileWellServer, "filewell", "Event Data", '../data/tc_prepped_events.csv')
  
  serverState = reactiveValues(is_minimized = F)
  
  fileLoaded = reactive({
    req(input$loadDataF)
    
    return(!is.null(input$loadDataF))
  })
  
  dataFileTest = eventReactive(input$loadDataT, {
    "../data/tc_prepped_events.csv"
  })
  
  output$loadData = renderUI({
    if(testing)
      actionButton(inputId = ns("loadDataT"), label = "Load Data")
    else
      wellPanel(
        uiOutput(ns("minimize")),
        fileInput(ns("loadDataF"), "Load Events", accept = '.csv')
      )
  })
  
  output$minimize = renderUI({
    if(fileLoaded())
      actionButton(inputId = ns("minimize"), label = "Minimize")
    else
      NULL
  })
  
  observeEvent(input$minimize, {
    serverState$is_minimized = !serverState$is_minimized
    label = ifelse(serverState$is_minimized, 
                   "Load New Events", "Minimize")
    updateActionButton(session, "minimize", label)
  })
  
  observe({
    minimize = serverState$is_minimized
    if(minimize)
      shinyjs::hide("loadDataF")
    else
      shinyjs::show("loadDataF")
  })
  
  return(reactive({
    if(testing) {
      req(dataFileTest())
      
      return(
        dataFileTest() %>% loadEventsClean)
    }
    else {
      req(input$loadDataF)
      
      return(
        input$loadDataF$datapath %>% read.csv %>% relativeTimes)
    }
  })
  )
}