library(shiny)
library(shinyjs)


fileWellUI = function(id, fileType) {
  ns = NS(id)
  uiOutput(ns("loadData"))
}


fileWellServer = function(input, output, session, fileType, testFile){
  ns = session$ns
  
  isMinimized = reactiveVal(F)
  
  fileLoaded = reactive({
    return(!is.null(input$loadF))
  })
  
  output$loadData = renderUI({
    label = paste("Load", fileType)
    if(config.testing)
      actionButton(inputId = ns("loadDataTest"), label = label)
    else
      wellPanel(
        uiOutput(ns("minimize")),
        fileInput(ns("loadF"), label, accept = '.csv'))
  })
  
  minimizeLabel = reactive({
    if(isMinimized()) {
      label = paste("Load", fileType)
      if(fileLoaded()) label = paste("Load New", fileType)
    }
    else {
      label = "Minimize"
    }
    
    return(label)
  })
  
  output$minimize = renderUI({
    actionButton(inputId = ns("minimize"), label = minimizeLabel())
  })
  
  observeEvent(input$minimize, {
    isMinimized(!isMinimized())})
  
  observe({
    if(isMinimized())
      shinyjs::hide("loadF")
    else
      shinyjs::show("loadF")
  })
  
  return(reactive({
    if(config.testing){
      list(name = testFile, datapath = testFile)
    } else {
      req(input$loadF)
      input$loadF
    }
  }))
}