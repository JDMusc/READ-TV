
cpaTabServer = function(input, output, session, filteredData, 
                        headerMinimalInformation, isDataLoaded,
                        customizeDisplay){
  ns = session$ns
  
  #----Plot----
  timePlot <- reactive({
    req(filteredData())

    cpa_params = calcCpa
    if(length(names(calcCpa)) == 0) cpa_params = NULL
    
    generateTimePlot(filteredData(), customizeDisplay, cpa_params)
  })
  
  output$eventPlot = renderPlot({
    req(isDataLoaded())
    timePlot()
  })
  
  output$eventPlotContainer = renderUI({
    fluidPage(
      plotOutput(ns("eventPlot"), 
                 height = customizeDisplay$plotHeight
      ),
      uiOutput(ns("facetPageSlider"))
    )
  })
  
  
  #----Side Panel ----
  output$sidePanel = renderUI({
    req(isDataLoaded())
    
    tabsetPanel(
      tabPanel("Preprocess"),
      tabPanel("CPA Parameters", cpaUI(ns("calcCPA")))
    )
  })
  
  
  #----CPA----
  calcCpa = callModule(cpaServer, "calcCPA", filteredData,
                       customizeDisplay)
  
  
  #----Return----
  return(list(
  ))
}
