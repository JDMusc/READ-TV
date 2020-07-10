
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
      tabPanel("Display"),
      tabPanel("CPA", cpaUI(ns("calcCPA"))),
      tabPanel("Event Statistics", 
               uiOutput(ns("showEventStats"), label = "Basic Statistics")),
      tabPanel("Download Data", uiOutput(ns("downloadDataOutput")))
    )
  })
  
  
  #----Event Stats----
  output$showEventStats = renderUI({
    if(isDataLoaded())
      actionButton(inputId = ns("showEventStats"), "Basic Statistics")
  })
  
  observeEvent(input$showEventStats, {
    callModule(showEventStats, "", data=filteredData)
  })
  
  #----Download Data----
  output$downloadData <- downloadHandler(
    filename = function() {
      hmi = headerMinimalInformation() %>% 
        {gsub(', ?', '-', .)} %>%
        {gsub('.csv','', .)}
      paste0(hmi, "-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(filteredData(), file, row.names = F)
    }
  )
  
  output$downloadDataOutput = renderUI({
    if(isDataLoaded())
      downloadButton(ns("downloadData"))
  })
  
  #----CPA----
  calcCpa = callModule(cpaServer, "calcCPA", filteredData,
                       customizeDisplay)
  
  
  #----Return----
  return(list(
  ))
}
