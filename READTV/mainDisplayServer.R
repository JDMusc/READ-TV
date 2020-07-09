
mainDisplayServer = function(input, output, session){
  ns = session$ns
  
  dataUploadTab = callModule(dataUploadTabServer, "dataUpload")
  data = reactive({dataUploadTab$data()})
  eventsInformation = dataUploadTab$eventsInformation
  headerMinimalInformation = reactive({
    dataUploadTab$headerMinimalInformation()
  })
  isDataLoaded = reactive({dataUploadTab$isDataLoaded()})
  
  metaDataFile <- callModule(metaQueryLoader, "loadMetaData")
  
  filteredMetaData <- callModule(metaQueryServer, "metaqueryui", 
                                 metaDataFile)
  
  dataFilter = callModule(dataFilterServer, "dataFilter", data)
  
  filteredData = reactive({
    req(isDataLoaded())
    dataFilter$filteredData()
  })
  
  customizeDisplay = callModule(customizeDisplayServer, "customizeDisplay", 
                                filteredData)
  
  output$dataFilter = renderUI({
    if(isDataLoaded()) dataFilterUI(ns("dataFilter"))
  })
  
  hist <- reactive({
    req(isDataLoaded())
    
    ggplot(filteredData(), aes(x = deltaTime)) +
      geom_histogram(fill = "black") + 
      xlab("Time Between Events") +
      ylab("Event Count")
  })
  
  event_colors = eventTypeColors()
  
  updateTimePlotCountDebug = printWithCountGen('time plot')

  timePlot <- reactive({
    req(isDataLoaded())
    req(dataFilter$hasValidQuery() |  !dataFilter$hasQueryInput())

    #showTab("tabs", "Source Code")
    cpa_params = calcCpa
    if(length(names(calcCpa)) == 0) cpa_params = NULL
    
    generateTimePlot(filteredData(), customizeDisplay, cpa_params)
  })
  
  doFacet = reactive({
    if(isDataLoaded()) !(
      customizeDisplay$facetRowsPerPage == customizeDisplay$no_selection)
    else F
  })
  
  facetPageN <- reactive({
    if(!doFacet()) -1
    else n_pages(timePlot())
  })
  
  eventStats <- reactive({
    summary(filteredData()$deltaTime)
  })
  
  plotHeight = reactive({
    if(is.null(customizeDisplay$plotHeight)) 400
    else customizeDisplay$plotHeight
  })
  
  output$facetPageSlider = renderUI({
    f = stringr::str_interp
    page = customizeDisplay$facetPage
    if(doFacet()) selectInput(
      ns("facetPageSlider"), 
      f("Facet Page (${page} out of ${facetPageN()})"), 
      1:facetPageN(), 
      selected = page)
    else NULL
  })
  
  observeEvent(input$facetPageSlider, {
    pg = as.numeric(input$facetPageSlider)
    customizeDisplay$facetPage = pg
  })
  
  output$eventPlotContainer = renderUI({
    fluidPage(
      plotOutput(ns("eventPlot"), 
                 height = plotHeight(),
                 brush = brushOpts(ns("event_plot_brush"),
                                   direction = "x",
                                   resetOnNew = T),
                 dblclick = clickOpts(ns("event_plot_dblclick"))
                 ),
      uiOutput(ns("facetPageSlider"))
    )
  })
  
  
  observeEvent(input$event_plot_dblclick, {
    brush = input$event_plot_brush
    if(!is.null(brush)) {
      left = brush$xmin
      right = brush$xmax
      
      dataFilter$constraints$RelativeTime = function(rt) rt >= left & rt <= right
    }
    else dataFilter$constraints$RelativeTime = function(rt) T
  })
  
  
  output$eventPlot = renderPlot({
    req(isDataLoaded())
    timePlot()
  })
  
  output$sidePanel = renderUI({
    req(isDataLoaded())
    
    tabsetPanel(
      tabPanel("Display", 
               customizeDisplayUI(ns("customizeDisplay"))),
      tabPanel("CPA", cpaUI(ns("calcCPA"))),
      tabPanel("Event Statistics", 
               uiOutput(ns("showEventStats"), label = "Basic Statistics")),
      tabPanel("Download Data", uiOutput(ns("downloadDataOutput")))
    )
  })
  
  output$eventStats = renderPrint({
    req(isDataLoaded())
    
    eventStats()
  })
  
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
  
  calcCpa = callModule(cpaServer, "calcCPA", filteredData,
                       customizeDisplay)
  
  output$showEventStats = renderUI({
    if(isDataLoaded())
      actionButton(inputId = ns("showEventStats"), "Basic Statistics")
  })
  
  observeEvent(input$showEventStats, {
    callModule(showEventStats, "", data=filteredData)
  })

  sourceCode <- callModule(sourceCodeServer, "sourcecode", 
			   customizeDisplay, dataFilter,
  			   eventsInformation, isDataLoaded)
}
