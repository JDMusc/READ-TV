
eventsDisplayServer = function(input, output, session){
  ns = session$ns
  
  isHeaderMinimized = reactiveVal(F)
  isDataLoaded = reactiveVal(F)
  
  eventsInformation = callModule(eventsLoader, "loadData")
  
  data <- reactive({
    req(eventsInformation$name())
    
    tbl = eventsInformation$data()
    if(isMetaDataLoaded()) {
      tbl = tbl %>% filter(Case %in% filteredMetaData()$data$Case)
    }
    isDataLoaded(T)
    tbl
  })
  
  metaDataFile <- callModule(metaQueryLoader, "loadMetaData")
  
  filteredMetaData <- callModule(metaQueryServer, "metaqueryui", 
                                 metaDataFile)
  
  isMetaDataLoaded = reactive({
    fmd = try(filteredMetaData(), silent = T)
    
    return(!(class(fmd) == "try-error"))
  })
  
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

    #showTab("tabs", "Source Code")
    generateTimePlot(filteredData(), customizeDisplay)
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
  
  showSource = callModule(showSourceServer, 'showSource')
  observeEvent(input$showSource, {
    showSource(filteredData())
  })
  
  observeEvent(input$calcCPA, {
    callModule(showCpa, "", data=filteredData)
  })
  
  observeEvent(input$minimizeHeader, {
    isHeaderMinimized(!isHeaderMinimized())
  })
  
  observe({
    shinyjs::toggle("loadDataHeader", condition = !isHeaderMinimized())
    updateActionButton(session, "minimizeHeader",
                       label = ifelse(isHeaderMinimized(),
                                      "Show",
                                      "Minimize")
    )
  })
  
  
  headerMinimalInformation = reactive({
    print("header minimal")
    parts = c()
    
    if(isDataLoaded())
      parts = append(parts, eventsInformation()$name)
    
    if(isMetaDataLoaded()) {
      parts = append(parts, metaDataFile()$name)
      parts = append(parts, filteredMetaData()$query)
    }
    
    print("end header minimal")
    return(toString(parts))
  })
  
  output$headerInformation = renderText({
    if(isHeaderMinimized()) headerMinimalInformation()
  })
  
  output$showSource = renderUI({
    if(input$plotType == "timePlot" & isDataLoaded())
      actionButton(inputId = ns("showSource"), label = "Show Source")
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
    req(input$plotType)
    
    if(input$plotType == "timePlot")
      return(timePlot())
    if(input$plotType == "hist")
      return(hist())
  })
  
  output$sidePanel = renderUI({
    req(isDataLoaded())
    
    wellPanel(
      selectInput(ns("plotType"), "Plot Type", 
                  c("Time Plot" = "timePlot", "Histogram" = "hist"),
                  selected = "timePlot"),
      customizeDisplayUI(ns("customizeDisplay")),
      uiOutput(ns("showSource")), 
      uiOutput(ns("calcCPA"), label = "Show CPA"),
      uiOutput(ns("showEventStats"), label = "Basic Statistics"),
      uiOutput(ns("downloadDataOutput"))
    )
  })
  
  output$eventStats = renderPrint({
    req(isDataLoaded())
    
    if(input$plotType == "timePlot")
      return(eventStats())
    if(input$plotType == "hist")
      return(eventStats())
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
  
  output$calcCPA = renderUI({
    if(input$plotType == "timePlot" & isDataLoaded())
      actionButton(inputId = ns("calcCPA"), label = "Show CPA")
  })
  
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
