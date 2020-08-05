
cpaOverlayTabServer = function(input, output, session, data, 
                               isDataLoaded, 
                               cpa, filteredPlotOpts){
  ns = session$ns
  
  #----Filter Data----
  dataFilter = callModule(dataFilterServer, "dataFilter", data)
  
  output$dataFilter = renderUI({
    if(isDataLoaded()) dataFilterUI(ns("dataFilter"))
  })
  
  filteredData = reactive({
    req(isDataLoaded())
    dataFilter$filteredData()
  })
  
  #----Plot----
  timePlot <- reactive({
    req(isDataLoaded())
    req(dataFilter$hasValidQuery() | !dataFilter$hasQueryInput())
    
    fd = filteredData()
    p = generateTimePlot(fd, customizeDisplay)
    
    if(cpa$plotCpa())
      p = addCpaMarkersToPlot(p, cpa$cpaMarkers(),
                              fd, customizeDisplay$yColumn)
    
    p
  })
  
  plotHeight = reactive({
    if(is.null(customizeDisplay$plotHeight)) 400
    else customizeDisplay$plotHeight
  })
  
  output$eventPlotContainer = renderUI({
    fluidPage(
      plotOutput(ns("eventPlot"), 
                 height = plotHeight()
      ),
      uiOutput(ns("facetPageControl"))
    )
  })
  
  output$eventPlot = renderPlot({
    req(isDataLoaded())
    timePlot()
  })
  
  
  #----Side Panel ----
  output$sidePanel = renderUI({
    req(isDataLoaded())
    
    tabsetPanel(
      tabPanel("Display", 
               customizeDisplayUI(ns("customizeDisplay")))
    )
  })
  
  
  #----Axis Settings----
  customizeDisplay = callModule(customizeDisplayServer, "customizeDisplay", 
                                filteredData)
  
  ##----Axis Settings: Facet----
  doFacet = reactive({
    cd = customizeDisplay
    isDataLoaded() & (cd$facetRowsPerPage != cd$no_selection)
  })
  
  facetPageN <- reactive({
    if(!doFacet()) -1
    else n_pages(timePlot())
  })
  
  output$facetPageControl = renderUI({
    if(doFacet())
      facetPageUI(ns("facetPageControl"))
  })
  
  facetPageControl = callModule(facetPageServer, "facetPageControl",
                                facetPageN)
  
  observeEvent(facetPageControl$page, {
    pg = facetPageControl$page
    if(!is.null(pg))
      customizeDisplay$facetPage = facetPageControl$page
  })
}
