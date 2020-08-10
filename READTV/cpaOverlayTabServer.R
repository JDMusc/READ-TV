
cpaOverlayTabServer = function(input, output, session, data, 
                               isDataLoaded, 
                               cpa, filteredPlotOpts){
  ns = session$ns
  f = stringr::str_interp
  
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
    
    if(showMarkers())
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
    marker_message_id = ns("cpaMarkerMessage")
    tabsetPanel(
      tabPanel("Display", 
               customizeDisplayUI(ns("customizeDisplay"))),
      tabPanel("CPA Markers",
               div(
                 tags$style(
                   type="text/css", 
                   f("#${marker_message_id} 
                     {white-space: pre-wrap;}")),
                 uiOutput(ns("cpaMarkerDisplay")),
                 uiOutput(marker_message_id)
                 )
               )
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
  
  #----CPA Markers Overlay----
  output$cpaMarkerDisplay = renderUI({
    req(cpa$doPlotCpa())
    
    selectInput(ns("cpaMarkerDisplayInput"), "Display Markers",
                choices = c("Yes", "No"))
  })
  
  showMarkers = reactive({
    if(is.null(input$cpaMarkerDisplayInput)) cpa$doPlotCpa()
    else input$cpaMarkerDisplayInput == "Yes"
  })
  
  output$cpaMarkerMessage = renderText({
    if(!cpa$doPlotCpa())
      "CPA input changed or CPA output not yet calculated.
      (Re)calculate CPA to see results."
    else
      NULL
  })
}
