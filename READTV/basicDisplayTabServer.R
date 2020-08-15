
basicDisplayTabServer = function(input, output, session, data, 
                              headerMinimalInformation, isDataLoaded){
  ns = session$ns
  f = stringr::str_interp
  
  #----Filter Data----
  filter_in = "data"
  selected_out = 'selected_data'
  filter_out = "filtered_data"
  dataFilter = callModule(dataFilterServer, "dataFilter", data,
                          filter_in, selected_out, filter_out)
  
  output$dataFilter = renderUI({
    if(isDataLoaded()) dataFilterUI(ns("dataFilter"))
  })
  
  filteredData = reactive({
    req(isDataLoaded())
    dataFilter$filteredData()
  })
  
  
  #----Plot----
  hist <- reactive({
    req(isDataLoaded())
    
    ggplot(filteredData(), aes(x = deltaTime)) +
      geom_histogram(fill = "black") + 
      xlab("Time Between Events") +
      ylab("Event Count")
  })
  
  updateTimePlotCountDebug = printWithCountGen('time plot')

  timePlot <- reactive({
    req(isDataLoaded())
    req(dataFilter$hasValidQuery() | !dataFilter$hasQueryInput())
    
    eval_tidy(plotCode(), env = env(plot_data = plotInput()))
  })
  
  plotCode = reactive({
    plot_data = plotInput()
    generateTimePlotCode(plot_data, customizeDisplay)
  })
  
  plot_in = 'filtered_data'
  plot_out = 'plot_data'
  
  makeDataMask = function(filtered_data) {
    mask = list()
    mask[[plot_in]] = filtered_data
    mask
  }
  plotInput = reactive({
    req(isDataLoaded())
    req(dataFilter$hasValidQuery() | !dataFilter$hasQueryInput())
    
    code = plotInputCode()
    eval_tidy(code, data = makeDataMask(filteredData()))
  })
  
  plotInputCode = reactive({
    req(isDataLoaded())
    
    filtered_data = filteredData()
    generatePreparePlotCode(quo(filtered_data), 
                            customizeDisplay,
                            sym(plot_in),
                            sym(plot_out)
                            )
  })
  
  
  plotHeight = reactive({
    if(is.null(customizeDisplay$plotHeight)) 400
    else customizeDisplay$plotHeight
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
      uiOutput(ns("facetPageControl"))
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
  
  
  #----Side Panel ----
  output$sidePanel = renderUI({
    req(isDataLoaded())
    
    tabsetPanel(
      tabPanel("Display", 
               customizeDisplayUI(ns("customizeDisplay"))),
      tabPanel("Event Statistics", 
               uiOutput(ns("showEventStats"), label = "Basic Statistics")),
      tabPanel("Download Data", uiOutput(ns("downloadDataOutput"))),
      tabPanel("Source Code", uiOutput(ns("sourceCodeSubTab")))
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
  
  
  #----Source Code----
  mySource = reactive({
    req(isDataLoaded())
    selected_code = dataFilter$selectedQuery()
    filtered_code = dataFilter$filteredQuery()
    plot_input_code = plotInputCode()
    plot_code = plotCode()
    f_name = headerMinimalInformation()
    paste(
      f("f_name = \"${f_name}\" #specify local path"),
      f("${filter_in} = loadEventsWithRelativeAndDeltaTime(f_name)"),
      expr_text(selected_code, width = 50),
      expr_text(filtered_code, width = 50),
      expr_text(plot_input_code, width = 50),
      expr_text(plot_code, width = 50), sep = '\n')
  })
  
  output$sourceCodeSubTab = renderUI({
    actionButton(ns("showSourceBtn"), "Show Source")
  })
  
  
  observeEvent(input$showSourceBtn, {
    showModal(modalDialog(
      title = "Source Code",
      size = "l",
      verbatimTextOutput(ns("mySource")),
    )
    )
  })
  
  output$mySource = renderText({
    mySource()
  })
  
  
  #----Return----
  return(list(
    customizeDisplay = customizeDisplay,
    dataFilter = dataFilter,
    filteredData = filteredData,
    facetPageN = facetPageN
  ))
}
