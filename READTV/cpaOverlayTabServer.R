
cpaOverlayTabServer = function(input, output, session, data, 
                               isDataLoaded, 
                               cpa, filteredPlotOpts,
                               previousSourceString,
                               input_sym = sym("filtered_data"),
                               select_output_sym = sym("selected_data2"),
                               output_sym = sym("filtered_data2")){
  ns = session$ns
  f = stringr::str_interp
  
  prepare_in = as.character(output_sym)
  plot_in = sym("plot_df")
  p_pronoun = sym("p")
  cpa_markers_pronoun = sym("cpa_markers")
  
  #----Filter Data----
  dataFilter = callModule(dataFilterServer, "dataFilter", data,
                          input_sym, select_output_sym, output_sym)
  
  output$dataFilter = renderUI({
    if(isDataLoaded()) dataFilterUI(ns("dataFilter"))
  })
  
  filteredData = reactive({
    req(isDataLoaded())
    dataFilter$filteredData()
  })
  
  #----Plot----
  no_cpa_msg = "CPA input changed or CPA output not yet calculated.
      (Re)calculate CPA to see results."
  timePlot <- reactive({
    req(isDataLoaded())
    req(dataFilter$hasValidQuery() | !dataFilter$hasQueryInput())
    
    #fd = filteredData()
    #p = generateTimePlot(fd, customizeDisplay)
    plot_codes = plotCodes()
    
    p = eval_tidy(plot_codes$base_plot, env = env(plot_df = plotInput()))
    
    if(showMarkers())
      p = eval_tidy(plot_codes$add_markers, 
                    data = list(cpa_markers = cpa$cpaMarkers(), p = p))
    
    p
  })
    
  
  plotCode = reactive({
    plot_df = plotInput()
    generateTimePlotCode(plot_df, customizeDisplay)
  })
  
  
  plotCodes = reactive({
    req(data())
    
    codes = list()
    
    filtered_data2 <- filteredData()
    
    plot_opts = customizeDisplay
    
    prepare_plot_input = plotInputCode()
    codes$prepare_plot_input = prepare_plot_input
    
    plot_df = plotInput()
    
    base_plot_code = generateTimePlotCode(plot_df, plot_opts)
    
    codes$base_plot = base_plot_code
    
    y_col = plot_opts$yColumn
    if(y_col == filteredPlotOpts$no_selection) y_col = NULL
    add_markers_code = expr(
      !!p_pronoun <- addCpaMarkersToPlot(!!p_pronoun, !!cpa_markers_pronoun,
                          !!output_sym, y_column = !!(y_col)))
    
    if(showMarkers())
      codes$add_markers = add_markers_code
    
    codes
  })
  
  
  makeDataMask = function(filtered_data) {
    mask = list()
    mask[[prepare_in]] = filtered_data
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
    
    filtered_data2 = filteredData()
    generatePreparePlotCode(quo(filtered_data2), 
                            customizeDisplay,
                            df_out_sym = plot_in)
  })
  
  plotHeight = reactive({
    if(is.null(customizeDisplay$plotHeight)) 400
    else customizeDisplay$plotHeight
  })
  
  marker_message_id = ns("cpaMarkerMessage")
  output$eventPlotContainer = renderUI({
    fluidPage(
      plotOutput(ns("eventPlot"), height = plotHeight()),
      #uiOutput(marker_message_id),
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
               ),
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
      no_cpa_msg
    else
      NULL
  })
  
  
  #---Source Code----
  output$sourceCodeSubTab = renderUI({
    actionButton(ns("showSourceBtn"), "Show Source")
  })
  
  observeEvent(input$showSourceBtn, {
    showModal(modalDialog(
      title = "Source Code",
      size = "l",
      verbatimTextOutput(ns("myPlotSource")),
    )
    )
  })
  
  mySourceString = reactive({
    req(isDataLoaded())
    
    selected_code = dataFilter$selectedQuery()
    filtered_code = dataFilter$filteredQuery()
    
    expressionsToString(
      previousSourceString(),
      "",
      selected_code,
      filtered_code
    )
  })
  
  myPlotSourceString = reactive({
    req(isDataLoaded())
    
    plot_input_code = plotInputCode()
    plot_codes = plotCodes()
    expressionsToString(
      mySourceString(),
      "",
      plot_input_code,
      plot_codes$base_plot,
      plot_codes$add_markers,
      "plot(p)"
    )
  })
  
  output$myPlotSource = renderText({
    myPlotSourceString()
  })
}
