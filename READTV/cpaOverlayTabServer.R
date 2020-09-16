
cpaOverlayTabServer = function(input, output, session, data, 
                               isDataLoaded, 
                               cpa, filteredPlotOpts,
                               previousSourceString,
                               input_sym = sym("data"),
                               cpa_markers_sym = sym("cpa_markers"),
                               select_output_sym = sym("selected_data2"),
                               output_sym = sym("filtered_data2")){
  ns = session$ns
  
  #----Code Gen Symbols and Pronouns----
  f = stringr::str_interp
  plot_in_sym = sym("plot_df")
  et = expr_text
  
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
    
    mask = list()
    mask[[et(input_sym)]] = data()
    mask[[et(cpa_markers_sym)]] = cpa$cpaMarkers()
    
    mask = runExpressions(currentTabWithPlotCode(), mask)
    
    mask$p
  })
  
  
  currentTabWithPlotCode = reactive({
    req(isDataLoaded())
    
    append(currentTabCode(), plotCodes())
  })

  
  plotCodes = reactive({
    req(data())
    
    codes = list()
    
    plot_opts = customizeDisplay
    
    mask = list()
    mask[[et(output_sym)]] = filteredData()
    codes[[et(plot_in_sym)]] = generatePreparePlotCode(
      mask[[et(output_sym)]], 
      customizeDisplay,
      df_in_pronoun = output_sym,
      df_out_sym = plot_in_sym)
    
    plot_df = eval_tidy(codes[[et(plot_in_sym)]], data = mask)
    
    base_p_pronoun = sym("base_p")
    base_plot_code = generateTimePlotCode(plot_df, plot_opts,
                                          plot_data_pronoun = plot_in_sym,
                                          out_p_pronoun = base_p_pronoun)
    codes[[et(base_p_pronoun)]] = base_plot_code
    
    y_col = plot_opts$y
    if(is_empty_str(y_col)) y_col = NULL
    add_markers_code = expr(
      p <- addCpaMarkersToPlot(!!base_p_pronoun, !!cpa_markers_sym,
                          !!output_sym, y_column = !!(y_col)))
    
    if(showMarkers())
      codes$p = add_markers_code
    else
      codes$p = expr(p <- !!base_p_pronoun)
    
    codes
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
    isDataLoaded() & (is_str_set(cd$facetRowsPerPage))
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
  annotateSourceString = function(tab_code)
    expressionsToString(previousSourceString(),
                        "",
                        "#CPA Overlay",
                        tab_code)
  
  output$sourceCodeSubTab = renderUI({
    actionButton(ns("showSourceBtn"), "Show Source")
  })
  
  observeEvent(input$showSourceBtn, {
    showModal(modalDialog(
      title = "Source Code",
      size = "l",
      verbatimTextOutput(ns("fullSourceWithPlot"))
    )
    )
  })
  
  fullSourceString = reactive({
    req(isDataLoaded())
    
    annotateSourceString(currentTabCode())
  })
  
  currentTabCode = reactive({
    req(isDataLoaded())
    
    codes = list()
    codes[[et(select_output_sym)]] = dataFilter$selectedQuery()
    codes[[et(output_sym)]] = dataFilter$filteredQuery()

    codes
  })
  
  fullSourceWithPlotString = reactive({
    req(isDataLoaded())
    
    plot_codes = plotCodes()
    expressionsToString(
      fullSourceString(),
      "",
      plotCodes(),
      "",
      "plot(p)"
    )
  })
  
  currentTabWithPlotCode = reactive({
    req(isDataLoaded())
    
    append(currentTabCode(), plotCodes())
  })
  
  output$fullSourceWithPlot = renderText({
    fullSourceWithPlotString()
  })
}
