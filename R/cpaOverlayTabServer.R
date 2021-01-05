
cpaOverlayTabServer = function(input, output, session, uploadCode, uploadMask,
                               isDataLoaded,
                               cpa, filteredPlotOpts,
                               previousSourceString,
                               input_sym = sym("data"),
                               cpa_markers_sym = sym("cpa_markers"),
                               select_output_sym = sym("selected_data2"),
                               output_sym = sym("filtered_data2")){

  #----Code Gen Symbols and Pronouns----
  ns = session$ns

  f = stringr::str_interp
  plot_in_sym = sym("plot_df")
  et = expr_text

  location = function(msg) f('cpaOverlayTabServer ${msg}')

  log_trace_bdt = log_info_module_gen('cpaOverlayTabServer')
  req_log = req_log_gen(log_trace_bdt)


  #----Filter Data----
  data = reactive({
    req_log('data', quo(isDataLoaded()))

    runExpressionsLast(uploadCode(), uploadMask(), location = location('data'))
  })

  dataFilter = callModule(dataFilterServer, "dataFilter", data,
                          input_sym, select_output_sym, output_sym,
                          filter_out_init = FALSE)

  output$dataFilter = renderUI({
    if(isDataLoaded()) dataFilterUI(ns("dataFilter"))
  })

  filteredData = reactive({
    req_log('filteredData', isDataLoaded())

    data() %>%
      list %>%
      set_expr_names(c(input_sym)) %>%
      runExpressions(currentTabCode(), ., location = location('filteredData')) %>%
      extract2(rtv.et(output_sym))
  })

  #----Plot----
  no_cpa_msg = "CPA input changed or CPA output not yet calculated.
      (Re)calculate CPA to see results."

  timePlot <- reactive({
    req_log('timePlot', isDataLoaded())
    req_log('timePlot', dataFilter$hasValidQuery() | !dataFilter$hasQueryInput())

    mask = list(data(), cpa$cpaMarkers()) %>%
      set_expr_names(c(input_sym, cpa_markers_sym))

    mask = runExpressions(currentTabWithPlotCode(), mask, location = location('timePlot'))

    mask$p
  })


  currentTabWithPlotCode = reactive({
    req_log('currentTabWithPlotCode', isDataLoaded())

    append(currentTabCode(), plotCodes())
  })


  plotCodes = reactive({
    req_log('plotCodes', data())

    codes = list()

    plot_opts = reactiveValuesToList(customizeDisplay)
    if(!dataFilter$filterOut())
      plot_opts$alpha = constants.alpha_col

    mask = filteredData() %>% list %>% set_expr_names(c(output_sym))
    codes[[rtv.et(plot_in_sym)]] = generatePreparePlotCode(
      mask[[rtv.et(output_sym)]],
      plot_opts,
      df_in_pronoun = output_sym,
      df_out_sym = plot_in_sym)

    plot_df = eval_tidy(codes[[rtv.et(plot_in_sym)]], data = mask)

    base_p_pronoun = sym("base_p")
    base_plot_code = generateTimePlotCode(plot_df, plot_opts,
                                          plot_data_pronoun = plot_in_sym,
                                          out_p_pronoun = base_p_pronoun)
    codes[[rtv.et(base_p_pronoun)]] = base_plot_code

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
    req_log('eventPlot', isDataLoaded())

    timePlot()
  })


  #----Side Panel ----
  output$sidePanel = renderUI({
    req_log('sidePanel', isDataLoaded())

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
    isDataLoaded() & is_str_set(customizeDisplay$facetRowsPerPage)
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
      customizeDisplay$facetPage = pg
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

    dataFilter$filteredDataExprs()
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
