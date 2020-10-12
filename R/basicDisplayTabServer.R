
basicDisplayTabServer = function(input, output, session, data,
                              fileName, isDataLoaded,
                              isFilePassed,
                              previousSourceString,
                              initPlotOpts = list(),
                              input_sym = sym('data'),
                              select_output_sym = sym('selected_data'),
                              output_sym = sym('filtered_data')){
  ns = session$ns
  f = stringr::str_interp
  et = expr_text

  #----Filter Data----
  dataFilter = callModule(dataFilterServer, "dataFilter", data,
                          input_sym, select_output_sym, output_sym,
                          filter_out_init = TRUE)

  output$dataFilter = renderUI({
    if(isDataLoaded()) dataFilterUI(ns("dataFilter"))
  })

  filteredData = reactive({
    req(isDataLoaded())

    data() %>%
      list %>%
      set_expr_names(c(input_sym)) %>%
      runExpressions(currentTabCode(), .) %>%
      extract2(rtv.et(output_sym))
  })


  #----Plot----
  updateTimePlotCountDebug = printWithCountGen('time plot')

  timePlot <- reactive({
    req(isDataLoaded())

    mask = data() %>% list %>% set_names(rtv.et(input_sym))
    mask = runExpressions(currentTabWithPlotCode(), mask)
    mask$p
  })

  plotCode = reactive({
    plot_data = plotInput()

    generateTimePlotCode(plot_data, plotOpts())
  })

  plot_in = as.character(output_sym)
  plot_out = 'plot_data'

  makeDataMask = function(filtered_data) {
    list(filtered_data) %>%
      set_names(nm = plot_in)
  }

  plotOpts = reactive({
    cd = customizeDisplay

    if(dataFilter$filterOut())
      cd$alpha = NULL
    else
      cd$alpha = constants.alpha_col

    cd
  })

  plotInput = reactive({
    req(isDataLoaded())
    req(dataFilter$hasValidQuery() | !dataFilter$hasQueryInput())

    code = plotInputCode()
    eval_tidy(code, data = makeDataMask(filteredData()))
  })

  plotInputCode = reactive({
    req(isDataLoaded())

    currentTabCode() %>%
      runExpressions(list(data = data())) %>%
      extract2(rtv.et(output_sym)) %>%
      generatePreparePlotCode(plotOpts(), df_in_pronoun = output_sym)
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
      tabPanel("Download Data", dataDownloadUI(ns("dataDownload"))),
      tabPanel("Source Code", uiOutput(ns("sourceCodeSubTab")))
    )
  })

  #----Axis Settings----
  customizeDisplay = callModule(customizeDisplayServer, "customizeDisplay",
                                filteredData, initPlotOpts)

  ##----Axis Settings: Facet----
  doFacet = reactive({
    cd = customizeDisplay
    isDataLoaded() & is_str_set(cd$facetRowsPerPage)
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
                                facetPageN,
                                initPlotOpts$facetPage)

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
  downloadControl = callModule(dataDownloadServer, "dataDownload",
                               filteredData, fileName,
                               isDataLoaded, isFilePassed,
                               'filtered-data')


  #----Source Code----
  annotateSourceString = function(tab_code)
    expressionsToString(previousSourceString(),
                        "",
                        "#Basic Filter",
                        tab_code)

  fullSourceString = reactive({
    req(isDataLoaded())

    annotateSourceString(currentTabCode())
  })

  currentTabCode = reactive({
    req(isDataLoaded())

    dataFilter$filteredDataExprs()
  })

  currentTabWithPlotCode = reactive({
    req(isDataLoaded())

    append(currentTabCode(),
           exprs(
             plot_data = !!plotInputCode(),
             p = !!plotCode())
    )
  })

  fullSourceWithPlotString = reactive({
    req(isDataLoaded())

    expressionsToString(
      annotateSourceString(
        currentTabWithPlotCode()
        ),
      "",
      "plot(p)")
  })

  output$sourceCodeSubTab = renderUI({
    actionButton(ns("showSourceBtn"), "Show Source")
  })


  observeEvent(input$showSourceBtn, {
    showModal(
      modalDialog(title = "Source Code",
                  size = "l",
                  verbatimTextOutput(ns("fullSourceWithPlot"))
                  )
      )
  })

  output$fullSourceWithPlot = renderText({
    fullSourceWithPlotString()
  })


  #----Return----
  list(
    customizeDisplay = customizeDisplay,
    dataFilter = dataFilter,
    filteredData = filteredData,
    facetPageN = facetPageN,
    fullSourceString = fullSourceString
  )
}
