
basicDisplayTabServer = function(input, output, session,
                                 prevCode,
                                 prevMask,
                                fileName, isDataLoaded,
                                isFilePassed,
                                initPlotOpts = list(),
                                input_sym = sym('data'),
                                select_output_sym = sym('selected_data'),
                                output_sym = sym('filtered_data')){
  ns = session$ns
  f = stringr::str_interp
  et = expr_text

  location = function(msg) f('basicDisplayTabServer ${msg}')

  log_info_bdt = log_info_module_gen('basicDisplayTabServer')
  req_log = req_log_gen(log_info_bdt)

  #----Filter Data----
  data = reactive({
    req_log('data', quo(isDataLoaded()))

    runExpressionsLast(prevCode(), prevMask(), location = location('data'))
  })

  dataFilter = callModule(dataFilterServer, "dataFilter", data,
                          input_sym, select_output_sym, output_sym,
                          filter_out_init = TRUE)

  output$dataFilter = renderUI({
    if(isDataLoaded()) dataFilterUI(ns("dataFilter"))
  })

  filteredData = reactive({
    req_log('filteredData', quo(isDataLoaded()))

    data() %>%
      list %>%
      set_expr_names(c(input_sym)) %>%
      runExpressions(currentTabCode(), ., location=location('filteredData')) %>%
      extract2(rtv.et(output_sym))
  })


  #----Plot----
  updateTimePlotCountDebug = printWithCountGen('time plot')

  timePlot <- reactive({
    req_log('timePlot', quo(isDataLoaded()))

    mask = data() %>%
      list %>%
      set_names(rtv.et(input_sym)) %>%
      {runExpressions(currentTabWithPlotCode(), ., location=location('timePlot'))}

    mask$p
  })

  plotCode = reactive({
    log_info_bdt('plotCode')
    plot_data = plotInput()

    generateTimePlotCode(plot_data, plotOpts())
  })

  plot_in = as.character(output_sym)
  plot_out = 'plot_data'

  makeDataMask = function(filtered_data) {
    log_info_bdt('makeDataMask')
    list(filtered_data) %>%
      set_names(nm = plot_in)
  }

  plotOpts = reactive({
    log_info_bdt('plotOpts')
    cd = customizeDisplay

    if(dataFilter$filterOut())
      cd$alpha = NULL
    else
      cd$alpha = constants.alpha_col

    cd
  })

  plotInput = reactive({
    fn_name = 'plotInput'

    req_log(fn_name, quo(isDataLoaded()))
    req_log(fn_name,
            quo(dataFilter$hasValidQuery() | !dataFilter$hasQueryInput()))

    code = plotInputCode()
    eval_tidy_verbose(code, data = makeDataMask(filteredData()),
                      location=location(fn_name))
  })

  plotInputCode = reactive({
    req_log('plotInputCode', quo(isDataLoaded()))

    data() %>%
      list %>%
      set_expr_names(c(input_sym)) %>%
      runExpressions(currentTabCode(), mask = ., location = location('plotInputCode')) %>%
      extract2(rtv.et(output_sym)) %>%
      generatePreparePlotCode(plotOpts(), df_in_pronoun = output_sym)
  })


  plotHeight = reactive({
    log_info_bdt('plotHeight')
    if(is.null(customizeDisplay$plotHeight)) 400
    else customizeDisplay$plotHeight
  })

  output$eventPlotContainer = renderUI({
    log_info_bdt('output eventplotContainer')
    fluidPage(
      plotOutput(ns("eventPlot"),
                 height = plotHeight(),
                 brush = brushOpts(ns("event_plot_brush"),
                                   direction = "x",
                                   resetOnNew = TRUE),
                 dblclick = clickOpts(ns("event_plot_dblclick"))
                 ),
      uiOutput(ns("facetPageControl"))
    )
  })


  output$eventPlot = renderPlot({
    req_log('output eventplot', isDataLoaded())
    timePlot()
  })


  #----Side Panel ----
  output$sidePanel = renderUI({
    req_log('output sidepanel', quo(isDataLoaded()))

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
    log_info_bdt('do facet')
    cd = customizeDisplay
    isDataLoaded() & is_str_set(cd$facetRowsPerPage)
  })

  facetPageN <- reactive({
    log_info_bdt('facet page n')
    if(!doFacet()) -1
    else n_pages(timePlot())
  })

  output$facetPageControl = renderUI({
    log_info_bdt('output facetpagecontrol')
    if(doFacet())
      facetPageUI(ns("facetPageControl"))
  })

  facetPageControl = callModule(facetPageServer, "facetPageControl",
                                facetPageN,
                                initPlotOpts$facetPage)

  observeEvent(facetPageControl$page, {
    log_info_bdt('observer facetPageControl$page')
    pg = facetPageControl$page
    if(!is.null(pg))
      customizeDisplay$facetPage = facetPageControl$page
  })


  #----Event Stats----
  output$showEventStats = renderUI({
    log_info_bdt('output showeventstats')
    if(isDataLoaded())
      actionButton(inputId = ns("showEventStats"), "Basic Statistics")
  })

  observeEvent(input$showEventStats, {
    log_info_bdt('observe input$showEventStats')
    callModule(showEventStats, "", data=filteredData)
  })


  #----Download Data----
  downloadControl = callModule(dataDownloadServer, "dataDownload",
                               filteredData, fileName,
                               isDataLoaded, isFilePassed,
                               'filtered-data')


  #----Source Code----
  annotateSourceString = function(tab_code)
    expressionsToString(prevCode(),
                        "",
                        "#Basic Filter",
                        tab_code)

  fullSourceString = reactive({
    req_log('full source string', quo(isDataLoaded()))

    annotateSourceString(currentTabCode())
  })

  currentTabCode = reactive({
    req_log('current tab code',
            quo(isDataLoaded()))

    dataFilter$filteredDataExprs()
  })

  currentTabWithPlotCode = reactive({
    req_log('currentTabWithPlotCode',
            quo(isDataLoaded()))

    append(currentTabCode(),
           exprs(
             plot_data = !!plotInputCode(),
             p = !!plotCode())
    )
  })

  fullSourceWithPlotString = reactive({
    req_log('fullSourceWithPlotString',
            quo(isDataLoaded()))

    expressionsToString(
      annotateSourceString(
        currentTabWithPlotCode()
        ),
      "",
      "plot(p)")
  })

  output$sourceCodeSubTab = renderUI({
    log_info_bdt('output scst')
    actionButton(ns("showSourceBtn"), "Show Source")
  })


  observeEvent(input$showSourceBtn, {
    log_info_bdt('input ssb')
    showModal(
      modalDialog(title = "Source Code",
                  size = "l",
                  verbatimTextOutput(ns("fullSourceWithPlot"))
                  )
      )
  })

  output$fullSourceWithPlot = renderText({
    log_info_bdt('output fswp')
    fullSourceWithPlotString()
  })


  #----Return----
  list(
    customizeDisplay = customizeDisplay,
    dataFilter = reactive({'blah'}),#dataFilter,
    filteredData = filteredData,
    facetPageN = facetPageN,
    fullSourceString = fullSourceString
  )
}
