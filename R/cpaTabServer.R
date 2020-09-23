
cpaTabServer = function(input, output, session, previousData,
                        isDataLoaded, previousPlotOpts,
                        facetPageN, previousSourceString,
                        input_sym = sym("filtered_data"),
                        cpa_markers_sym = sym("cpa_markers")){

  ns = session$ns

  #----Code Gen Symbols and Pronouns----
  et = expr_text
  base_plot_df_pronoun = sym("base_plot_df")
  cpa_markers_sym = sym("cpa_markers")
  cpa_input_df_pronoun = sym("cpa_input_df")


  #----Plot----
  timePlot <- reactive({
    req(previousData())

    mask = list()
    mask[[et(input_sym)]] = previousData()

    mask = runExpressions(currentTabWithPlotCode(), mask)

    mask$p
  })


  currentTabWithPlotCode = reactive({
    req(isDataLoaded())

    append(currentTabCode(), plotCodes())
  })

  plotCodes = reactive({
    req(previousData())

    codes = list()

    cpa_input_df = cpaInputData()
    show_original = showOriginal()
    show_original_and_event_frequency = showOriginalAndEventFrequency()

    plot_opts = plotOptions
    cpa_plot_opts = smoothedPlotOptions

    prepare_plot_input = generatePreparePlotCode(
      previousData(),
      plot_opts,
      df_in_pronoun = input_sym,
      df_out_sym = base_plot_df_pronoun)

    codes[[et(base_plot_df_pronoun)]] = prepare_plot_input

    mask = list()
    mask[[et(input_sym)]] = previousData()
    plot_df = eval_tidy(prepare_plot_input, data = mask)

    base_p_pronoun = sym("base_p")
    base_plot_code = cpaTabLogic.basePlotCode(
      cpa_plot_df = cpa_input_df,
      cpa_plot_opts = cpa_plot_opts,
      cpa_plot_df_pronoun = cpa_input_df_pronoun,
      base_plot_df = plot_df,
      plot_opts = plot_opts,
      base_plot_df_pronoun = base_plot_df_pronoun,
      show_original = show_original,
      out_p_pronoun = base_p_pronoun
    )
    codes[[et(base_p_pronoun)]] = base_plot_code


    add_ef_p_pronoun = sym("event_freq_p")
    add_event_freq_code = cpaTabLogic.addEventFrequencyCode(
      plot_opts,
      base_p_pronoun,
      cpa_plot_df_pronoun = cpa_input_df_pronoun,
      cpa_plot_opts = cpa_plot_opts,
      show_original_and_event_frequency = show_original_and_event_frequency,
      smooth_fn_name = preprocess$agg_fn_label,
      out_p_pronoun = add_ef_p_pronoun)
    codes[[et(add_ef_p_pronoun)]] = add_event_freq_code

    add_markers_code = cpaTabLogic.addCpaMarkersCode(
      p_pronoun = add_ef_p_pronoun,
      cpa_plot_df = cpa_input_df,
      cpa_plot_opts = cpa_plot_opts,
      cpa_plot_df_pronoun = cpa_input_df_pronoun,
      base_plot_df = plot_df,
      plot_opts = plot_opts,
      base_plot_df_pronoun = base_plot_df_pronoun,
      cpa_markers_pronoun = cpa_markers_sym,
      add_markers = doPlotCpa(),
      show_original = show_original,
      show_original_and_event_frequency = show_original_and_event_frequency
    )

    codes$p = add_markers_code

    codes
  })


  output$eventPlot = renderPlot({
    req(isDataLoaded())
    timePlot()
  })


  output$eventPlotContainer = renderUI({
    print("rerender event plot container")
    fluidPage(
      plotOutput(ns("eventPlot"),
                 height = plotOptions$plotHeight
      ),
      uiOutput(ns("facetPageControl"))
    )
  })

  showOriginalAndEventFrequency = reactive({
    is.list(yDisplay())
  })

  showSmoothed = reactive({
    (
      ('CpaInput' %in% yDisplay()) |
        showOriginalAndEventFrequency()
    ) & doRegularize()
  })

  showOriginal = reactive({
    (previousPlotOpts$y %in% yDisplay()) |
      showOriginalAndEventFrequency()
  })

  copyPlotOpts = function(plot_opts) {
    generatePlotDefaults(plot_opts)
  }

  #reactiveValues so that dependencies update on a given field instead of any field
  plotOptions = reactiveValues()
  observe({
    req(previousPlotOpts)
    plot_opts = copyPlotOpts(previousPlotOpts)
    if(doFacet())
      plot_opts$facetPage = facetPageControl$page

    updateReactiveVals(plot_opts, plotOptions)
  })

  smoothedPlotOptions = reactiveValues()
  observe({
    req(plotOptions)

    plot_opts = copyPlotOpts(plotOptions)

    y_col = yDisplay()
    if(is.list(y_col))
      y_col = y_col[[regularSpacedData]]

    plot_opts$y = y_col
    plot_opts$shape = NULL
    plot_opts$color = NULL

    updateReactiveVals(plot_opts, smoothedPlotOptions)
  })


  updateReactiveVals = function(src_opts, rvs)
    for(nm in names(src_opts)) {
      src_val = src_opts[[nm]]
      if(!identical(src_val, rvs[[nm]]))
        rvs[[nm]] = src_val
    }


  #----Side Panel ----
  output$sidePanel = renderUI({
    req(isDataLoaded())

    tabsetPanel(
      tabPanel("Calculate CPA",
               div(
                 cpaPreprocessUI(ns("preprocess")),
                 cpaParamsUI(ns("cpaParams"))
                 )
               ),
      tabPanel("Display", uiOutput(ns("display"))),
      tabPanel("Source Code",
               uiOutput(ns("sourceCodeSubTab")))
    )
  })


  #----Facet----
  doFacet = reactive({
    ppo = previousPlotOpts
    isDataLoaded() & (is_str_set(ppo$facetRowsPerPage))
  })

  output$facetPageControl = renderUI({
    if(doFacet())
      facetPageUI(ns("facetPageControl"))
  })

  facetPageControl = callModule(facetPageServer, "facetPageControl",
                                facetPageN)


  #----Preprocess----
  preprocess = callModule(cpaPreprocessServer, "preprocess", previousData,
                          previousPlotOpts)
  doRegularize = reactive({
    getElementSafe('do_regularize', preprocess, F)
  })

  #---Display----
  regularSpacedData = "Regularly Spaced Data"
  output$display = renderUI({

    yColumns = displayEmptyStrAsAnyEvent(
      c(previousPlotOpts$y, regularSpacedData, "Both"),
      anyEvent = previousPlotOpts$anyEvent)

    fluidRow(
      if(doRegularize())
        column(selectInput(ns("y_column"), "Y-axis",
                         choices = yColumns,
                         selected = "Both"), width = 2)
      else NULL
    )
  })

  yColMapping = reactive({
    mapping = list('CpaInput') %>% set_names(regularSpacedData)

    prev_y = previousPlotOpts$y
    mapping[[prev_y]] = prev_y

    mapping$Both = mapping

    mapping
  })

  yDisplay = reactive({
    mapping = yColMapping()

    ds = doRegularize()
    if(!ds)
      return(mapping[[previousPlotOpts$y]])

    y_col_selected = !is.null(input$y_column)
    if(y_col_selected)
      return(mapping[[input$y_column]])
    else
      return(mapping$Both)
  })


  #----CPA----
  cpaParams = callModule(cpaParamsServer, "cpaParams", cpaInputData)

  doPlotCpa = reactiveVal(F)
  observeEvent(cpaInputData(), {
    doPlotCpa(F)
  })

  observe({
    doPlotCpa(getElementSafe('submit_valid', cpaParams, F))
  })

  cpaMarkers = reactive({
    req(cpaInputData())
    cpa_params = cpaParams

    if(is.null(cpa_params)) return(NULL)

    plot_opts = smoothedPlotOptions
    cpa_input = cpaInputData()
    mask = list()
    mask[[expr_text(cpa_input_df_pronoun)]] = cpa_input
    eval_tidy(cpaMarkersCode(), data = mask)
  })

  cpaInputData = reactive({
    req(previousData())

    mask = list()
    mask[[et(input_sym)]] = previousData()
    eval_tidy(cpaInputDataCode(), data = mask)
  })

  cpaInputDataCode = reactive({
    req(previousData())

    if(doRegularize())
      expr(!!cpa_input_df_pronoun <- !!sym(input_sym) %>%
             preprocessForCpa(
               !!(preprocess$window_width),
               agg_fn_expr = !!(preprocess$agg_fn_expr),
               stride = !!(preprocess$window_stride),
               index_col = !!(cpaIndexColumn()),
               values_col = !!(preprocess$values_col),
               facet_col = !!(previousPlotOpts$facetOn)
             )
      )
    else
      expr(!!cpa_input_df_pronoun <- !!sym(input_sym))
  })

  cpaMarkersCode = reactive({
    if(!doPlotCpa())
      return("")

    facet_col_sym = previousPlotOpts$facetOn
    if(is_str_set(facet_col_sym)) facet_col_sym = sym(facet_col_sym)

    cpa_params = reactiveValuesToList(cpaParams)
    cpa_params$submit_valid = NULL
    cpaPipelineCode(cpa_input_df_pronoun, sym(cpaIndexColumn()), sym(cpaInputColumn()),
                    output_sym = cpa_markers_sym, facet_column = facet_col_sym,
                    !!!cpa_params)
  })

  observeEvent(doRegularize(), {print("do regularize changed")})
  observeEvent(previousData(), {print("prev data changed")})
  observe({tmp = preprocess$agg_fn; print("agg fn changed")})
  observe({tmp = cpaIndexColumn(); print("index col changed")})
  observe({tmp = previousPlotOpts$facetOn; print("facet col changed")})

  cpaInputColumn = reactive({
    if(doRegularize()) 'CpaInput'
    else yDisplay()
  })

  cpaIndexColumn = reactive({
    smoothedPlotOptions$x
  })


  #----Source Code----
  annotateSourceString = function(tab_code)
    expressionsToString(previousSourceString(),
                        "",
                        "#CPA",
                        tab_code)

  fullSourceWithPlotString = reactive({
    req(previousSourceString())

    expressionsToString(
      annotateSourceString(
        currentTabWithPlotCode())
      ,
      "",
      "plot(p)"
    )
  })

  currentTabCode = reactive({
    req(isDataLoaded())

    codes = list()
    codes[[et(cpa_input_df_pronoun)]] = cpaInputDataCode()
    codes[[et(cpa_markers_sym)]] = cpaMarkersCode()

    codes
  })

  fullSourceString = reactive({
    req(isDataLoaded())

    annotateSourceString(currentTabCode())
  })

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

  output$fullSourceWithPlot = renderText({
    fullSourceWithPlotString()
  })


  #----Return----

  return(list(
    cpaMarkers = cpaMarkers,
    doPlotCpa = doPlotCpa,
    fullSourceString = fullSourceString
  ))
}
