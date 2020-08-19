
cpaTabServer = function(input, output, session, previousData, 
                        isDataLoaded, previousPlotOpts, 
                        facetPageN, previousSourceString,
                        input_sym = sym("filtered_data")){
  ns = session$ns
  
  
  #----Code Gen Symbols and Pronouns----
  base_plot_df_pronoun = sym("base_plot_df")
  cpa_markers_pronoun = sym("cpa_markers")
  cpa_plot_df_pronoun = sym("cpa_plot_df")
  p_pronoun = sym("p")
  
  
  #----Plot----
  timePlot <- reactive({
    req(previousData())
    
    plot_codes = plotCodes()
    cpa_plot_df = cpaInputData()
    base_plot_df = previousData()
    
    plot_opts = plotOptions
    cpa_plot_opts = smoothedPlotOptions
    
    plot_df = eval_tidy(plot_codes$prepare_plot_input, 
                        data = list(filtered_data = base_plot_df))
    
    base_plot_mask = list()
    base_plot_mask[[expr_text(base_plot_df_pronoun)]] = plot_df
    base_plot_mask[[expr_text(cpa_plot_df_pronoun)]] = cpa_plot_df
    p = eval_tidy(plot_codes$base_plot, data = base_plot_mask)
    
    event_freq_mask = list()
    event_freq_mask[[expr_text(p_pronoun)]] = p
    event_freq_mask[[expr_text(cpa_plot_df_pronoun)]] = cpa_plot_df
    p = eval_tidy(plot_codes$add_event_freq, data = event_freq_mask)
    
    
    add_markers_mask = list()
    add_markers_mask[[expr_text(p_pronoun)]] = p
    add_markers_mask[[expr_text(base_plot_df_pronoun)]] = base_plot_df
    add_markers_mask[[expr_text(cpa_plot_df_pronoun)]] = cpa_plot_df
    add_markers_mask[[expr_text(cpa_markers_pronoun)]] = cpaMarkers()
    
    p = eval_tidy(plot_codes$add_markers, data = add_markers_mask)
    
    p
  })
  
  plotCodes = reactive({
    req(previousData())
    
    codes = list()
    
    cpa_plot_df = cpaInputData()
    base_plot_df = previousData()
    filtered_data <- base_plot_df
    show_original = showOriginal()
    show_original_and_event_frequency = showOriginalAndEventFrequency()
    
    plot_opts = plotOptions
    cpa_plot_opts = smoothedPlotOptions
    
    prepare_plot_input = generatePreparePlotCode(quo(filtered_data),
                                                 plot_opts,
                                                 base_plot_df_pronoun)
    
    codes$prepare_plot_input = prepare_plot_input
    
    plot_df = eval_tidy(prepare_plot_input, 
                        data = list(orig_data = base_plot_df))
    
    base_plot_code = cpaTabLogic.basePlotCode(
      cpa_plot_df = cpa_plot_df,
      cpa_plot_opts = cpa_plot_opts,
      base_plot_df = plot_df,
      plot_opts = plot_opts,
      show_original = show_original,
      base_plot_df_pronoun = base_plot_df_pronoun,
      cpa_plot_df_pronoun = cpa_plot_df_pronoun
    )
    
    codes$base_plot = base_plot_code
    
    add_event_freq_code = cpaTabLogic.addEventFrequencyCode(
      plot_opts, 
      p_pronoun,
      cpa_plot_df_pronoun = cpa_plot_df_pronoun,
      cpa_plot_opts = cpa_plot_opts,
      show_original_and_event_frequency = show_original_and_event_frequency,
      smooth_fn_name = preprocess$agg_fn_label)
    
    codes$add_event_freq = add_event_freq_code
    
    add_markers_code = cpaTabLogic.addCpaMarkersCode(
      p_pronoun = p_pronoun, 
      cpa_plot_df = cpa_plot_df, 
      cpa_plot_opts = cpa_plot_opts, 
      cpa_plot_df_pronoun = cpa_plot_df_pronoun, 
      base_plot_df = plot_df, 
      plot_opts = plot_opts, 
      base_plot_df_pronoun = base_plot_df_pronoun, 
      cpa_markers_pronoun = cpa_markers_pronoun, 
      add_markers = doPlotCpa(), 
      show_original = show_original, 
      show_original_and_event_frequency = show_original_and_event_frequency
    )
    
    codes$add_markers = add_markers_code
    
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
    is.list(yColumnDisplay())
  })
  
  showSmoothed = reactive({
    (
      ('CpaInput' %in% yColumnDisplay()) | 
        showOriginalAndEventFrequency()
    ) & doSmooth()
  })
  
  showOriginal = reactive({
    (previousPlotOpts$yColumn %in% yColumnDisplay()) |
      showOriginalAndEventFrequency()
  })
  
  copyPlotOpts = function(plot_opts) {
    no_selection = plot_opts$no_selection
    
    generatePlotDefaults(no_selection, plot_opts)
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
    no_selection = plot_opts$no_selection
    
    y_col = yColumnDisplay()
    if(is.list(y_col)) 
      y_col = y_col$`Event Frequency`
    plot_opts$yColumn = y_col
    plot_opts$shapeColumn = no_selection
    plot_opts$colorColumn = no_selection
    
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
    isDataLoaded() & (ppo$facetRowsPerPage != ppo$no_selection)
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
  doSmooth = reactive({
    getElementSafe('do_smooth', preprocess, F)
  })
  
  #---Display----
  output$display = renderUI({
    
    yColumns = displayNoSelectionAsAnyEvent(
      c(previousPlotOpts$yColumn, "Event Frequency", "Both"),
      anyEvent = previousPlotOpts$anyEvent,
      no_selection = previousPlotOpts$no_selection)
    
    fluidRow(
      if(doSmooth())
        column(selectInput(ns("y_column"), "Y-axis",
                         choices = yColumns,
                         selected = "Both"), width = 2)
      else NULL
    )
  })
  
  yColMapping = reactive({
    mapping = list(`Event Frequency` = 'CpaInput')
    
    prev_y = previousPlotOpts$yColumn
    mapping[[prev_y]] = prev_y
    
    mapping$Both = mapping
    
    mapping
  })
  
  yColumnDisplay = reactive({
    mapping = yColMapping()
    
    ds = doSmooth()
    if(!ds)
      return(mapping[[previousPlotOpts$yColumn]])
    
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
    mask[[expr_text(cpa_plot_df_pronoun)]] = cpa_input
    eval_tidy(cpaMarkersCode(), data = mask)
  })
  
  cpaInputData = reactive({
    req(previousData())
    eval_tidy(cpaInputDataCode(), data = makeDataMask(previousData()))
  })
  
  cpaInputDataCode = reactive({
    req(previousData())
    
    ds = doSmooth()
    if(ds)
      expr(!!cpa_plot_df_pronoun <- !!sym(input_sym) %>% 
             preprocessForCpa(
               !!(preprocess$smooth_window_n), 
               agg_fn_expr = !!(preprocess$agg_fn_expr),
               stride = !!(preprocess$smooth_stride),
               index_col = !!(cpaIndexColumn()),
               facet_col = !!(facetColumn())
             )
      )
    else
      expr(!!cpa_plot_df_pronoun <- !!sym(input_sym))
  })
  
  cpaMarkersCode = reactive({
    if(!doPlotCpa())
      return("")
    
    facet_col_sym = facetColumn()
    if(!is.null(facet_col_sym)) facet_col_sym = sym(facet_col_sym)
    
    cpa_params = reactiveValuesToList(cpaParams)
    cpa_params$submit_valid = NULL
    cpaPipelineCode(cpa_plot_df_pronoun, sym(cpaIndexColumn()), sym(cpaInputColumn()), 
                    output_sym = sym(expr_text(cpa_markers_pronoun)),
                    facet_column = facet_col_sym, !!!cpa_params)
  })

  observeEvent(doSmooth(), {print("do smooth changed")})
  observeEvent(previousData(), {print("prev data changed")})
  observe({tmp = preprocess$agg_fn; print("agg fn changed")})
  observe({tmp = cpaIndexColumn(); print("index col changed")})
  observe({tmp = facetColumn(); print("facet col changed")})
  
  cpaInputColumn = reactive({
    if(doSmooth()) 'CpaInput'
    else yColumnDisplay()
  })
  
  cpaIndexColumn = reactive({
    smoothedPlotOptions$xColumn
  })
  
  facetColumn = reactive({
    ppo = previousPlotOpts
    if(ppo$facetColumn == ppo$no_selection) NULL
    else ppo$facetColumn
  })
  
  
  #----Source Code----
  myPlotSourceString = reactive({
    req(previousSourceString())
    
    plot_codes = plotCodes()
    
    expressionsToString(
      mySourceString(),
      "",
      "#Prepare Plot Input",
      plot_codes$prepare_plot_input,
      "",
      "#Base Plot",
      plot_codes$base_plot,
      "",
      "#Plot Event Frequency Overlay",
      plot_codes$add_event_freq,
      "",
      "#Plot CPA Markers",
      plot_codes$add_markers
    )
  })
  
  mySourceString = reactive({
    expressionsToString(
      previousSourceString(),
      "",
      "#CPA Plot DF",
      cpaInputDataCode(),
      "",
      "#CPA Markers",
      cpaMarkersCode()
    )
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
    myPlotSourceString()
  })
  
  makeDataMask = function(data) {
    mask = list()
    mask[[input_sym]] = data
    mask
  }
  
  #----Return----
  ret = reactiveValues()
  
  return(list(
    cpaMarkers = cpaMarkers,
    doPlotCpa = doPlotCpa,
    mySourceString = mySourceString
  ))
}
