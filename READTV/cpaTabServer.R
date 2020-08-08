
cpaTabServer = function(input, output, session, previousData, 
                        isDataLoaded, previousPlotOpts, 
                        facetPageN){
  ns = session$ns
  
  #----Plot----
  timePlot <- reactive({
    req(previousData())
    
    prev_data = previousData()
    plot_opts = plotOptions
    
    if(showSmoothed()) {
      cpa_input_data = cpaInputData()
      cpa_plot_opts = smoothedPlotOptions
    }
    
    if(!showOriginal()) {
      p = generateTimePlot(cpa_input_data, cpa_plot_opts)
    }
    else
      p = generateTimePlot(prev_data, plot_opts)
    
    show_both = showOriginalAndEventFrequency()
    if(show_both & !showSmoothed()) browser()
    if(show_both){
      p = addEventFrequencyToPlot(
        p, cpa_input_data, 
        cpa_plot_opts$xColumn, cpa_plot_opts$yColumn,
        preprocess$agg_fn_label
      )
    }
    
    if(plotCpa()) {
      use_cpa_y = !showOriginal()
      if(show_both) {
        mx_cpa = cpa_input_data %>% 
          pull(!!sym(cpa_plot_opts$yColumn)) %>% 
          max(na.rm = T)
        
        mx_prev = prev_data %>% 
          getElementSafe(plot_opts$yColumn, 1) %>% 
          max(na.rm = T)
        
        use_cpa_y = mx_cpa > mx_prev
      }
      
      if(use_cpa_y) {
        plot_data = cpa_input_data
        y_col = cpa_plot_opts$yColumn
      } else {
        plot_data = prev_data
        y_col = plot_opts$yColumn
      }
      
      p = addCpaMarkersToPlot(p, cpaMarkers(),
                              plot_data, y_col)
    }
    
    p
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
      tabPanel("Display", uiOutput(ns("display")))
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
#      column(selectInput(ns("markerDirection"), 
#                         "Change-point Marker Direction",
#                         choices = c("Vertical", "Horizontal", "Both")),
#             width = 4),
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
  
  plotCpa = reactiveVal(F)
  observeEvent(cpaInputData(), {
    plotCpa(F)
  })
  
  observe({
    plotCpa(getElementSafe('submit_valid', cpaParams, F))
  })
  
  cpaMarkers = reactive({
    req(cpaInputData())
    cpa_params = cpaParams
    
    if(is.null(cpa_params)) return(NULL)
    
    plot_opts = smoothedPlotOptions
    cpaInputData() %>% cpaPipeline(
      time_column = cpaIndexColumn(),
      values_column = cpaInputColumn(), 
      facet_column = facetColumn(),
      cpa_params = cpa_params, preprocess = F)
  })
  
  cpaInputData = reactive({
    req(previousData())
    #print("cpa data about to be set")
    
    ds = doSmooth()
    if(ds)
      previousData() %>% 
        preprocessForCpa(
          preprocess$smooth_window_n, 
          agg_fn = preprocess$agg_fn,
          index_col = cpaIndexColumn(),
          facet_col = facetColumn())
    else
      previousData()
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
  
  
  #----Return----
  ret = reactiveValues()
  
  return(list(
    cpaMarkers = cpaMarkers,
    plotCpa = plotCpa
  ))
}
