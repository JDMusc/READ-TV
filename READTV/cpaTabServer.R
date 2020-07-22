
cpaTabServer = function(input, output, session, previousData, 
                        headerMinimalInformation, isDataLoaded,
                        previousPlotOpts){
  ns = session$ns
  
  #----Plot----
  timePlot <- reactive({
    req(previousData())
    
    prev_data = previousData()
    plot_opts = plotOptions()
    
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
    
    if(doCpa()) {
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
    fluidPage(
      plotOutput(ns("eventPlot"), 
                 height = plotOptions()$plotHeight
      ),
      uiOutput(ns("facetPageControl"))
    )
  })
  
  showOriginalAndEventFrequency = reactive({
    is.list(yColumnSelect())
  })
  
  showSmoothed = reactive({
    (
      ('CpaInput' %in% yColumnSelect()) | 
        showOriginalAndEventFrequency()
    ) & doSmooth()
  })
  
  showOriginal = reactive({
    (previousPlotOpts$yColumn %in% yColumnSelect()) | 
      showOriginalAndEventFrequency()
  })
  
  copyPlotOpts = function(plot_opts) {
    no_selection = plot_opts$no_selection
    
    generatePlotDefaults(no_selection, plot_opts)
  }
  
  plotOptions = reactive({
    plot_opts = copyPlotOpts(previousPlotOpts)
    if(doFacet())
      plot_opts$facetPage = facetPageControl$page
    plot_opts
  })
  
  #reactiveValues so that dependencies update on a given field instead of whole list
  smoothedPlotOptions = reactiveValues()
  observe({
    plot_opts = copyPlotOpts(plotOptions())
    no_selection = plot_opts$no_selection
    
    y_col = yColumnSelect()
    if(is.list(y_col)) 
      y_col = y_col$`Event Frequency`
    plot_opts$yColumn = y_col
    plot_opts$shapeColumn = no_selection
    plot_opts$colorColumn = no_selection
    
    comparable_fields = setdiff(names(plot_opts),
                                'geomFunction')
    for(nm in comparable_fields) {
      po_val = plot_opts[[nm]]
      if(!(nm %in% names(smoothedPlotOptions)))
        smoothedPlotOptions[[nm]] = po_val
      else {
        if(smoothedPlotOptions[[nm]] != po_val)
          smoothedPlotOptions[[nm]] = po_val
      }
    }
    
    smoothedPlotOptions$geomFunction = 
      plot_opts$geomFunction
  })
  
  
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
  
  facetPageN <- reactive({
    if(!doFacet()) -1
    else {
      ppo = previousPlotOpts
      n_plots = n_distinct(previousData()[[facetColumn()]])
      n_plots %/% ppo$facetRowsPerPage + 
        ifelse(n_plots %% ppo$facetRowsPerPage > 0, 1, 0)
    }
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
    yColumns = c("Original", "Event Frequency", "Both")
    
    fluidRow(
      column(selectInput(ns("markerDirection"), 
                         "Change-point Marker Direction",
                         choices = c("Vertical", "Horizontal", "Both")),
             width = 4),
      if(doSmooth())
        column(selectInput(ns("y_column"), "Y-axis",
                         choices = yColumns,
                         selected = "Both"), width = 2)
      else NULL
    )
  })
  
  yColumnSelect = reactive({
    original = previousPlotOpts$yColumn
    if(is.null(input$y_column)) return(original)
    
    mapping = list(Original = original, 
                   `Event Frequency` = 'CpaInput')
    mapping$Both = mapping
    
    mapping[[input$y_column]]
  })
  

  #----CPA----
  cpaParams = callModule(cpaParamsServer, "cpaParams", cpaInputData)
  
  doCpa = reactiveVal(F)
  observeEvent(cpaInputData(), {
    doCpa(F)
  })
  
  observe({
    doCpa(getElementSafe('submit_valid', cpaParams, F))
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
    else yColumnSelect()
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
  ))
}
