
cpaTabServer = function(input, output, session, previousData, 
                        headerMinimalInformation, isDataLoaded,
                        previousPlotOpts){
  ns = session$ns
  
  #----Plot----
  timePlot <- reactive({
    req(previousData())
    
    prev_data = previousData()
    prev_plot_opts = previousPlotOpts
    
    if(showSmoothed()) {
      cpa_data = cpaData()
      cpa_plot_opts = smoothedPlotOptions()
    }
    
    if(!showOriginal()) {
      p = generateTimePlot(cpa_data, cpa_plot_opts)
    }
    else
      p = generateTimePlot(prev_data, prev_plot_opts)
    
    show_both = showOriginalAndEventFrequency()
    if(show_both){
      p = addEventFrequencyToPlot(
        p, cpa_data, 
        cpa_plot_opts$xColumn, cpa_plot_opts$yColumn)
    }
    
    if(doCpa()) {
      use_cpa_y = !showOriginal()
      if(show_both) {
        mx_cpa = cpa_data %>% 
          pull(!!sym(cpa_plot_opts$yColumn)) %>% 
          max(na.rm = T)
        
        mx_prev = prev_data %>% 
          getElementSafe(prev_plot_opts$yColumn, 1) %>% 
          max(na.rm = T)
        
        use_cpa_y = mx_cpa > mx_prev
      }
      
      if(use_cpa_y) {
        plot_data = cpa_data
        y_col = cpa_plot_opts$yColumn
      } else {
        plot_data = prev_data
        y_col = prev_plot_opts$yColumn
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
                 height = previousPlotOpts$plotHeight
      ),
      uiOutput(ns("facetPageSlider"))
    )
  })
  
  showOriginalAndEventFrequency = reactive({
    is.list(yColumn())
  })
  
  showSmoothed = reactive({
    (
      ('CpaInput' %in% yColumn()) | 
        showOriginalAndEventFrequency()
    ) & doSmooth()
  })
  
  showOriginal = reactive({
    (previousPlotOpts$yColumn %in% yColumn()) | 
      showOriginalAndEventFrequency()
  })
  
  smoothedPlotOptions = reactive({
    ppo = previousPlotOpts
    no_selection = ppo$no_selection
    
    plot_opts = generatePlotDefaults(no_selection, ppo)
    
    y_col = yColumn()
    if(is.list(y_col)) 
      y_col = y_col$`Event Frequency`
    plot_opts$yColumn = y_col
    plot_opts$shapeColumn = no_selection
    plot_opts$colorColumn = no_selection
    
    plot_opts
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
  

  #----Preprocess----
  preprocess = callModule(cpaPreprocessServer, "preprocess", previousData,
                          previousPlotOpts)
  doSmooth = reactive({
    getElementSafe('do_smooth', preprocess, F)
  })
  
  #---Display----
  output$display = renderUI({
    fluidRow(
      column(selectInput(ns("markerDirection"), 
                         "Change-point Marker Direction",
                         choices = c("Vertical", "Horizontal", "Both")),
             width = 4),
      column(selectInput(ns("y_column"), "Y-axis",
                         choices = yColumns,
                         selected = "Both"), width = 2)
    )
  })
  
  yColumns = c("Original", "Event Frequency", "Both")
  
  yColumn = reactive({
    original = previousPlotOpts$yColumn
    if(is.null(input$y_column)) return(original)
    
    mapping = list(Original = original, 
                   `Event Frequency` = 'CpaInput')
    mapping$Both = mapping
    
    mapping[[input$y_column]]
  })
  

  #----CPA----
  cpaParams = callModule(cpaParamsServer, "cpaParams", cpaData)
  
  doCpa = reactiveVal(F)
  observeEvent(cpaData(), {
    doCpa(F)
  })
  
  observe({
    doCpa(getElementSafe('submit_valid', cpaParams, F))
  })
  
  cpaMarkers = reactive({
    req(cpaData())
    cpa_params = cpaParams
    
    if(is.null(cpa_params)) return(NULL)
    
    plot_opts = smoothedPlotOptions()
    cpaData() %>% cpaPipeline(
      time_column = cpaIndexColumn(),
      values_column = cpaInputColumn(), 
      facet_column = facetColumn(),
      cpa_params = cpa_params, preprocess = F)
  })
  
  cpaData = reactive({
    req(previousData())
    
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
  
  cpaInputColumn = reactive({
    if(preprocess$do_smooth) 'CpaInput'
    else yColumn()
  })
  
  
  cpaIndexColumn = reactive({
    smoothedPlotOptions()$xColumn
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
