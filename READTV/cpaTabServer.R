
cpaTabServer = function(input, output, session, previousData, 
                        headerMinimalInformation, isDataLoaded,
                        previousPlotOpts){
  ns = session$ns
  
  #----Plot----
  timePlot <- reactive({
    req(plotData())
    
    plot_opts = plotOptions()
    p = generateTimePlot(plotData(), plot_opts)
    
    if(doCpa()) {
      p = addCpaMarkersToPlot(p, cpaMarkers())
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
  
  showSmoothed = reactive({
    (yColumn() != previousPlotOpts$yColumn) & doSmooth()
  })
  
  plotData = reactive({
    d = previousData()
    
    if(showSmoothed())
      cpaData()
    else
      previousData()
  })
  
  plotOptions = reactive({
    ppo = previousPlotOpts
    plot_opts = ppo
    
    if(showSmoothed()) {
      no_selection = ppo$no_selection
      
      plot_opts = generatePlotDefaults(
        no_selection, ppo)
      
      plot_opts$yColumn = yColumn()
      plot_opts$shapeColumn = no_selection
      plot_opts$colorColumn = no_selection
    }
    
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
                         choices = yColumns), width = 2)
    )
  })
  
  yColumns = c("Original", "Smoothed")
  
  yColumn = reactive({
    default = previousPlotOpts$yColumn
    if(is.null(input$y_column)) return(default)
    
    if(input$y_column == "Original") default
    else 'CpaInput'
  })
  

  #----CPA----
  cpaParams = callModule(cpaParamsServer, "cpaParams", preprocess)
  
  doCpa = reactive({
    if(length(names(cpaParams)) == 0) return(F)
    
    return(cpaParams$submit)
  })
  
  cpaMarkers = reactive({
    req(cpaData())
    cpa_params = cpaParams
    
    if(is.null(cpa_params)) return(NULL)
    
    plot_opts = plotOptions()
    cpaData() %>% cpaPipeline(
      time_column = plot_opts$xColumn,
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
          preprocess$smoothed_window_n, agg_fn = preprocess$agg_fn,
          facet_col = facetColumn())
    else
      previousData()
  })
  
  cpaInputColumn = reactive({
    if(preprocess$do_smooth) 'CpaInput'
    else yColumn()
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
