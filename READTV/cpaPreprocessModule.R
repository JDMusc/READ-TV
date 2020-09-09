cpaPreprocessUI = function(id) {
  ns = NS(id)
  
  fluidRow(
    column(uiOutput(ns("doSmooth")), width = 2),
    column(uiOutput(ns("windowWidth")), width = 2),
    column(uiOutput(ns("smoothStride")), width = 2),
    column(selectInput(ns("aggFn"), "Freq. Metric",
                       choices = c("count", "rate"),
                       selected = "count"),
           width = 2),
    column(actionButton(ns("preprocessSubmit"), label = "Preprocess"),
           width = 2)
  )
}


cpaPreprocessServer = function(input, output, session, previousData,
                               previousPlotOpts) {
  ns = session$ns
  
  #----Do Smooth----
  doSmooth = reactive({
    if(is.null(input$doSmoothSelect)) return(FALSE)
    
    return(input$doSmoothSelect == "Event Frequency")
  })
  
  
  yColumn = reactive({
    previousPlotOpts$yColumn
  })
  
  isYcolAnyEvent = reactive(
    yColumn() == previousPlotOpts$no_selection
  )
  
  eventFrequency = "Event Frequency"
  
  validCpaInputs = reactive({
    vals = c(eventFrequency)
    
    if(isYcolAnyEvent()) vals
    else yColumn() %>% 
      append(vals) %>% 
      displayNoSelectionAsAnyEvent(
        previousPlotOpts$anyEvent,
        previousPlotOpts$no_selection
      )
  })
  
  output$doSmooth = renderUI({
    selected = if(isYcolAnyEvent()) eventFrequency else yColumn()
    selectInput(ns("doSmoothSelect"), "CPA Input",
                     choices = validCpaInputs(),
                     selected = selected)
  })
  
  smoothInputUIs = c("windowWidth", "smoothStride", "aggFn", "preprocessSubmit")
  smoothInputs = c("windowWidthText", "smoothStrideText", "aggFn")
  
  toggleSmoothControl = function(id)
    shinyjs::toggle(id = id, condition = doSmooth())
  
  observe({
    smoothInputUIs %>% sapply(toggleSmoothControl)
  })
  
  
  #----Time Options----
  showTimeOptions = reactive({
    req(previousData())
    
    previousData() %>% 
      pull(previousPlotOpts$xColumn) %>% 
      {is.difftime(.) | is.timepoint(.)}
  })
  
  timeChoices = c(
    "dnanoseconds",
    "dmicroseconds",
    "dmilliseconds",
    "dpicoseconds",
    "dseconds",
    "dminutes",
    "dhours",
    "ddays",
    "dweeks",
    "dmonths",
    "dyears"
  )
  
  timeUnitsTextInput = function(tag, text_label, value) {
    ti = textInput(ns(f("${tag}Text")), text_label,
                   value = value)
    
    if(showTimeOptions())
      fluidRow(
        ti,
        selectInput(ns(f("${tag}Units")), "Units",
                    choices = timeChoices)
      )
    else
      ti
  }
  
  
  extractTimeValue = function(tag) {
    val = input %>% extract2(f("${tag}Text")) %>% as.numeric
    
    if(showTimeOptions()) {
      units = input %>% extract2(f('${tag}Units'))
      
      expr(!!val %>% !!parse_expr(units)) %>% 
        eval_tidy
    }
    else
      val
  }
  
  
  #----Window Width----
  output$windowWidth = renderUI({
    req(previousData())
    
    value = previousData() %>% 
      pull(!!sym(previousPlotOpts$xColumn)) %>% 
      {. - dplyr::lag(.)} %>% 
      replace_na(0) %>% 
      {.[.>=0]} %>% 
      mean %>% 
      round(2)
    
    value = getElementSafe("smooth_window_n", ret, value)
    
    timeUnitsTextInput("windowWidth", "Interval Width",
              value = value)
  })
  
  windowWidth = reactive({
    req(input$windowWidthText)
    
    extractTimeValue('windowWidth')
  })
  
  
  #----Smooth Stride----
  output$smoothStride = renderUI({
    req(previousData())
    value = getElementSafe("smooth_stride", ret, 5)
    
    timeUnitsTextInput("smoothStride", "Interval Stride",
                       value = value)
  })
  
  
  smoothStride = reactive({
    req(input$smoothStrideText)
    
    extractTimeValue('smoothStride')
  })

  #----Aggregate Function----
  aggregateFunctionExpr = reactive({
    agg_fn = input$aggFn
    if(agg_fn == "count") expr(length(.values))
    else expr(length(.values)/!!(windowWidth()))
  })
  
  #----Preprocess Changed----
  observe({
    for(si in setdiff(smoothInputs, "preprocessSubmit")) 
      tmp = input[[si]]
    
    shinyjs::enable('preprocessSubmit')
  })
  
  #----Return----
  ret = reactiveValues()
  
  observeEvent(input$preprocessSubmit, {
    ret$smooth_window_n = windowWidth()
    ret$agg_fn_expr = aggregateFunctionExpr()
    ret$agg_fn_label = input$aggFn
    ret$smooth_stride = smoothStride()
    ret$do_smooth = doSmooth()
    shinyjs::disable("preprocessSubmit")
  })
  
  return(ret)
}
