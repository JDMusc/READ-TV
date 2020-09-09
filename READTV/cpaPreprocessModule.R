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
  smoothInputs = reactive({
    base = c("windowWidthText", "smoothStrideText", "aggFn")
    
    if(showTimeOptions())
      c(base,
        "windowWidthUnits", "smoothStrideUnits")
    else
      base
  })
  
  toggleSmoothControl = function(id)
    shinyjs::toggle(id = id, condition = doSmooth())
  
  observe({
    smoothInputUIs %>% sapply(toggleSmoothControl)
  })
  
  
  #----Time Options----
  convValues = reactiveValues(windowWidth = 5,
                               smoothStride = 2)
  convUnits = reactiveValues(windowWidth = NULL,
                             smoothStride = NULL)
  
  showTimeOptions = reactive({
    req(previousData())
    
    previousData() %>% 
      pull(previousPlotOpts$xColumn) %>% 
      {is.difftime(.) | is.timepoint(.)}
  })
  
  
  timePrefixes = list(
    pico = 1e-12,
    nano = 1e-9,
    micro = 1e-6,
    milli = 1e-3
  )
  
  timeUnits = c(
    "seconds",
    "minutes",
    "hours",
    "days",
    "weeks",
    "months",
    "years"
  )
  
  timeChoices = append(
    timePrefixes %>% names %>% purrr::map(~ paste0(.x, 'seconds')),
    timeUnits
  ) %>% 
    purrr::map(~ paste0('d', .x))
  
  
  defaultUnits = function(times) {
    tdiff = max(times) - min(times)
    range_units = attr(tdiff, 'units')
    
    if(range_units == 'secs')
      range_units = 'seconds'
    else if(range_units == 'mins')
      range_units = 'minutes'
    else if(range_units == 'auto')
      return('seconds')
    
    ix = which(timeUnits == range_units)
    
    if(ix > 1) return(timeUnits[[ix - 1]])
    
    tdiff_sec = as.numeric(tdiff, 'secs')
    ret = timePrefixes %>% 
      keep(~ .x < tdiff_sec) %>% 
      names %>% 
      map(~ paste0(.x, 'seconds')) %>% 
      head(1)
    
    if(is_empty(ret)) 'picoseconds'
    else ret
  }
  
  
  timeUnitsTextInput = function(tag, text_label, value) {
    ti = textInput(ns(f("${tag}Text")), text_label,
                   value = value)
    
    if(showTimeOptions()) {
      fluidRow(
        ti,
        selectInput(ns(f("${tag}Units")), "Units",
                    choices = timeChoices,
                    selected = convUnits[[tag]])
      )
    }
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
              value = convValues$windowWidth)
  })
  
  windowWidth = reactive({
    req(input$windowWidthText)
    
    extractTimeValue('windowWidth')
  })
  
  
  observe({
    req(showTimeOptions())
    
    for(tag in names(convUnits)) {
      val = input %>% extract2(f('${tag}Units'))
      if(is.null(val))
        val = previousData() %>% 
          pull(previousPlotOpts$xColumn) %>% 
          defaultUnits %>% 
          {paste0('d', .)}
      
      orig = convUnits[[tag]]
      if(not_equals_null_safe(orig, val))
        convUnits[[tag]] = val
    }
  })
  
  observe({
    for(tag in names(convValues)) {
      val = input %>% extract2(f('${tag}Text')) %>% as.numeric
      if(is_null_or_empty(val))
        next
      
      orig = convValues[[tag]]
      if(not_equals(orig, val))
        convValues[[tag]] = val
    }
  })
  
  windowWidthUnits = reactive({
    req(showTimeOptions())
    units = input %>% extract2(f('${tag}Units'))
  })
  
  
  #----Smooth Stride----
  output$smoothStride = renderUI({
    req(previousData())
    value = getElementSafe("smooth_stride", ret, 5)
    
    timeUnitsTextInput("smoothStride", "Interval Stride",
                       value = convValues$smoothStride)
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
    for(si in setdiff(smoothInputs(), "preprocessSubmit")) 
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
