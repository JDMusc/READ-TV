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
  
  #----Window Width----
  output$windowWidth = renderUI({
    req(previousData())
    
    value = previousData() %>% 
      pull(!!sym(previousPlotOpts$xColumn)) %>% 
      {. - dplyr::lag(., default = 0)} %>% 
      {.[.>=0]} %>% 
      mean %>% 
      round(2)
    
    value = getElementSafe("smooth_window_n", ret, value)
    
    textInput(ns("windowWidthText"), "Interval Width",
              value = value)
  })
  
  windowWidth = reactive({
    req(input$windowWidthText)
    
    as.numeric(input$windowWidthText)
  })
  
  #----Smooth Stride----
  output$smoothStride = renderUI({
    req(previousData())
    value = getElementSafe("smooth_stride", ret, 5)
    
    textInput(ns("smoothStrideText"), "Interval Stride",
              value = value)
  })

  #----Aggregate Function----
  aggregateFunction = reactive({
    agg_fn = input$aggFn
    if(agg_fn == "count") length
    else function(values) length(values)/windowWidth()
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
    ret$smooth_window_n = as.numeric(input$windowWidthText)
    ret$agg_fn = aggregateFunction()
    ret$agg_fn_label = input$aggFn
    ret$smooth_stride = as.numeric(input$smoothStrideText)
    ret$do_smooth = doSmooth()
    shinyjs::disable("preprocessSubmit")
  })
  
  return(ret)
}
