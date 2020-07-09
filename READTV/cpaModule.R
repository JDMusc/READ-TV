
cpaUI = function(id) {
  ns = NS(id)
  uiOutput(ns("cpaOptions"))
}

cpaServer = function(input, output, session, data,
                                  plot_options) {
  ns = session$ns
  
  methods = cpaMethods()
  penalties = cpaPenalties()
  
  defaults = generateCpaDefaults()
  ret = reactiveValues()
  
  output$cpaOptions = renderUI({
    fluidRow(
      column(uiOutput(ns("windowWidth")), width = 2),
      column(uiOutput(ns("methodSelect")), width = 2),
      column(uiOutput(ns("penaltySelect")), width = 2),
      column(uiOutput(ns("qSelect")), width = 2),
      actionButton(ns("cpaSubmit"), label = "Submit")
    )
  })
  
  output$windowWidth = renderUI({
    req(data())
    
    value = data()[[plot_options$xColumn]] %>% 
      as.numeric %>% 
      mean %>% 
      round(2)
    
    if("smooth_window_n" %in% names(ret))
      value = ret$smooth_window_n
    
    textInput(ns("windowSelect"), "Smooth Width",
              value = value)
  })
  
  output$methodSelect = renderUI({
    method = defaultOrRet('method')
    selectInput(ns("methodSelect"), "Method", methods, selected = method)
  })
  
  output$penaltySelect = renderUI({
    penalty = defaultOrRet('penalty')
    selectInput(ns("penaltySelect"), "Penalty", penalties, selected = penalty)
  })
  
  output$qSelect = renderUI({
    q = defaultOrRet('Q')
    selectInput(ns("qSelect"), "# Change Pts", 1:6, selected = q)
  })
  
  defaultOrRet = function(field) {
    if(field %in% names(ret))
      ret[[field]]
    else
      defaults[[field]]
  }
  
  observeEvent(input$cpaSubmit, {
    ret$Q = as.numeric(input$qSelect)
    ret$method = input$methodSelect
    ret$penalty = input$penaltySelect
    ret$smooth_window_n = as.numeric(input$windowSelect)
    ret$pen.value = defaults$pen.value
  })
  
  return(ret)
}
