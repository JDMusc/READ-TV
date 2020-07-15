
cpaParamsUI = function(id) {
  ns = NS(id)
  fluidRow(
    column(uiOutput(ns("methodSelect")), width = 2),
    column(uiOutput(ns("penaltySelect")), width = 2),
    column(uiOutput(ns("qSelect")), width = 2),
    actionButton(ns("cpaSubmit"), label = "Calculate CPA")
  )
}

cpaParamsServer = function(input, output, session, preprocess) {
  ns = session$ns
  
  defaults = generateCpaDefaults()
  
  #----CPA Method----
  output$methodSelect = renderUI({
    method = defaultOrRet('method')
    selectInput(ns("methodSelect"), "Method", methods, selected = method)
  })
  
  methods = cpaMethods()
  
  #----CPA Penalty----
  output$penaltySelect = renderUI({
    penalty = defaultOrRet('penalty')
    selectInput(ns("penaltySelect"), "Penalty", penalties, selected = penalty)
  })
  
  penalties = cpaPenalties()
  
  #----Number of Change Points----
  output$qSelect = renderUI({
    q = defaultOrRet('Q')
    selectInput(ns("qSelect"), "# Change Pts", 1:6, selected = q)
  })
  
  
  #----Should Recalculate----
  observe({
    tmp = input$qSelect
    tmp = input$methodSelect
    tmp = input$penaltySelect
    tmp = input$pen.value
    tmp = preprocess
    
    shinyjs::enable('cpaSubmit')
  })
  
  #----Submit----
  ret = reactiveValues(submit = F)
  
  defaultOrRet = function(field) 
    getElementSafe(field, ret, defaults[[field]])
  
  observeEvent(input$cpaSubmit, {
    ret$submit = T
    ret$Q = as.numeric(input$qSelect)
    ret$method = input$methodSelect
    ret$penalty = input$penaltySelect
    ret$pen.value = defaults$pen.value
    
    updateActionButton(session, "cpaSubmit", label = "Update CPA Params")
    shinyjs::disable("cpaSubmit")
  })
  
  return(ret)
}
