cpaPreprocessUI = function(id) {
  ns = NS(id)
  
  fluidRow(
    column(selectInput(ns("doSmooth"), "Smooth",
                       choices = c("Yes", "No"),
                       selected = "Yes"),
           width = 2),
    column(uiOutput(ns("windowWidth")), width = 2),
    column(selectInput(ns("aggFn"), "Smooth Function",
                       choices = c("sum", "mean"),
                       selected = "sum"),
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
    if(is.null(input$doSmooth)) return(FALSE)
    
    return(input$doSmooth == "Yes")
  })
  
  
  #----Window Width----
  observe({
    shinyjs::toggle(id = "windowWidth",
                    condition = doSmooth())
  })
  
  output$windowWidth = renderUI({
    req(previousData())
    
    value = previousData()[[previousPlotOpts$xColumn]] %>% 
      as.numeric %>% 
      mean %>% 
      round(2)
    
    value = getElementSafe("smoothed_window_n", ret, value)
    
    textInput(ns("windowWidthText"), "Smooth Width",
              value = value)
  })
  
  #----Preprocess Changed----
  observe({
    tmp = doSmooth()
    tmp = input$windowWidthText
    tmp = input$aggFn
    
    shinyjs::enable('preprocessSubmit')
  })
  
  #----Return----
  ret = reactiveValues()
  
  observeEvent(input$preprocessSubmit, {
    ret$smoothed_window_n = as.numeric(input$windowWidthText)
    ret$agg_fn = eval(parse(text = input$aggFn))
    ret$do_smooth = doSmooth()
    shinyjs::disable("preprocessSubmit")
  })
  
  return(ret)
}