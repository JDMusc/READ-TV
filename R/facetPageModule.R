facetPageUI = function(id) {
  ns = NS(id)
  
  fluidRow(
    column(uiOutput(ns("facetPageSlider")),
           width = 3),
    wellPanel(
    actionButton(ns("frontPg"), "<<"),
    actionButton(ns("backPg"), "<"),
    actionButton(ns("nextPg"), ">"),
    actionButton(ns("endPg"), ">>"))
  )
}


facetPageServer = function(input, output, session, nPages,
                               initialPage = 1) {
  ns = session$ns
  
  output$facetPageSlider = renderUI({
    req(nPages())
    
    f = stringr::str_interp
    selectInput(ns("facetPageSlider"), 
                f("Facet Page (${page()} out of ${nPages()})"), 
                1:nPages(), 
                selected = page())
  })
  
  page = reactive({
    getElementSafe('page', ret, initialPage)
  })
  
  observeEvent(input$facetPageSlider, {
    ret$page = as.numeric(input$facetPageSlider)
  })
  
  observeEvent(input$nextPg, {
    ret$page = page() + 1
  })
  
  observeEvent(input$endPg, {
    ret$page = nPages()
  })
  
  observeEvent(input$backPg, {
    ret$page = page() - 1
  })
  
  observeEvent(input$frontPg, {
    ret$page = 1
  })
  
  nextButtons = c("nextPg", "endPg")
  backButtons = c("backPg", "frontPg")
  
  observeEvent(page(), {
    pg = page()
    tsGen = function(condition) 
      function(id) shinyjs::toggleState(id = id, condition = condition)
    
    nextButtons %>% sapply(tsGen(pg < nPages()))
    backButtons %>% sapply(tsGen(pg > 1))
  })
  
  #----Return----
  ret = reactiveValues()
  
  return(ret)
}
