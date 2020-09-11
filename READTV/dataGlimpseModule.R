dataGlimpseUI = function(id) {
  ns = NS(id)
  
  verbatimTextOutput(ns("dataGlimpse"))
}

dataGlimpseServer = function(input, output, session, data) {
  
  output$dataGlimpse = renderPrint({
    req(data())
    
    str(data())
  })
}