function(input, output, session){
  
  output$mainDisplay = renderUI({
    id = "mainDisplay"
    callModule(mainDisplayServer, id)
    
    div(
      mainDisplayUI(id)
    )
  })
}
