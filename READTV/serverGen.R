genServerFn = function(data = NULL, eventsPath = NULL)
  function(input, output, session){
    
    output$mainDisplay = renderUI({
      id = "mainDisplay"
      callModule(mainDisplayServer, id, eventsPath, data)
      
      div(
        mainDisplayUI(id)
      )
    })
  }