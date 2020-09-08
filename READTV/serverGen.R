genServerFn = function(eventsPath = NULL)
  function(input, output, session){
    
    output$mainDisplay = renderUI({
      id = "mainDisplay"
      callModule(mainDisplayServer, id, eventsPath)
      
      div(
        mainDisplayUI(id)
      )
    })
  }