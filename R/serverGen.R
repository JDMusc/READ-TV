genServerFn = function(data = NULL, eventsPath = NULL, initPlotOpts = list())
  function(input, output, session){

    output$mainDisplay = renderUI({
      id = "mainDisplay"

      callModule(mainDisplayServer, id, eventsPath = eventsPath, inputData = data,
                   initPlotOpts = initPlotOpts)

      div(
        mainDisplayUI(id)
      )
    })
  }
