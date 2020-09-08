launchReadtv = function(eventsPath = NULL)
  shinyApp(ui = genUi(), 
           server = genServerFn(eventsPath = eventsPath))