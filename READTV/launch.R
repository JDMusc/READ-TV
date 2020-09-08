launchReadtv = function(eventsPath = NULL) {
  source('global.R') #keep in function to prevent recursion
  
  shinyApp(ui = genUi(), server = genServerFn(eventsPath = eventsPath))
}