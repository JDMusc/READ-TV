launchReadtv = function(data = NULL, eventsPath = NULL) {
  if(!is.null(eventsPath) & !is.null(data))
    stop("do not specify both eventsPath and data, specificying neither is OK")
  shinyApp(ui = genUi(), 
           server = genServerFn(eventsPath = eventsPath, data = data))
}