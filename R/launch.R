#' @export
launchReadtv = function(data = NULL, eventsPath = NULL, plotOpts = list()) {
  if(!is.null(eventsPath) & !is.null(data))
    stop("do not specify both eventsPath and data, specificying neither is OK")
  shiny::shinyApp(ui = genUi(),
           server = genServerFn(eventsPath = eventsPath, data = data,
                                initPlotOpts = plotOpts))
}
