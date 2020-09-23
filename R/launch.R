#' Launch or create READ-TV shiny application
#'
#' Returns a read-tv shiny application, or launches it if not set
#'
#' @param data A data frame. If \code{data} does not have columns for \code{Time}, \code{Case}, and \code{Event.Type}, then a pop-up will display to map these columns.
#' @param eventsPath A file path to a data frame. Can not be set with \code{data}. READ-TV will load \code{eventsPath} with \code{readr::read_rds} or \code{readr::read_csv}.
#' @param plotOpts A list of keys and values for generating the first basic display plot. THe defaults can be viewed with \code{generatePlotDefaults()}. Wrap in \code{tvOpts} call for non-standard evaluation. See \code{?tvOpts} for more information.
#'
#' @export
#'
#' @examples
#'
#' library(dplyr)
#' library(magrittr)
#'
#' app = readtv::japan_eq_3_11 %>%
#'   mutate(Time = time, Case = 1, Event.Type = place) %>%
#'   launchReadtv(plotOpts = tvOpts(
#'     x = time, y = mag, color = place, facetOn = place,
#'     isFacetPaginated = TRUE, facetRowsPerPage = 3, facetPage = 2))
#'
#' #shiny::runApp(app, port = 80, host = '0.0.0.0')
#'
launchReadtv = function(data = NULL, eventsPath = NULL, plotOpts = list()) {
  if(!is.null(eventsPath) & !is.null(data))
    stop("do not specify both eventsPath and data, specificying neither is OK")
  shiny::shinyApp(ui = genUi(),
           server = genServerFn(eventsPath = eventsPath, data = data,
                                initPlotOpts = plotOpts))
}
