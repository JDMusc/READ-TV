
sourceCodeUI <- function(id) {
  ns = NS(id)
  fluidPage(
    shinyjs::useShinyjs(),
    tags$style(type="text/css", 
	       paste0("#", ns('code'), ' {white-space: pre-wrap;}')),
    textOutput(ns("code"))
  )
}

