
cpaTabUI <- function(id) {
  ns = NS(id)
  div(
    fluidRow(
      column(uiOutput(ns("eventPlotContainer")), width = 12)
      ),
    fluidRow(
      column(uiOutput(ns("sidePanel")), width = 12)
    )
  )
}

