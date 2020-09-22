
cpaOverlayTabUI <- function(id) {
  ns = NS(id)
  div(
    uiOutput(ns("dataFilter")),
    fluidRow(
      column(uiOutput(ns("eventPlotContainer")), width = 12)
    ),
    fluidRow(
      column(uiOutput(ns("sidePanel")), width = 12)
    )
  )
}

