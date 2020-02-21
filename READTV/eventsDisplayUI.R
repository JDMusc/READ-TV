
eventsDisplayUI <- function(id) {
  ns = NS(id)
  fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(list(.invalid_query = 'background-color: #f006')),
    tabsetPanel(
      tabPanel(
        "Data Upload",
        div(
          actionButton(ns("minimizeHeader"), "Minimize"),
          uiOutput(ns("headerInformation")),
          div(id = ns("loadDataHeader"),
              fluidRow(
                eventsLoaderUI(ns("loadData"))),
              fluidRow(
                metaQueryLoaderUI(ns("loadMetaData")),
                metaQueryUI(ns("metaqueryui"))
                )
          )
        )
      ),
      tabPanel(
        "Display",
        div(
          uiOutput(ns("dataFilter")),
          fluidRow(
            column(uiOutput(ns("eventPlotContainer")), width = 10),
            column(uiOutput(ns("sidePanel")), width = 2)
          )
        )
      )
    )
  )
}

