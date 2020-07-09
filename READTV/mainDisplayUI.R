
mainDisplayUI <- function(id) {
  ns = NS(id)
  fp = fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(list(.invalid_query = 'background-color: #f006')),
    tabsetPanel(
      tabPanel(
        "Data Upload",
        div(
          shinythemes::themeSelector(),
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
        "Basic Display",
        div(
          uiOutput(ns("dataFilter")),
          fluidRow(
            column(uiOutput(ns("eventPlotContainer")), width = 12)
            ),
          fluidRow(
            column(uiOutput(ns("sidePanel")), width = 12)
          )
        )
      ),
      tabPanel(
        "CPA"
      ),
      tabPanel(
        "Forecasting"
      ),
      tabPanel(
        "Source Code",
      	div(
      	  sourceCodeUI(ns("sourcecode"))
      	)
      ),
    id = ns("tabs")
    )
  )

  fp
}

