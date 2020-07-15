
mainDisplayUI <- function(id) {
  ns = NS(id)
  fp = fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(list(.invalid_query = 'background-color: #f006')),
    tabsetPanel(
      tabPanel("Data Upload",
               dataUploadTabUI(ns("dataUpload"))
      ),
      tabPanel(
        'Filter & Facet',
        basicDisplayTabUI(ns("basicDisplay"))
      ),
      tabPanel(
        "CPA",
        cpaTabUI(ns("cpa"))
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

