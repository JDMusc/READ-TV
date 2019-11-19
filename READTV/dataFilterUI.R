

dataFilterUI = function(id) {
  ns = NS(id)
  selectRows(ns)
}


selectRows <- function(ns) {
  fluidRow(
    column(
      width = 2,
      multiSelectUI(ns("caseFilter"), "Case")
    ),
    column(
      width = 2,
      multiSelectUI(ns("eventTypeFilter"), "Event")
    ),
    column(
      width = 2,
      uiOutput(ns("extraFilter"))
    ),
    column(
      width = 8,
      uiOutput(ns("customQuery"))
    )
  )
}
