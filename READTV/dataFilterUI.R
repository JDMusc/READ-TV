

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
      multiSelectUI(ns("eventTypeFilter"), "Event Type")
    ),
    column(
      width = 6,
      customEventsQueryUI(ns("customQuery"))
    )
  )
}
