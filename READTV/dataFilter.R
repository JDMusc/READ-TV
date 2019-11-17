

dataFilterUI = function(id) {
  ns = NS(id)
  selectRows(ns)
}


selectRows <- function(ns) {
  fluidRow(
    column(
      width = 2,
      multiSelectUI(ns("caseSelect"), "Case")
      ),
    column(
      width = 2,
      multiSelectUI(ns("eventSelect"), "Event")
    ),
    column(
      width = 2,
      multiSelectUI(ns("phaseSelect"), "Phase")
      )
  )
}


dataFilterServer = function(input, output, session, data) {
  ns = session$ns
  
  filteredData <- reactive({
    phase()$filteredData()
  })
  
  case <- callModule(multiSelectServer, "caseSelect", data, 'Case')
  eventType <- callModule(multiSelectServer, "eventSelect", case()$filteredData, 
                          "Event.Type")
  phase <- callModule(multiSelectServer, "phaseSelect", eventType()$filteredData, 'Phase')
  
  
  return(filteredData)
}

