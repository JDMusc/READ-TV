

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
      multiSelectUI(ns("phaseSelect"), "Phase")
      ),
    column(
      width = 2,
      multiSelectUI(ns("eventSelect"), "Event")
      )
  )
}


dataFilterServer = function(input, output, session, data) {
  ns = session$ns
  
  filteredData <- reactive({
    eventType()$filteredData()
  })
  
  case <- callModule(multiSelectServer, "caseSelect", data, 'Case')
  phase <- callModule(multiSelectServer, "phaseSelect", case()$filteredData, 'Phase')
  eventType <- callModule(multiSelectServer, "eventSelect", phase()$filteredData, 
                               "Event.Type")
  
  return(filteredData)
}

