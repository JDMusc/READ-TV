

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
      #multiSelectUI(ns("phaseSelect"), "Phase")
      uiOutput(ns("extraColumn"))
      ),
    column(
      width = 8,
      textInput(ns("queryInput"), "Event Filter", placeholder = "")
      )
  )
}


dataFilterServer = function(input, output, session, data) {
  ns = session$ns
  
  filteredData <- reactive({
    req(extraColumn())
    ec = extraColumn()
    
    ec()$filteredData()
  })
  
  case <- callModule(multiSelectServer, "caseSelect", data, 'Case')
  eventType <- callModule(multiSelectServer, "eventSelect", case()$filteredData, 
                          "Event.Type")
  #phase <- callModule(multiSelectServer, "phaseSelect", eventType()$filteredData, 'Phase')
  
  extraColumnName = reactive({
    req(data())
    
    colnames(data())[3]
  })
  
  output$extraColumn <- renderUI({
    req(extraColumnName())
    
    multiSelectUI(ns("extraColumn"), extraColumnName())
  })
  
  extraColumn = reactive({
    callModule(multiSelectServer, "extraColumn", 
                              eventType()$filteredData, extraColumnName())
  })
  
  
  return(filteredData)
}

