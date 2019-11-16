

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
      multiSelectUI(ns("fdSelect"), "FD")
      )
  )
}


dataFilterServer = function(input, output, session, data) {
  ns = session$ns
  
  filteredData <- reactive({
    
    d = data()
    ca = case$selected()
    ph = phase$selected()
    fd = flowDisruption$selected()
    
    if(isSelected(ca)) d = d %>% filter(Case %in% ca)
    if(isSelected(ph)) d = d %>% filter(Phase %in% ph)
    if(isSelected(fd)) d = d %>% filter(FD.Type %in% fd)
    
    d
  })
  
  case <- callModule(multiSelectServer, "caseSelect", data, 'Case')
  phase <- callModule(multiSelectServer, "phaseSelect", data, 'Phase',
                      parents = reactiveValues(Case = case))
  flowDisruption <- callModule(multiSelectServer, "fdSelect", data, "FD.Type",
                               parents = reactiveValues(Case = case, Phase = phase))
  
  
  return(filteredData)
}

