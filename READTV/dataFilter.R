

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
    
    if(isSelected(ca)) d = d %>% filter(Case == ca)
    if(isSelected(ph)) d = d %>% filter(Phase == ph)
    if(isSelected(fd)) d = d %>% filter(FD.Type %in% fd)
    
    d
  })
  
  case <- callModule(multiSelectServer, "caseSelect")
  phase <- callModule(multiSelectServer, "phaseSelect")
  flowDisruption <- callModule(multiSelectServer, "fdSelect")
  
  
  observe({
    cases = data()$Case %>% unique
    case$choices(cases)
  })
  
  observe({
    ca = case$selected()
    d = data()
    
    if(isSelected(ca)) phases = d[d$Case== ca,]$Phase %>% unique
    else phases = unique(d$Phase)
    
    phase$choices(phases)
  })
  
  
  observe({
    ca = case$selected()
    ph = phase$selected()
    d = data()
    
    if(isSelected(ca)) d = d %>% filter(Case == ca)
    if(isSelected(ph)) d = d %>% filter(Phase == ph)
    fds = d$FD.Type %>% unique %>% as.character
    
    flowDisruption$choices(fds)
  })
  
  return(filteredData)
}

