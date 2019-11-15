

dataFilterUI = function(id) {
  ns = NS(id)
  selectRows(ns)
}


selectRows <- function(ns) {
  fluidRow(
    column(
      width = 2,
      selectInput(ns("caseSelect"), "Select Case", selectableChoices(c()))),
    column(
      width = 2,
      selectInput(ns("phaseSelect"), "Select Phase", selectableChoices(c()))),
    column(
      width = 2,
      selectInput(ns("fdSelect"), "Select FD Type", selectableChoices(c()),
                  multiple = TRUE))
  )
}


dataFilterServer = function(input, output, session, data) {
  filteredData <- reactive({
    
    d = data()
    ca = case()
    ph = phase()
    fd = flowDisruption()
    
    if(isSelected(ca)) d = d %>% filter(Case == ca)
    if(isSelected(ph)) d = d %>% filter(Phase == ph)
    if(isSelected(fd)) d = d %>% filter(FD.Type %in% fd)
    
    d
  })
  
  case <- eventReactive(input$caseSelect, {input$caseSelect})
  
  observe({
    cases = data()$Case %>% unique
    updateSelectInput(session, "caseSelect", 
                      choices = selectableChoices(cases)
    )
  })
  
  phase <- eventReactive(input$phaseSelect, {input$phaseSelect})
  
  observe({
    req(input$caseSelect)
    
    ca = input$caseSelect
    d = data()
    
    if(isSelected(ca)) phases = d[d$Case== ca,]$Phase %>% unique
    else phases = unique(d$Phase)
    
    updateSelectInput(session, "phaseSelect", 
                      choices = selectableChoices(phases),
                      selected = "All"
    )
  })
  
  
  observe({
    ca = case()
    ph = phase()
    d = data()
    
    if(isSelected(ca)) d = d %>% filter(Case == ca)
    if(isSelected(ph)) d = d %>% filter(Phase == ph)
    fds = d$FD.Type %>% unique %>% as.character
    
    updateSelectInput(session, "fdSelect", 
                      choices = selectableChoices(fds),
                      selected = "All")
  })
  
  flowDisruption = eventReactive(input$fdSelect, {input$fdSelect})
  
  observeEvent(input$fdSelect, {
    fd = flowDisruption()
    has_all = 'All' %in% fd
    only_all = length(fd) == 1
    
    if(has_all & !only_all) {
      was_all = fd[1] == 'All'
      if(was_all) 
        updateSelectInput(session, "fdSelect", 
                          selected = fd[-1])
      else
        updateSelectInput(session, "fdSelect",
                          selected = 'All')
    }
  })
  
  return(filteredData)
}

