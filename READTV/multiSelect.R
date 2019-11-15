

multiSelectUI = function(id, title) {
  ns = NS(id)
  selectInput(ns("select"), paste("Select", title), selectableChoices(c()),
              multiple = T, selected = "All")
}


multiSelectServer = function(input, output, session) {
  ns = session$ns
  
  choices = reactiveVal()
  
  selected = eventReactive(input$select, {input$select})
  
  observeEvent(input$select, {
    sd = selected()
    has_all = 'All' %in% sd
    only_all = length(sd) == 1
    
    if(has_all & !only_all) {
      was_all = sd[1] == 'All'
      if(was_all) 
        sd = sd[-1]
      else
        sd = "All"
      
      updateSelectInput(session, "select",
                        selected = sd)
    }
  })
  
  observe({
    updateSelectInput(session, "select", 
                      choices = selectableChoices(choices()),
                      selected = "All")
  })
  
  return(list(choices = choices, selected = selected))
}