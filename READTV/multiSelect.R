

multiSelectUI = function(id, title) {
  ns = NS(id)
  selectInput(ns("select"), paste("Select", title), selectableChoices(c()),
              multiple = T, selected = "All")
}


multiSelectServer = function(input, output, session, data, col) {
  ns = session$ns
  
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

  
  choices = reactive({
    req(data())
    
    d = data()
    chs = d[[col]] %>% unique
    
    if(class(chs) == "factor")
      chs = as.character(chs)
    
    return(chs)
  })
  
  filteredData = reactive({
    req(data())
    
    d = data()
    
    val = selected()
    if(isSelected(val)) d = d %>% filter(.data[[col]] %in% val)
    
    d
  })
  
  return(reactive({
    list(selected = selected, filteredData = filteredData)
  }))
}