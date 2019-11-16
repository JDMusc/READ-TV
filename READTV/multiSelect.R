

multiSelectUI = function(id, title) {
  ns = NS(id)
  selectInput(ns("select"), paste("Select", title), selectableChoices(c()),
              multiple = T, selected = "All")
}


multiSelectServer = function(input, output, session, 
                             data = NULL, col = NULL, parents = NULL) {
  ns = session$ns
  
  #choices = reactiveVal()
  
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
    req(!is.null(data))
    
    d = data()
    if(!is.null(parents)) {
      parent_cols = names(parents)
      for(i in 1:length(parent_cols)){
        parent_col = parent_cols[i]
        val = parents[[parent_col]]$selected()
      
        if(isSelected(val)) d = d %>% filter(.data[[parent_col]] %in% val)
      }
    }
    
    chs = d[[col]] %>% unique
    
    if(class(d[[col]]) == "factor")
      chs = as.character(chs)
    
    return(chs)
  })
  
  return(list(choices = choices, selected = selected))
}