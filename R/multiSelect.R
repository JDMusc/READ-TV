

multiSelectUI = function(id, title) {
  ns = NS(id)
  selectInput(ns("select"), paste("Select", title), selectableChoices(c()),
              multiple = T, selected = "All")
}


multiSelectServer = function(input, output, session, choices) {
  ns = session$ns
  
  selected = eventReactive(input$select,  {
    sd = ifAllOnlyAll(input$select)
    
    if(!all(sd == input$select)) {
      updateSelectInput(session, "select",
                        selected = sd)
    }
    
    sd
  })
  
  ifAllOnlyAll = function(selected_vals) {
    has_all = 'All' %in% selected_vals
    only_all = length(selected_vals) == 1
    
    if(has_all & !only_all) {
      was_all = selected_vals[1] == 'All'
      if(was_all) 
        selected_vals = selected_vals[-1]
      else
        selected_vals = "All"
    }
    
    return(selected_vals)
  }
  
  currentChoices = reactiveVal(selectableChoices(c()))
  
  updateChoices = function(choices) {
    s_choices = selectableChoices(sort(choices))
    
    sd = selected()
    new_choices_contain_selected = all(sd %in% s_choices)
    if(!new_choices_contain_selected)
      sd = 'All'
    
    new_choices_same = all(s_choices %in% currentChoices()) &
      all(currentChoices() %in% s_choices)
    
    if (!new_choices_same) {
      currentChoices(s_choices)
      updateSelectInput(session, 'select', choices = s_choices,
                        selected = sd)
    }
    
  }
  
  return(reactive({
    list(selected = selected, updateChoices = updateChoices)
  }))
}