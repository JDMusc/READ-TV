

dataFilterServer = function(input, output, session, data) {
  ns = session$ns
  
  selectMods <- reactiveValues()
  
  selectedQuery = function(selected_values) {
    selected_ixs = selected_values %>% names %>%
      sapply(function(n) !('All' %in% selected_values[[n]]))
    if(sum(selected_ixs) == 0)
      return("")
    
    selected_values = selected_values[selected_ixs]
    
    selected_values %>% 
      names %>% 
      sapply(function(n) {
        values = selected_values[[n]]
        is_char = class(values) == 'character'
        
        values_str = values %>% sapply(function(v) 
          if(is_char)  paste0("'", v, "'") else v) %>%
          paste(collapse = ',')
        
        paste(n, '%in%', paste0('c(', values_str, ')'))
        }) %>%
      paste(collapse = '&')
  }
  
  filteredDataCount = printWithCountGen("filtered data")
  filteredData <- reactive({
    req(data())
    
    d = data()
    
    filteredDataCount()
    
    if('Case' %in% names(selectMods)) {
      cols = c('Case', 'Event.Type', extraFilterName())
      for(col in cols){
        m = selectMods[[col]]()
        req(m$selected())
        val = m$selected()
        
        if(isSelected(val)) d = d %>% filter(.data[[col]] %in% val)
      }
    }
    
    d
  })
  
  updateChoicesCountDebug = printWithCountGen('update choices')
  observe({
    req('Case' %in% names(selectMods))
    
    d = data()
    cols = c('Case', 'Event.Type', extraFilterName())
    selected_vals = sapply(cols, 
                           function(col) selectMods[[col]]()$selected())
    for(col in cols) {
      qry = selectedQuery(selected_vals[setdiff(cols, col)])
      
      df = filterData(qry, 'data')
      
      chs = columnValues(df, col)
      
      if(length(chs) == 1) browser()
      
      req(selectMods[[col]]())
      
      selectMods[[col]]()$updateChoices(chs)
      }
  })
  
  
  columnValues = function(df, col) {
    chs = df[[col]] %>% unique
    
    if(class(chs) == "factor")
      as.character(chs)
    else
      chs
  }
  
  
  observe({
    req(extraFilterName())
    
    d = data()
    
    selectMods[["Case"]] = callModule(multiSelectServer, "caseFilter",
                                      columnValues(d, "Case"))
    selectMods[["Event.Type"]] = callModule(multiSelectServer, "eventTypeFilter",
                                            columnValues(d, "Event.Type"))
    efn = extraFilterName()
    selectMods[[efn]] = callModule(multiSelectServer, "extraFilter", 
                                   columnValues(d, efn))
  })
  
  
  extraFilterName = reactive({
    req(data())
    
    colnames(data())[3]
  })
  
  
  output$extraFilter <- renderUI({
    req(extraFilterName())
    
    multiSelectUI(ns("extraFilter"), extraFilterName())
  })
    
  
  output$customQuery = renderUI({
    div(
      textInput(ns("queryInput"), "Custom Filter", placeholder = ""),
      actionButton(ns("queryInclude"), "Include Condition")
    )
  })
  
  observeEvent(input$queryInclude, {
    req(filteredData())
    fd = filteredData()
    
    fieldClass = reactive({
      class(fd[, input$field])
    })
    
    output$fieldOptions = renderUI({
      req(input$field)
      
      filter_choices = c(">", ">=", "<", "<=", "==", "!=")
      choices = unique(fd[,input$field])
      if(fieldClass() %in% c("character", "logical"))
        filter_choices = c("==", "!=")
      else
        choices = sort(choices)
      
      fluidRow(
        column(
          selectizeInput(ns("fieldFilter"), "",
                         choices = filter_choices),
          width = 2),
        column(
          selectizeInput(ns("fieldValue"), "",
                         choices = choices),
          width = 2)
      )
    })
    
    output$appendQueryOptions = renderUI({
      if(!hasQueryInput()) return(NULL)
      selectizeInput(ns("appendQueryOption"), "How To Include",
                     choices = c("&", "|"))
    })
    
    showModal(modalDialog(
      title = "Condition",
      footer = fluidRow(
        actionButton(ns("modalSubmit"), "Include"),
        modalButton("Cancel")
      ),
      easyClose = T,
      selectInput(ns("field"), "Field", 
                  choices = colnames(fd)),
      uiOutput(ns("fieldOptions")),
      uiOutput(ns("appendQueryOptions"))
    ))
    
    observeEvent(input$modalSubmit, {
      
      field_value = input$fieldValue
      if(fieldClass() == "character")
        field_value = paste0("'", field_value, "'")
      
      new_qry = paste(input$field, input$fieldFilter, field_value)
      
      if(hasQueryInput()) {
        new_qry = paste(input$queryInput, input$appendQueryOption,
                        new_qry)
      }
      
      updateTextInput(session, "queryInput", value = new_qry)
      removeModal()
    },
    ignoreInit = TRUE
    )
  })
  
  queryCompiles = reactive({
    fmd = try(filterData(input$queryInput), silent = T)
    
    return(!(class(fmd) == "try-error"))
  })
  
  hasQueryInput = reactive({
    if(is.null(input$queryInput)) return(F)
    if(input$queryInput == "") return(F)
    
    T
  })
  
  filterData = function(qry, data_str = 'filteredData')
    try(
      qry %>%
        {paste0(data_str, '() %>% filter(', ., ')')} %>%
        {parse(text = .)} %>%
        eval, 
      silent = T)
  
  customFilteredData = reactive({
    req(hasQueryInput())
    req(queryCompiles())
    
    return(filterData(input$queryInput))
  })
  
  return(reactive({
    if(hasQueryInput() & queryCompiles())
      customFilteredData()
    else
      filteredData()
  }))
}

