

dataFilterServer = function(input, output, session, data) {
  ns = session$ns
  
  selectMods <- reactiveValues()
  
  selectedQuery = function(selected_values) {
    selected_ixs = selected_values %>% names %>%
      sapply(function(n) !('All' %in% selected_values[[n]]))
    tryCatch({
      if(sum(selected_ixs) == 0)
        return("")
    }, error = function(e) browser)
    
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
  
  
  constraints = reactiveValues()
  
  
  filteredDataCount = printWithCountGen("filtered data")
  preQueryFilteredData <- reactive({
    req(data())
    
    d = data()
    
    #filteredDataCount()
    
    if('Case' %in% names(selectMods)) {
      cols = c('Case', 'Event.Type', extraFilterName())
      for(col in cols){
        m = selectMods[[col]]()
        req(m$selected())
        val = m$selected()
        
        if(isSelected(val)) d = d %>% filter(.data[[col]] %in% val)
      }
    }
    
    for(col in names(constraints)) {
      fn = constraints[[col]]
      passing_rows = fn(d[[col]])
      d = d[passing_rows,]
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
      
      df = applyQuery(qry, data())
      
      chs = columnValues(df, col)
      
      req(selectMods[[col]]())
      
      selectMods[[col]]()$updateChoices(chs)
      }
  })
  
  
  columnValues = function(df, col) {
    chs = tryCatch({df[[col]] %>% unique},
                   error = function(e) browser())
    
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
    #efn = extraFilterName()
    #browser()
    #selectMods[[efn]] = callModule(multiSelectServer, "extraFilter", 
    #                               columnValues(d, efn))
  })
  
  
  extraFilterName = reactive({
    req(data())
    
    colnames(data())[3]
  })
  
  
  output$extraFilter <- renderUI({
    req(extraFilterName())
    
    multiSelectUI(ns("extraFilter"), extraFilterName())
  })
  
  customQuery = callModule(customEventsQueryServer, "customQuery", preQueryFilteredData)
  
  filteredData = reactive({
    hvq = customQuery$hasValidQuery()
    if(hvq)
      customQuery$filteredData()
    else
      preQueryFilteredData()
  })
  
  return(list(filteredData = filteredData,
              hasValidQuery = customQuery$hasValidQuery,
              constraints = constraints))
}

