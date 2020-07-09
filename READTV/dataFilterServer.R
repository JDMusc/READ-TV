
dataFilterServer = function(input, output, session, data) {
  ns = session$ns
  
  selectMods <- reactiveValues()
  
  constraints = reactiveValues()
  
  filteredDataCount = printWithCountGen("filtered data")

  selectedVals <- reactive({
    cols = c('Case', 'Event.Type')

    sapply(cols, 
	   function(col) selectMods[[col]]()$selected())
  })

  selectedData <- reactive({
    req(data())
    
    d = data()
    
    #filteredDataCount()

    selected_vals = selectedVals()
    qry = generateSelectedQuery(selected_vals)
    df = applyQuery(qry, data())
    
    #if('Case' %in% names(selectMods)) {
    #  cols = c('Case', 'Event.Type', extraFilterName())
    #  for(col in cols){
    #    m = selectMods[[col]]()
    #    req(m$selected())
    #    val = m$selected()
    #    
    #    if(isSelected(val)) d = d %>% filter(.data[[col]] %in% val)
    #  }
    #}
    df
    
    for(col in names(constraints)) {
      fn = constraints[[col]]
      passing_rows = fn(df[[col]])
      df = df[passing_rows,]
    }
    
    df
  })

  updateChoicesCountDebug = printWithCountGen('update choices')
  observe({
	  req('Case' %in% names(selectMods))

	  d = data()
	  cols = c('Case', 'Event.Type')
	  selected_vals = selectedVals()

	  for(col in cols) {
		  others = setdiff(cols, col)
	  	  qry = generateSelectedQuery(selected_vals[others])
	  	  df = applyQuery(qry, d)

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
  })


  extraFilterName = reactive({
	  req(data())

	  colnames(data())[3]
  })


  output$extraFilter <- renderUI({
	  req(extraFilterName())

	  multiSelectUI(ns("extraFilter"), extraFilterName())
  })

  customQuery = callModule(customEventsQueryServer, "customQuery", selectedData)

  filteredData = reactive({
	  hvq = customQuery$hasValidQuery()
	  if(hvq)
		  customQuery$filteredData()
	  else
		  selectedData()
  })

  return(list(filteredData = filteredData,
	      hasValidQuery = customQuery$hasValidQuery,
	      hasQueryInput = customQuery$hasQueryInput,
	      query = customQuery$query,
	      selectedVals = selectedVals,
	      constraints = constraints))
}

