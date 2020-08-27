
dataFilterServer = function(input, output, session, data,
                            in_pronoun, selected_pronoun,
                            out_pronoun) {
  ns = session$ns
  
  selectMods <- reactiveValues()
  
  constraints = reactiveValues()
  
  filteredDataCount = printWithCountGen("filtered data")

  #----Selected----
  selectedVals <- reactive({
    cols = c('Case', 'Event.Type')

    ret = lapply(cols, 
	   function(col) selectMods[[col]]()$selected())
    names(ret) = cols
    
    ret
  })
  
  selectedQuery = reactive({
    req(data())
    
    selected_vals = selectedVals()
    generateSelectedQuery(in_pronoun,
                           selected_pronoun,
                           selected_vals)
  })
  
  createDataMask = function(in_data){
    mask = list()
    mask[[as.character(in_pronoun)]] = in_data
    mask
  }

  selectedData <- reactive({
    req(data())
    
    d = data()
    
    #filteredDataCount()

    qry = selectedQuery()
    df = eval_tidy(qry, data = createDataMask(d))
    
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
	  	  qry = generateSelectedQuery(
	  	    in_pronoun, out_pronoun,
	  	    selected_vals[others])
	  	  
	  	  df = eval_tidy(qry, data = createDataMask(d))

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
	  req(data())

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
  

  #----Custom Query----
  customQuery = callModule(customEventsQueryServer, "customQuery", 
                           selectedData, selected_pronoun,
                           out_pronoun)

  filteredData = reactive({
	  hvq = customQuery$hasValidQuery()
	  if(hvq)
		  customQuery$filteredData()
	  else
		  selectedData()
  })

  
  #----Return----
  return(list(filteredData = filteredData,
	      hasValidQuery = customQuery$hasValidQuery,
	      hasQueryInput = customQuery$hasQueryInput,
	      query = customQuery$query,
	      selectedVals = selectedVals,
	      selectedQuery = selectedQuery,
	      filteredQuery = customQuery$filterQuery,
	      constraints = constraints))
}

