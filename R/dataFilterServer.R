
dataFilterServer = function(input, output, session, data,
                            in_pronoun, selected_pronoun,
                            out_pronoun,
                            filter_out_init) {
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

    rhs = selectedQueryRhs()
    expr(!!selected_pronoun <- !!rhs)
  })

  selectedQueryRhs = reactive({
    req(data())

    generateSelectedQueryRhs(in_pronoun, selectedVals())
  })

  createDataMask = function(in_data){
    in_data %>%
      list %>%
      set_names(expr_text(in_pronoun))
  }

  selectedData <- reactive({
    req(data())

    qry = selectedQuery()
    df = eval_tidy(qry, data = createDataMask(data()))

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
    df %>%
      select(col) %>%
      dplyr::mutate_if(is.factor, as.character) %>%
      pull(col) %>%
      unique
  }


  observe({
	  req(data())

	  d = data()

	  selectMods[["Case"]] = callModule(multiSelectServer, "caseFilter",
					    columnValues(d, "Case"))
	  selectMods[["Event.Type"]] = callModule(multiSelectServer, "eventTypeFilter",
						  columnValues(d, "Event.Type"))
  })


  #----Custom Query----
  customQuery = callModule(customEventsQueryServer, "customQuery",
                           selectedData, selected_pronoun,
                           out_pronoun, filter_out_init = filter_out_init)

  filteredDataExprs = reactive({
    list(selectedQuery(), customQuery$filterQuery()) %>%
      set_expr_names(c(selected_pronoun, out_pronoun))
  })


  #----Return----
  list(filteredDataExprs = filteredDataExprs,
       hasValidQuery = customQuery$hasValidQuery,
       hasQueryInput = customQuery$hasQueryInput,
       filterOut = customQuery$filterOut,
       constraints = constraints)
}

