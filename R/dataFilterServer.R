
dataFilterServer = function(input, output, session, data,
                            in_pronoun, selected_pronoun,
                            out_pronoun,
                            filter_out_init) {
  ns = session$ns
  f = stringr::str_interp

  location = function(msg) f('dataFilterServer, ${msg}')
  log_trace_dfs = log_info_module_gen('dataFilterServer')
  log_trace_pre = function(fn) log_trace_dfs(f('${fn}, pre-req'))
  log_trace_post = function(fn) log_trace_dfs(f('${fn}, post-req'))
  req_log = req_log_gen(log_trace_dfs)

  selectMods <- reactiveValues()

  constraints = reactiveValues()

  filteredDataCount = printWithCountGen("filtered data")

  #----Selected----
  hasCol = function(col) col %in% colnames(data())
  selectableCols = reactive({
    req_log('selectableCols', quo(data()))


    intersect(
      c('Case', 'Event.Type'),
      colnames(data())
    )
  })

  output$selectRows = renderUI({
    req_log('output select rows', quo(data()))

    fluidRow(
      if(hasCol('Case')) {
        column(
          width = 2,
          multiSelectUI(ns("caseFilter"), "Case")
          )
        }
      ,
      if(hasCol('Event.Type')) {
        column(
          width = 2,
          multiSelectUI(ns("eventTypeFilter"), "Event Type")
          )
        },
      column(
        width = 8,
        customEventsQueryUI(ns("customQuery"))
      )
    )
  })

  selectedVals <- reactive({
    log_trace_dfs('selectedVals')

    cols = names(selectMods)

    ret = lapply(cols,
	   function(col) selectMods[[col]]()$selected())
    names(ret) = cols

    ret
  })

  selectedQuery = reactive({
    req_log('selectedQuery', quo(data()))

    rhs = selectedQueryRhs()
    expr(!!selected_pronoun <- !!rhs)
  })

  selectedQueryRhs = reactive({
    req_log('selectedQueryRhs', quo(data()))

    generateSelectedQueryRhs(in_pronoun, selectedVals())
  })

  createDataMask = function(in_data){
    log_trace_dfs('createDataMask')

    in_data %>%
      list %>%
      set_names(expr_text(in_pronoun))
  }

  selectedData <- reactive({
    req_log('selectedData', quo(data()))

    qry = selectedQuery()
    df = eval_tidy_verbose(qry, data = createDataMask(data()),
                           location = location('selectedData'))

    for(col in names(constraints)) {
      fn = constraints[[col]]
      passing_rows = fn(df[[col]])
      df = df[passing_rows,]
    }

    df
  })

  updateChoicesCountDebug = printWithCountGen('update choices')
  observe({
    req_log('observe Data', quo(data()))

	  d = data()
	  cols = selectableCols()
	  selected_vals = selectedVals()

	  for(col in cols) {
		  others = setdiff(cols, col)
	  	  qry = generateSelectedQuery(
	  	    in_pronoun, out_pronoun,
	  	    selected_vals[others])

	  	  df = eval_tidy_verbose(qry, data = createDataMask(d),
	  	                 location = location('observe Data'))

		  chs = columnValues(df, col)

		  req(selectMods[[col]])
		  req(selectMods[[col]]())

		  selectMods[[col]]()$updateChoices(chs)
	  }
  })


  columnValues = function(df, col) {
    log_trace_dfs('columnValues')
    df %>%
      select(col) %>%
      dplyr::mutate_if(is.factor, as.character) %>%
      pull(col) %>%
      unique
  }


  observe({
    req_log('observe data 2', quo(data()))

	  d = data()

	  if(hasCol('Case'))
	    selectMods[["Case"]] = callModule(multiSelectServer, "caseFilter",
	                                      columnValues(d, "Case"))
	  if(hasCol('Event.Type'))
	    selectMods[["Event.Type"]] = callModule(multiSelectServer, "eventTypeFilter",
	                                            columnValues(d, "Event.Type"))
  })


  #----Custom Query----
  customQuery = callModule(customEventsQueryServer, "customQuery",
                           selectedData, selected_pronoun,
                           out_pronoun, filter_out_init = filter_out_init)

  filteredDataExprs = reactive({
    log_trace_dfs('filteredDataExprs')
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

