customEventsQueryUI <- function(id) {
  ns = NS(id)
  renderUI(
    {
      div(
        fluidRow(
          column(textInput(ns("queryInput"), "Custom Filter", placeholder = "",
                           width = '100%'),
                 width = 8),
          column(uiOutput(ns("filterOut")), width = 4)
          ),
        actionButton(ns("queryInclude"), "Include Condition"),
        actionButton(ns("eventsView"), "View Data")
      )
    }
  )
}


customEventsQueryServer = function(input, output, session, data,
                                   in_pronoun, out_pronoun, filter_out_init = TRUE) {
  ns = session$ns

  observeEvent(input$queryInclude, {
    req(data())
    d = data()

    fieldClass = reactive({
      req(input$field)

      d %>% pull(input$field) %>% class
    })

    choices = reactive({
      req(input$field)

      d %>% pull(input$field) %>% unique %>% sort
    })

    is_type = function(type_fn)
      type_fn(d[[input$field]])

    output$fieldOptions = renderUI({
      req(input$field)

      fn_choices = exprs(`==`, `!=`) %>% name_expressions
      if(is_type(is_character))
        fn_choices = append(
          list(detect = expr(str_detect),
               exclude = function(string, pattern)
                 expr(str_detect(!!enexpr(string), !!pattern, negate = TRUE))
               ),
          fn_choices)
      else if(!is_type(is_logical))
        fn_choices = append(exprs(`>`, `>=`, `<`, `<=`) %>%
                              name_expressions,
                            fn_choices)

      if(is_type(is_character))
        fieldValue = textInput(ns("fieldValue"), "")
      else
        fieldValue = selectizeInput(ns("fieldValue"), "", choices = choices())

      fluidRow(
        column(
          selectizeInput(ns("fieldFilter"), "",
                         choices = fn_choices),
          width = 4),
        column(
          fieldValue,
          width = 4)
      )
    })

    output$appendQueryOptions = renderUI({
      if(!hasQueryInput()) return(NULL)

      choices = exprs(`&`, `|`) %>% name_expressions
      selectizeInput(ns("appendQueryOption"), "How To Include",
                     choices = choices)
    })

    showModal(modalDialog(
      title = "Condition",
      footer = fluidRow(
        actionButton(ns("modalSubmit"), "Include"),
        modalButton("Cancel")
      ),
      easyClose = T,
      selectInput(ns("field"), "Field",
                  choices = colnames(data())),
      uiOutput(ns("fieldOptions")),
      uiOutput(ns("appendQueryOptions"))
    ))

    observeEvent(input$modalSubmit, {
      field_value = if(c(is_numeric, is_logical) %>%
                       purrr::map(is_type) %>%
                       any &
                       !is_type(is.timepoint))
        parse_expr(input$fieldValue)
      else
        input$fieldValue
      fn_name = parse_expr(input$fieldFilter)
      field = sym(input$field)

      if(is_call(fn_name))
        new_qry = eval_tidy_verbose(fn_name)(!!field, field_value)
      else
        new_qry = expr((!!fn_name)(!!field, !!field_value))

      if(hasQueryInput()) {
        join_fn = parse_expr(input$appendQueryOption)
        iqi = parse_expr(paste0('(', input$queryInput, ')')) #not elegant
        new_qry = expr((!!join_fn)(!!iqi, !!new_qry))
      }

      new_qry_txt = expr_text(new_qry)

      updateTextInput(session, "queryInput", value = new_qry_txt)
      removeModal()
    },
    ignoreInit = TRUE
    )
  })

  showSource = callModule(showSourceServer, 'showSource')
  observeEvent(input$eventsView, {
    req(doesQueryCompile())

    if(hasQueryInput())
      d = filteredData()
    else
      d = data()

    showSource(d)
  })


  filterQuery = reactive({
    rhs = filterQueryRhs()
    expr(!!out_pronoun <- !!rhs)
  })


  filterQueryRhs = reactive({
    qry_ex = if(hasQueryInput()) parse_expr(input$queryInput) else expr("")
    mappedExpr(in_pronoun, qry_ex, filterOut())
  })

  filterQuerySafe = reactive({
    expr(!!out_pronoun <- !!(filterQueryRhsSafe()))
  })

  filterQueryRhsSafe = reactive({
    if(doesQueryCompile()) filterQueryRhs()
    else in_pronoun
  })

  doesQueryCompile = reactive({
    if(hasQueryInput())
      data() %>%
        list %>%
        set_names(expr_text(in_pronoun)) %>%
        doesEvalCompile(filterQueryRhs(), .)
    else
      TRUE
  })


  observe({
    if(hasValidQuery() | !hasQueryInput()) {
      shinyjs::removeClass("queryInput", "invalid_query")
      shinyjs::enable("eventsView")
    } else {
      shinyjs::addClass("queryInput", "invalid_query")
      shinyjs::disable("eventsView")
    }
  })

  hasQueryInput = reactive({
    if(is.null(input$queryInput)) return(FALSE)
    if(input$queryInput == "") return(FALSE)

    TRUE
  })

  hasValidQuery = reactive({
    hasQueryInput() & doesQueryCompile()
  })

  validQuery = reactive({
    req(hasValidQuery())

    input$queryInput
  })

  filteredData = reactive({
    data() %>%
      list %>%
      set_names(c(expr_text(in_pronoun))) %>%
      eval_tidy_verbose(filterQueryRhsSafe(), .)
  })

  rm = 'remove'
  tr = 'transparent'
  output$filterOut = renderUI({
    selected = if(filter_out_init) rm else tr

    selectInput(ns("filterOutSelect"), "Custom Filtered Data",
                       choices = c(rm, tr), selected = selected)
  })

  filterOut = reactive({
    if(is.null(input$filterOutSelect))
      filter_out_init
    else
      input$filterOutSelect == rm
  })

  list(
    hasQueryInput = hasQueryInput,
    hasValidQuery = hasValidQuery,
    filterQuery = filterQuerySafe,
    filterOut = filterOut)

}
