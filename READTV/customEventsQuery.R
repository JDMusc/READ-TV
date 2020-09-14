customEventsQueryUI <- function(id) {
  ns = NS(id)
  renderUI(
    {
      div(
        textInput(ns("queryInput"), "Custom Filter", placeholder = "", 
                  width = '100%'),
        actionButton(ns("queryInclude"), "Include Condition"),
        actionButton(ns("eventsPreview"), "Preview Events")
      )
    }
  )
}


customEventsQueryServer = function(input, output, session, data,
                                   in_pronoun, out_pronoun) {
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
        new_qry = eval_tidy(fn_name)(!!field, field_value)
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
  observeEvent(input$eventsPreview, {
    req(queryCompiles())
    
    if(hasQueryInput()) 
      d = filteredData()
    else
      d = data()
    
    showSource(d)
  })
  
  
  filterQuery = reactive({
    if(hasValidQuery())
      expr(!!out_pronoun <- !!in_pronoun %>% 
             filter(!!parse_expr(input$queryInput)))
    else
      expr(!!out_pronoun <- !!in_pronoun)
  })
  
  queryCompiles = reactive({
    if(hasQueryInput()) 
      doesFilterCompile(input$queryInput, data())
    else
      TRUE
  })
  
  
  observe({
    if(hasValidQuery() | !hasQueryInput()) {
      shinyjs::removeClass("queryInput", "invalid_query")
      shinyjs::enable("eventsPreview")
    } else {
      shinyjs::addClass("queryInput", "invalid_query")
      shinyjs::disable("eventsPreview")
    }
  })
  
  hasQueryInput = reactive({
    if(is.null(input$queryInput)) return(FALSE)
    if(input$queryInput == "") return(FALSE)
    
    TRUE
  })

  hasValidQuery = reactive({
    hasQueryInput() & queryCompiles()
  })

  validQuery = reactive({
    req(hasValidQuery())

    input$queryInput
  })
  
  filteredData = reactive({
    req(hasValidQuery())
    
    applyQuery(validQuery(), data())
  })
  
  return(list(filteredData = filteredData, 
              hasQueryInput = hasQueryInput,
              hasValidQuery = hasValidQuery,
              filterQuery = filterQuery,
              query = validQuery))

}
