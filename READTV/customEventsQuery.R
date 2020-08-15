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
      class(d[, input$field])
    })
    
    output$fieldOptions = renderUI({
      req(input$field)
      
      filter_choices = c(">", ">=", "<", "<=", "==", "!=")
      choices = unique(d[,input$field])
      if(fieldClass() %in% c("character", "logical")) {
        filter_choices = c("==", "!=")
        if(fieldClass() == "character")
          filter_choices = append(c("grepl", "!grepl"), filter_choices)
      }
      else
        choices = sort(choices)
      
      fieldValue = selectizeInput(ns("fieldValue"), "", choices = choices)
      
      if(fieldClass() == "character")
        fieldValue = textInput(ns("fieldValue"), "")
      
      fluidRow(
        column(
          selectizeInput(ns("fieldFilter"), "",
                         choices = filter_choices),
          width = 2),
        column(
          fieldValue,
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
                  choices = colnames(data())),
      uiOutput(ns("fieldOptions")),
      uiOutput(ns("appendQueryOptions"))
    ))
    
    observeEvent(input$modalSubmit, {
      
      field_value = input$fieldValue
      if(fieldClass() == "character")
        field_value = paste0("'", field_value, "'")
      
      new_qry = paste(input$field, input$fieldFilter, field_value)
      
      if(input$fieldFilter %in% c("grepl", "!grepl"))
        new_qry = paste0(
          input$fieldFilter, "(", field_value, ", ", input$field, ")"
        )
      
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
    if(!queryCompiles() | !hasQueryInput())
      return(expr(!!sym(out_pronoun) <- !!sym(in_pronoun)))
    
    
    qry_expr = parse_expr(paste0('filter(', input$queryInput,' )'))
    expr(!!sym(out_pronoun) <- !!sym(in_pronoun) %>% !!qry_expr)
  })
  
  queryCompiles = reactive({
    if(!hasQueryInput())
      return(TRUE)
    
    doesQueryStringCompile(input$queryInput, data())
  })
  
  
  observe({
    hqi = hasQueryInput()
    
    qc = queryCompiles()
    
    valid_query = qc | !hqi
    
    if(valid_query) {
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

  query = reactive({
    req(hasQueryInput())
    req(queryCompiles())

    return(input$queryInput)
  })
  
  filteredData = reactive({
    req(hasQueryInput())
    
    return(applyQuery(query(), data()))
  })
  
  hasValidQuery = reactive({
    hasQueryInput() & queryCompiles()
  })
  
  return(list(filteredData = filteredData, 
              hasQueryInput = hasQueryInput,
              hasValidQuery = hasValidQuery,
              filterQuery = filterQuery,
              query = query))

}
