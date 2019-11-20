customEventsQueryUI <- function(id) {
  ns = NS(id)
  renderUI(
    {
      div(
        textInput(ns("queryInput"), "Custom Filter", placeholder = ""),
        actionButton(ns("queryInclude"), "Include Condition")
      )
    }
  )
}


customEventsQueryServer = function(input, output, session, data) {
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
                  choices = colnames(data())),
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
    doesQueryCompile(input$queryInput, data())
  })
  
  hasQueryInput = reactive({
    if(is.null(input$queryInput)) return(F)
    if(input$queryInput == "") return(F)
    
    T
  })
  
  filteredData = reactive({
    req(hasQueryInput())
    req(queryCompiles())
    
    return(applyQuery(input$queryInput, data()))
  })
  
  hasValidQuery = reactive({
    hasQueryInput() & queryCompiles()
  })
  
  return(list(filteredData = filteredData, hasValidQuery = hasValidQuery))

}