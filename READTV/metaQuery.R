library(shiny)
library(shinyjs)

metaQueryUI = function(id){
  ns <- NS(id)
  uiOutput(ns("metaQuery"))
}


metaQueryServer = function(input, output, session, metaDataFile) {
  ns = session$ns
  
  output$metaQuery = renderUI({
    validate(need(metaDataFile(), F))
    mdf = metaDataFile()
    wellPanel(
      textInput(ns("queryInput"), "Case Filter", placeholder = ""),
      actionButton(ns("queryInclude"), "Include Condition"),
      uiOutput(ns("metaQueryApplyOutput")),
      fluidRow(
        column(actionButton(ns("metaPreview"), "Preview Cases"), width = 2),
        column(renderText(paste("Meta Data File: ", mdf)),
               width = 8)
      )
    )
  })
  
  output$metaQueryApplyOutput = renderUI({
    validate(need(input$queryInput, F))
    
    qcc = column(actionButton(ns("queryClear"), "Clear Case Filter"),
                 width = 2)
    qac = column(actionButton(ns("querySubmit"), "Apply Case Filter"),
                 width = 2)
    if(input$queryInput != "")
      fluidRow(
        qcc
      )
    else
      NULL
  })
  
  observe({
    qc = queryCompiles()
    hqi = hasQueryInput()
    if(!qc & hqi) {
      shinyjs::addClass("queryInput", "invalid_query",
                        !queryCompiles())
      shinyjs::disable("metaPreview")
    } else {
      shinyjs::removeClass("queryInput", "invalid_query")
      shinyjs::enable("metaPreview")
    }
  })
  
  observeEvent(input$metaPreview, {
    meta_data = metaData()
    if(queryCompiles()) 
      meta_data = filteredMetaData()
    
    showModal(modalDialog(
      title = "Cases",
      renderDataTable(meta_data),
      easyClose = TRUE,
      size = "m"
    ))
  })
  
  observe({
    req(metaData())
    
    fmd = filteredMetaData()
    if(is.null(fmd)) fmd = metaData()
    updateActionButton(session, "metaPreview", 
                       paste("Preview", nrow(fmd),  "Cases"))
  })
  
  metaData <- reactive({
    req(metaDataFile())
    
    mdf = metaDataFile()
    read.csv(mdf, header = T, stringsAsFactors = F)
  })
  
  observeEvent(input$queryClear, {
    updateTextInput(session, "queryInput", value = "")
  })
  
  observeEvent(input$queryInclude, {
    meta_data = metaData()
    
    fieldClass = reactive({
      class(meta_data[, input$metaField])
    })
    
    output$fieldOptions = renderUI({
      req(input$metaField)
      
      filter_choices = c(">", ">=", "<", "<=", "==", "!=")
      choices = unique(meta_data[,input$metaField])
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
      selectizeInput("appendQueryOption", "How To Include",
                     choices = c("&", "|"))
    })
    
    showModal(modalDialog(
      title = "Condition",
      footer = fluidRow(
        actionButton(ns("modalSubmit"), "Include"),
        modalButton("Cancel")
      ),
      selectInput(ns("metaField"), "Field", 
                  choices = colnames(meta_data)),
      uiOutput(ns("fieldOptions")),
      uiOutput(ns("appendQueryOptions"))
    ))
    
    observeEvent(input$modalSubmit, {
      
      field_value = input$fieldValue
      if(fieldClass() == "character")
        field_value = paste0("'", field_value, "'")
      
      new_qry = paste(input$metaField, input$fieldFilter, field_value)
      
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
    fmd = try(filterMetaData(input$queryInput), silent = T)
    
    return(!(class(fmd) == "try-error"))
  })
  
  hasQueryInput = reactive({
    if(is.null(input$queryInput)) return(F)
    if(input$queryInput == "") return(F)
    
    T
  })
  
  filteredMetaData = reactive({
    if(queryCompiles()) filterMetaData(input$queryInput)
    else NULL
  })
  
  filterMetaData = function(qry)
    try(
      qry %>%
        {paste0('metaData() %>% filter(', ., ')')} %>%
        {parse(text = .)} %>%
        eval, 
      silent = T)
  
  observeEvent(input$querySubmit, {
    serverState$meta_cases = filterMetaData(input$queryInput)$Case
  })
  
  return(reactive({
    if(queryCompiles()) filteredMetaData()
    else metaData()
  }))
}