library(shiny)
library(shinyjs)

metaQueryUI = function(id){
  ns <- NS(id)
  uiOutput(ns("metaQuery"))
}


metaQueryServer = function(input, output, session, metaDataFile) {
  ns = session$ns
  
  serverState = reactiveValues(is_minimized = FALSE, query_text = NULL)
  
  output$metaQuery = renderUI({
    validate(need(metaDataFile(), F))
    mdf = metaDataFile()
    div(
      actionButton(ns("minimize"), "Minimize"),#,
      #style = "position:relative;left:60em;top:3em"),
    uiOutput(ns("queryInput")),
    uiOutput(ns("queryText")),
    uiOutput(ns("queryInclude")),
    uiOutput(ns("metaQueryApplyOutput")),
    fluidRow(
      column(actionButton(ns("metaPreview"), "Preview Cases"), width = 2),
      column(uiOutput(ns("metaDataFile")), width = 8)
    ))
  })
  
  output$queryInput = renderUI({
    req(!serverState$is_minimized)
    
    if(!is.null(serverState$query_text))
      value = serverState$query_text
    else
      value = ""
    textInput(ns("queryInput"), "Case Filter", placeholder = "", value = value)
  })
  
  output$queryText = renderText({
    req(serverState$is_minimized)
    
    serverState$query_text = input$queryInput
    serverState$query_text
  })
  
  output$metaQueryApplyOutput = renderUI({
    validate(need(input$queryInput, F))
    req(!serverState$is_minimized)
    
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
  
  output$queryInclude = renderUI({
    req(!serverState$is_minimized)
    
    actionButton(ns("queryInclude"), "Include Condition")
  })
  
  output$metaDataFile = renderText({
    req(!serverState$is_minimized)
      
    mdf = metaDataFile()
    paste("Meta Data File: ", mdf$name)
  })
  
  observeEvent(input$minimize, {
    serverState$is_minimized = !serverState$is_minimized
    label = ifelse(serverState$is_minimized, 
                   "Edit Meta Filter", "Minimize")
    updateActionButton(session, "minimize", label)
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
    
    mdf = metaDataFile()$datapath
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
  
  return(reactive({
    qry = input$queryInput
    
    if(queryCompiles()) data = filteredMetaData()
    else data = metaData()
    
    return(list(query = qry, data = data))
  }))
}
