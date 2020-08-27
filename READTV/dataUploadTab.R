dataUploadTabUI = function(id) {
  ns = NS(id)
  div(
    shinythemes::themeSelector(),
    actionButton(ns("minimizeHeader"), "Minimize"),
    uiOutput(ns("headerInformation")),
    div(id = ns("loadDataHeader"),
        fluidRow(
          eventsLoaderUI(ns("loadData"))),
        fluidRow(
          metaQueryLoaderUI(ns("loadMetaData")),
          metaQueryUI(ns("metaqueryui"))
        ),
        fluidRow(
          column(uiOutput(ns("sidePanel")),
                 width = 12)
        )
    )
  )
}


dataUploadTabServer = function(input, output, session,
                               output_sym = sym('data')) {
  ns = session$ns

  #------------Header--------
  isHeaderMinimized = reactiveVal(F)
  
  observeEvent(input$minimizeHeader, {
    isHeaderMinimized(!isHeaderMinimized())
  })
  
  output$headerInformation = renderText({
    if(isHeaderMinimized()) headerMinimalInformation()
  })
  
  headerMinimalInformation = reactive({
    parts = c()

    if(isDataLoaded())
      parts = append(parts, eventsInformation$name())
    
    if(isMetaDataLoaded()) {
      parts = append(parts, metaDataFile()$name)
      parts = append(parts, filteredMetaData()$query)
    }
    
    return(toString(parts))
  })
  
  observe({
    shinyjs::toggle("loadDataHeader", condition = !isHeaderMinimized())
    updateActionButton(session, "minimizeHeader",
                       label = ifelse(isHeaderMinimized(),
                                      "Show",
                                      "Minimize")
    )
  })
  
  #---Data--------
  eventsInformation = callModule(eventsLoader, "loadData",
                                 output_sym)
  isDataLoaded = reactiveVal(F)
  data <- reactive({
    req(eventsInformation$name())
    
    tbl = eventsInformation$data()
    if(isMetaDataLoaded()) {
      tbl = tbl %>% filter(Case %in% filteredMetaData()$data$Case)
    }
    isDataLoaded(T)
    tbl
  })
  
  
  #---Metadata------
  metaDataFile <- callModule(metaQueryLoader, "loadMetaData")
  filteredMetaData <- callModule(metaQueryServer, "metaqueryui", 
                                 metaDataFile)
  isMetaDataLoaded = reactive({
    fmd = try(filteredMetaData(), silent = T)
    
    return(!(class(fmd) == "try-error"))
  })
  
  
  #----Side Panel----
  output$sidePanel = renderUI({
    
    tabsetPanel(
      tabPanel("Source Code", 
               verbatimTextOutput(ns("sourceCodeSubTab")))
    )
  })
  
  
  #----Source Code----
  fullSourceString = reactive({
    req(eventsInformation$sourceString())
    
    expressionsToString(
      "#Load Data",
      eventsInformation$sourceString()
    )
  })
  
  output$sourceCodeSubTab = renderText({
    req(fullSourceString())
    
    fullSourceString()
  }
  )
  
  #---Return----
  return(list(
    data = data,
    eventsInformation = eventsInformation,
    fileName = eventsInformation$name,
    headerMinimalInformation = headerMinimalInformation,
    isDataLoaded = isDataLoaded,
    fullSourceString = fullSourceString
  ))
}
