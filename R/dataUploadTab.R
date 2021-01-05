dataUploadTabUI = function(id) {
  ns = NS(id)
  div(
    shinythemes::themeSelector(),
    actionButton(ns("minimizeHeader"), "Minimize"),
    uiOutput(ns("headerInformation")),
    div(id = ns("loadDataHeader"),
        fluidRow(
          eventsLoaderUI(ns("loadData"))
        ),
        #fluidRow(
        #  metaQueryLoaderUI(ns("loadMetaData")),
        #  metaQueryUI(ns("metaqueryui"))
        #),
        fluidRow(
          column(uiOutput(ns("sidePanel")),
                 width = 12)
        )
    )
  )
}


dataUploadTabServer = function(input, output, session,
                               eventsPath = NULL,
                               inputData = NULL,
                               output_sym = sym('data')) {
  ns = session$ns
  f = stringr::str_interp

  location = function(msg) f('dataUploadTab, ${msg}')

  log_info_du = log_info_module_gen('dataUploadTab')

  #------------Header--------
  isHeaderMinimized = reactiveVal(F)

  observeEvent(input$minimizeHeader, {
    isHeaderMinimized(!isHeaderMinimized())
  })

  output$headerInformation = renderText({
    if(isHeaderMinimized()) headerMinimalInformation()
  })

  headerMinimalInformation = reactive({
    log_info_du('headerMinimalInformation')
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
  eventsInformation = callModule(eventsLoader, "loadData", output_sym,
                                 eventsPath, inputData)

  isDataLoaded = eventsInformation$isDataReady

  data = reactive({
    req(isDataLoaded())

    runExpressionsLast(eventsInformation$code(), eventsInformation$mask(),
                   location=location('data'))
  })


  #---Metadata------
  metaDataFile <- callModule(metaQueryLoader, "loadMetaData")
  filteredMetaData <- callModule(metaQueryServer, "metaqueryui",
                                 metaDataFile)
  isMetaDataLoaded = reactive({
    fmd = try(filteredMetaData(), silent = TRUE)

    return(!(class(fmd) == "try-error"))
  })


  #----Side Panel----
  output$sidePanel = renderUI({

    tabsetPanel(
      tabPanel("Source Code",
               verbatimTextOutput(ns("sourceCodeSubTab"))),
      tabPanel("Data Glimpse",
               dataGlimpseUI(ns("dataGlimpse")))
    )
  })

  #----Data Glimpse----
  dataGlimpse = callModule(dataGlimpseServer, 'dataGlimpse', data)

  #----Source Code----
  fullSourceString = reactive({
    req(eventsInformation$isDataReady())

    expressionsToString(
      eventsInformation$code()
    )
  })

  output$sourceCodeSubTab = renderText({
    req(fullSourceString())

    fullSourceString()
  }
  )

  #---Return----
  list(
    fileName = eventsInformation$name,
    isDataLoaded = isDataLoaded,
    code = eventsInformation$code,
    mask = eventsInformation$mask
  )
}
