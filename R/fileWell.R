

fileWellUI = function(id) {
  ns = NS(id)
  uiOutput(ns("loadData"))
}


fileWellServer = function(input, output, session, fileType, filepath = NULL){
  ns = session$ns
  f = stringr::str_interp

  isMinimized = reactiveVal(F)

  isFilePassed = reactive({
    !is_empty(filepath)
  })

  fileinfo = reactive({
    if(isFilePassed())
      list(name = basename(filepath), datapath = filepath)
    else
      input$loadF
  })

  fileLoaded = reactive({
    return(!is.null(fileinfo))
  })

  output$loadData = renderUI({
    if(!isFilePassed()) {
      label = paste("Load", fileType)
      wellPanel(
        uiOutput(ns("minimize")),
        fileInput(ns("loadF"), label, accept = c('.csv', '.rds', '.RDS', '.CSV')),
        uiOutput(ns("filename")))
    }
    else
      column(wellPanel(textOutput(ns("loadMessage"))), width = 12)
  })

  output$loadMessage = renderText({
    f("Passed ${filepath} for input data")
  })

  minimizeLabel = reactive({
    if(isMinimized())
      if_else(fileLoaded(),
              paste("Load New", fileType),
              paste("Load", fileType)
              )
    else
      "Minimize"
  })

  output$filename = renderText({
    req(isMinimized())
    req(fileinfo())

    fileinfo()$name
  })

  output$minimize = renderUI({
    actionButton(inputId = ns("minimize"), label = minimizeLabel())
  })

  observeEvent(input$minimize, {
    isMinimized(!isMinimized())})

  observe({
    if(isMinimized())
      shinyjs::hide("loadF")
    else
      shinyjs::show("loadF")
  })

  return(reactive({
    req(fileinfo())

    fileinfo()
  }))
}
