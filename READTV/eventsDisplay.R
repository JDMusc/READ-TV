

eventsDisplayUI <- function(id) {
  ns = NS(id)
  fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(list(.invalid_query = 'background-color: #f006')),
    tabsetPanel(
      tabPanel(
        "Data Upload",
        div(
          actionButton(ns("minimizeHeader"), "Minimize"),
          uiOutput(ns("headerInformation")),
          div(id = ns("loadDataHeader"),
              fluidRow(
                eventsLoaderUI(ns("loadData"))),
              fluidRow(
                metaQueryLoaderUI(ns("loadMetaData")),
                metaQueryUI(ns("metaqueryui"))
                )
          )
        )
      ),
      tabPanel(
        "Display",
        div(
          uiOutput(ns("dataFilter")),
          fluidRow(
            column(uiOutput(ns("eventPlotContainer")), width = 10),
            column(uiOutput(ns("sidePanel")), width = 2)
          )
        )
      )
    )
  )
}

eventsDisplayServer = function(input, output, session){
  ns = session$ns
  
  isHeaderMinimized = reactiveVal(F)
  isDataLoaded = reactiveVal(F)
  
  eventsInformation = callModule(eventsLoader, "loadData")

  
  data <- reactive({
    tbl = eventsInformation()$data
    if(isMetaDataLoaded()) {
      tbl = tbl %>% filter(Case %in% filteredMetaData()$data$Case)
    }
    isDataLoaded(T)
    tbl
  })
  
  metaDataFile <- callModule(metaQueryLoader, "loadMetaData")
  
  filteredMetaData <- callModule(metaQueryServer, "metaqueryui", 
                                 metaDataFile)
  
  isMetaDataLoaded = reactive({
    fmd = try(filteredMetaData(), silent = T)
    
    return(!(class(fmd) == "try-error"))
  })
  
  filteredData = callModule(dataFilterServer, "dataFilter", data)
  
  customizeDisplay = callModule(customizeDisplayServer, "customizeDisplay", 
                                filteredData)
  
  output$dataFilter = renderUI({
    if(isDataLoaded()) dataFilterUI(ns("dataFilter"))
  })
  
  hist <- reactive({
    req(isDataLoaded())
    
    ggplot(filteredData(), aes(x = deltaTime)) +
      geom_histogram(fill = "black") + 
      xlab("Time Between Events") +
      ylab("Event Count")
  })
  
  event_colors = eventTypeColors()
  
  updateTimePlotCountDebug = printWithCountGen('time plot')
  
  timePlot <- reactive({
    req(isDataLoaded())
    req(!is.null(input$doStemPlot))
    
    no_selection = customizeDisplay$no_selection
    shape_col = customizeDisplay$shapeColumn()
    color_col = customizeDisplay$colorColumn()
    y_col = customizeDisplay$yColumn()
    facet_col = customizeDisplay$facetColumn()
    facet_order = customizeDisplay$facetOrder()
    facet_labels = customizeDisplay$facetLabels()
    facet_customized = customizeDisplay$facetCustomized()
    
    point_aes = aes_string(y = y_col)
    if(!(shape_col == no_selection))
      point_aes$shape = quo(!!sym(shape_col))
    if(!(color_col == no_selection))
      point_aes$colour = quo(!!sym(color_col))
    
    show_data = filteredData() %>%
      mutate(Event = TRUE) %>% {
        if(!(shape_col == no_selection))
          mutate(., !!shape_col := factor(!!sym(shape_col)))
        else .}
    
    if(facet_customized)
      show_data[[facet_col]] = factor(show_data[[facet_col]],
                                      levels = facet_order,
                                      labels = facet_labels)
    
    p = show_data %>%
      ggplot(aes(x = RelativeTime)) + 
      geom_point(point_aes)
    
    y_class = class(show_data[[y_col]])
    if(y_class == "logical")
      p = p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    
    if(input$doStemPlot){
      if(!(color_col == no_selection))
        p = p + geom_segment(aes_string(xend = "RelativeTime", 
                                        yend = 0, 
                                        y = y_col,
                                        colour = color_col))
      else
        p = p + geom_segment(aes_string(xend = "RelativeTime", 
                                        yend = 0, 
                                        y = y_col))
    }
    
    if(!(facet_col == no_selection))
      p = p + facet_grid(formula(paste(facet_col, "~ .")))
    
    return(p)
  })
  
  eventStats <- reactive({
    summary(filteredData()$deltaTime)
  })
  
  showSource = callModule(showSourceServer, 'showSource')
  observeEvent(input$showSource, {
    showSource(filteredData())
  })
  
  observeEvent(input$calcCPA, {
    callModule(showCpa, "", data=filteredData)
  })
  
  observeEvent(input$minimizeHeader, {
    isHeaderMinimized(!isHeaderMinimized())
  })
  
  observe({
    shinyjs::toggle("loadDataHeader", condition = !isHeaderMinimized())
    updateActionButton(session, "minimizeHeader",
                       label = ifelse(isHeaderMinimized(),
                                      "Show",
                                      "Minimize")
    )
  })
  
  
  headerMinimalInformation = reactive({
    parts = c()
    
    if(isDataLoaded())
      parts = append(parts, eventsInformation()$name)
    
    if(isMetaDataLoaded()) {
      parts = append(parts, metaDataFile()$name)
      parts = append(parts, filteredMetaData()$query)
    }
    
    return(toString(parts))
  })
  
  output$headerInformation = renderText({
    if(isHeaderMinimized()) headerMinimalInformation()
  })
  
  output$doStemPlot = renderUI({
    if(input$plotType == "timePlot" & isDataLoaded())
      checkboxInput(ns("doStemPlot"), "Stem Plot", value = T)
  })
  
  output$showSource = renderUI({
    if(input$plotType == "timePlot" & isDataLoaded())
      actionButton(inputId = ns("showSource"), label = "Show Source")
  })
  
  plotHeight = reactive({
    if(is.null(customizeDisplay$plotHeight)) 400
    else customizeDisplay$plotHeight()
  })
  
  
  output$eventPlotContainer = renderUI({
    plotOutput(ns("eventPlot"), height = plotHeight())
  })
  
  output$eventPlot = renderPlot({
    req(isDataLoaded())
    req(input$plotType)
    
    if(input$plotType == "timePlot")
      return(timePlot())
    if(input$plotType == "hist")
      return(hist())
  })
  
  
  output$sidePanel = renderUI({
    req(isDataLoaded())
    
    wellPanel(
      selectInput(ns("plotType"), "Plot Type", 
                  c("Time Plot" = "timePlot", "Histogram" = "hist"),
                  selected = "timePlot"),
      #sliderInput(ns("plotHeight"), "Plot Height", 
      #            value = 400, min = 20, max = 2000),
      customizeDisplayUI(ns("customizeDisplay")),
      uiOutput(ns("showSource")), 
      uiOutput(ns("calcCPA"), label = "Show CPA"),
      uiOutput(ns("showEventStats"), label = "Basic Statistics"),
      uiOutput(ns("downloadDataOutput")),
      uiOutput(ns("doStemPlot"))
    )
  })
  
  output$eventStats = renderPrint({
    req(isDataLoaded())
    
    if(input$plotType == "timePlot")
      return(eventStats())
    if(input$plotType == "hist")
      return(eventStats())
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      hmi = headerMinimalInformation() %>% 
        {gsub(', ?', '-', .)} %>%
        {gsub('.csv','', .)}
      paste0(hmi, "-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(filteredData(), file, row.names = F)
    }
  )
  
  output$downloadDataOutput = renderUI({
    if(isDataLoaded())
      downloadButton(ns("downloadData"))
  })
  
  output$calcCPA = renderUI({
    if(input$plotType == "timePlot" & isDataLoaded())
      actionButton(inputId = ns("calcCPA"), label = "Show CPA")
  })
  
  output$showEventStats = renderUI({
    if(isDataLoaded())
      actionButton(inputId = ns("showEventStats"), "Basic Statistics")
  })
  
  observeEvent(input$showEventStats, {
    callModule(showEventStats, "", data=filteredData)
  })
}
