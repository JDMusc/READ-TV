

selectRows <- function(ns) {
  fluidRow(
    column(
      width = 2,
      selectInput(ns("caseSelect"), "Select Case", selectableChoices(c()))),
    column(
      width = 2,
      selectInput(ns("phaseSelect"), "Select Phase", selectableChoices(c()))),
    column(
      width = 2,
      selectInput(ns("fdSelect"), "Select FD Type", selectableChoices(c()),
                  multiple = TRUE)),
    column(
      width = 2,
      selectInput(ns("plotType"), "Plot Type", 
                  c("Time Plot" = "timePlot", "Histogram" = "hist"),
                  selected = "timePlot")),
    column(
      width = 2,
      uiOutput(ns("doStemPlot"))
    )
  )
}


eventsDisplayUI <- function(id) {
  ns = NS(id)
  fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(list(.invalid_query = 'background-color: #f006')),
    actionButton(ns("minimizeHeader"), "Minimize Header"),
    uiOutput(ns("headerInformation")),
    div(id = ns("loadDataHeader"),
        fluidRow(
          eventsLoaderUI(ns("loadData")),
          wellPanel(
            metaQueryLoaderUI(ns("loadMetaData")),
            metaQueryUI(ns("metaqueryui"))
          )
        )
    ),
    selectRows(ns),
    fluidRow(
      column(plotOutput(ns("eventPlot")), width = 10),
      column(wellPanel(uiOutput(ns("showSource")), 
                uiOutput(ns("calcCPA"), label = "Show CPA"),
                uiOutput(ns("showEventStats"), label = "Basic Statistics"),
                uiOutput(ns("downloadDataOutput"))), 
             width = 2)
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
  
  
  filteredData <- reactive({
    req(isDataLoaded())
    
    d = data()
    ca = case()
    ph = phase()
    fd = flowDisruption()
    
    if(isSelected(ca)) d = d %>% filter(Case == ca)
    if(isSelected(ph)) d = d %>% filter(Phase == ph)
    if(isSelected(fd)) d = d %>% filter(FD.Type %in% fd)
    
    d
  })
  
  case <- eventReactive(input$caseSelect, {input$caseSelect})
  
  observe({
    cases = data()$Case %>% unique
    updateSelectInput(session, "caseSelect", 
                      choices = selectableChoices(cases)
    )
  })
  
  phase <- eventReactive(input$phaseSelect, {input$phaseSelect})
  
  observe({
    ca = input$caseSelect
    d = data()
    
    if(isSelected(ca)) phases = d[d$Case== ca,]$Phase %>% unique
    else phases = unique(d$Phase)
    
    updateSelectInput(session, "phaseSelect", 
                      choices = selectableChoices(phases),
                      selected = "All"
    )
  })
  
  observe({
    ca = case()
    ph = phase()
    d = data()
    
    if(isSelected(ca)) d = d %>% filter(Case == ca)
    if(isSelected(ph)) d = d %>% filter(Phase == ph)
    fds = d$FD.Type %>% unique %>% as.character
    
    updateSelectInput(session, "fdSelect", 
                      choices = selectableChoices(fds),
                      selected = "All")
  })
  
  flowDisruption = eventReactive(input$fdSelect, {input$fdSelect})
  
  observeEvent(input$fdSelect, {
    fd = flowDisruption()
    has_all = 'All' %in% fd
    only_all = length(fd) == 1
    
    if(has_all & !only_all) {
      was_all = fd[1] == 'All'
      if(was_all) 
        updateSelectInput(session, "fdSelect", 
                          selected = fd[-1])
      else
        updateSelectInput(session, "fdSelect",
                          selected = 'All')
    }
  })
  
  hist <- reactive({
    req(isDataLoaded())
    
    ggplot(filteredData(), aes(x = deltaTime)) +
      geom_histogram(fill = "black") + 
      xlab("Time Between Events") +
      ylab("Event Count")
  })
  
  fd_colors = fdTypeColors()
  
  timePlot <- reactive({
    req(isDataLoaded())
    
    p = filteredData() %>% mutate(Event = TRUE) %>%
      ggplot(aes(x = RelativeTime)) + 
      geom_point(aes(y = Event, colour = FD.Type, shape = factor(Phase) )) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
      labs(col = "FD Type", shape = "Phase") +
      scale_shape_manual(values = list("1" = 16, "2" = 17, "3" = 15, "4" = 3)) +
      scale_color_manual(values = fd_colors)
    
    if(input$doStemPlot){
      p = p + geom_segment(aes(xend = RelativeTime, 
                               yend = Event - Event, 
                               y = Event,
                               colour = FD.Type))
    }
    
    return(p)
  })
  
  fdStats <- reactive({
    summary(filteredData()$deltaTime)
  })
  
  observeEvent(input$showSource, {
    showSource(filteredData)
  })
  
  observeEvent(input$calcCPA, {
    callModule(showCpa, "", data=filteredData)
  })
  
  observeEvent(input$minimizeHeader, {
    isHeaderMinimized(!isHeaderMinimized())
  }
  )
  
  observe({
    shinyjs::toggle("loadDataHeader", condition = !isHeaderMinimized())
    updateActionButton(session, "minimizeHeader",
                       label = ifelse(isHeaderMinimized(),
                                      "Show Header",
                                      "Minimize Header")
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
    else NULL
  })
  
  output$doStemPlot = renderUI({
    if(input$plotType == "timePlot" & isDataLoaded())
      checkboxInput(ns("doStemPlot"), "Stem Plot", value = T)
    else
      NULL
  })
  
  output$showSource = renderUI({
    if(input$plotType == "timePlot" & isDataLoaded())
      actionButton(inputId = ns("showSource"), label = "Show Source")
    else
      NULL
  })
  
  output$eventPlot = renderPlot({
    req(isDataLoaded())
    
    if(input$plotType == "timePlot")
      return(timePlot())
    if(input$plotType == "hist")
      return(hist())
  })
  
  output$eventStats = renderPrint({
    req(isDataLoaded())
    
    if(input$plotType == "timePlot")
      return(eventStats())
    if(input$plotType == "hist")
      return(fdStats())
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
      downloadButton("downloadData")
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
