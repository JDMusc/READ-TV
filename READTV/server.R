library(dplyr)
library(ggplot2)
library(shiny)
library(shinyjs)

source('../analysis_helpers.R')
source('../dataMungers.r')

source('utils.R')


function(input, output, session){
    
    serverState = reactiveValues(data_loaded = FALSE,
        meta_cases = NULL)
    
    dataFile = eventReactive(input$loadData, {
        "../data/tc_prepped_events.csv"
        })
    
    data <- reactive({
        tbl = loadEventsClean(dataFile())
        if(!is.null(serverState$meta_cases)){
            tbl = tbl %>% filter(Case %in% serverState$meta_cases)
        }
        serverState$data_loaded = TRUE
        tbl
        })
    
    metaDataFile <- eventReactive(input$loadMetaData, {
        '../data/mockMetaData.csv'
    })
    
    metaData <- reactive({
        mdf = metaDataFile()
        read.csv(mdf, header = T)
    })
    
    output$metaQuery = renderUI({
        mdf = metaDataFile()
        wellPanel(
            textInput("queryInput", "Enter Case Filter", placeholder = ""),
            actionButton("querySubmit", "Submit Case Filter"), 
            actionButton("queryClear", "Clear Clase Filter"),
            actionButton("metaPreview", "Preview Cases"),
            renderText(paste("Meta Data File: ", mdf))
        )
    })
    
    filterMetaData = function(qry) {
        qry %>%
            {paste0('metaData() %>% filter(', ., ')')} %>%
            {parse(text = .)} %>%
            eval
    }
    
    observeEvent(input$querySubmit, {
        serverState$meta_cases = filterMetaData(input$queryInput)$Case
    })
    
    observeEvent(input$queryClear, {
        serverState$meta_cases = NULL
        updateTextInput(session, "queryInput", value = "")
    })
    
    observeEvent(input$metaPreview, {
        meta_data = metaData()
        if(input$queryInput != "") 
            meta_data = filterMetaData(input$queryInput)
        
        showModal(modalDialog(
            title = "Cases",
            renderDataTable(meta_data),
            easyClose = TRUE,
            size = "m"
        ))
    })
    
    filteredData <- reactive({
        req(serverState$data_loaded)
        
        d = data()
        ca = case()
        ph = phase()
        fd = flowDisruption()
        
        if(!is.null(serverState$meta_cases)) {
            d = d %>% filter(Case %in% serverState$meta_cases)
        }
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
    
    output$hist <- renderPlot({
        ggplot(filteredData(), aes(x = deltaTime)) +
            geom_histogram(fill = "black") + 
            xlab("Time Between Events") +
            ylab("Event Count")
    })
    
    fd_colors = fdTypeColors()
    output$timePlot <- renderPlot({
        if(!isSelected(case())) return
        p = filteredData() %>% mutate(Event = TRUE) %>%
            ggplot(aes(x = Time)) + 
            geom_point(aes(y = Event, colour = FD.Type, shape = factor(Phase) )) +
            theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
            labs(col = "FD Type", shape = "Phase") +
            scale_shape_manual(values = list("1" = 16, "2" = 17, "3" = 15, "4" = 3)) +
            scale_color_manual(values = fd_colors)
        if(input$doStemPlot){
            p = p + geom_segment(aes(xend = Time, 
                                     yend = Event - Event, 
                                     y = Event,
                                     colour = FD.Type))
        }
        return(p)
    })
    
    output$countPlot <- renderPlot({
        if(!isSelected(case())) return
        filteredData() %>% mutate(Event = TRUE) %>%
            ggplot(aes(x = Time)) + geom_point(aes(y = Event, 
                                                   colour = FD.Type,
                                                   shape = Phase))
        filteredData() %>% eventCount(5) %>%
            ggplot(aes(x = Time)) + geom_point(aes(y = WithinN))
    })
    
    output$fdStats <- renderPrint({
        summary(filteredData()$deltaTime)
    })
    
    output$eventStats <- renderPrint({
        d = filteredData()
        fd = flowDisruption()
        totalTime = function(da) da %>% 
            group_by(Case) %>%
            summarise(TDiff = last(Time) - first(Time)) %>%
            {sum(.$TDiff)}
        
        total_time = totalTime(d)
        d %>% 
            group_by(FD.Type) %>% 
            summarise(n = n(), rate = n/total_time) %>% 
            as.data.frame %>% 
            add_row(FD.Type = "Combined", n = nrow(d), rate = n/total_time) %>%
            rename(`Disruption Type` = FD.Type, Count = n, Rate = rate)
    })
    
    observeEvent(input$showSource, {
        showModal(modalDialog(
            title = "Source Data",
            renderDataTable(filteredData()[,
                                           c("Case", "Phase", "Time", "FD.Type", "Notes")]
            ),
            easyClose = TRUE,
            size = "m"
        ))
    })
    
    observeEvent(input$calcCPA, {
        fdata = filteredData()
        start_time = fdata$Time[1]
        end_time = max(fdata$Time)
        showModal(modalDialog(
            title = "Change Point Analysis",
            selectInput("cpaSelect", "Select N", 1:5, selected = 4),
            renderPlot({
                n = as.numeric(input$cpaSelect)
                time_data = withinTimeSeries(fdata$RelativeTime, n = n)
                plot(time_data, ylab = paste("# Events per", n*2, "minutes"))
            })
        ))
    })
}
