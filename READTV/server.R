library(dplyr)
library(ggplot2)
library(shiny)
library(shinyjs)

source('../analysis_helpers.R')
source('../dataMungers.r')

source('utils.R')


function(input, output, session){
    
    serverState = reactiveValues(data_loaded = FALSE)
    
    eventsData = callModule(eventsLoader, "loadData")
    
    data <- reactive({
        #tbl = loadEventsClean(dataFile())
        tbl = eventsData()#dataFile() %>% read.csv %>% relativeTimes
        if(metaDataLoaded()) {
          tbl = tbl %>% filter(Case %in% filteredMetaData()$Case)
        }
        serverState$data_loaded = TRUE
        tbl
        })
    
    metaDataFile <- callModule(metaQueryLoader, "loadMetaData")
    
    observe({
      req(metaDataFile())
      
      callModule(metaQueryServer, "metaqueryui", metaDataFile)
      })
    
    
    filteredMetaData <- callModule(metaQueryServer, "metaqueryui", 
                                   metaDataFile)
    
    metaDataLoaded = reactive({
      fmd = try(filteredMetaData(), silent = T)
      
      return(!(class(fmd) == "try-error"))
    })

    
    filteredData <- reactive({
        req(serverState$data_loaded)
        
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
      showSource(filteredData)
    })
    
    observeEvent(input$calcCPA, {
      callModule(showCpa, "", data=filteredData)
    })
}
