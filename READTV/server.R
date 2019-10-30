library(changepoint)
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
        req(metaDataFile())
        
        mdf = metaDataFile()
        read.csv(mdf, header = T, stringsAsFactors = F)
    })
    
    output$metaQueryApplyOutput = renderUI({
      validate(need(input$queryInput, F))
      
      qcc = column(actionButton("queryClear", "Clear Case Filter"),
                   width = 2)
      qac = column(actionButton("querySubmit", "Apply Case Filter"),
                   width = 2)
      if(input$queryInput != "")
          fluidRow(
              qcc
          )
      else
          NULL
    })
    
    
    output$metaPreview = renderUI({
      actionButton("metaPreview", "Preview Cases")
    })

    output$queryInput = renderUI({
        textInput("queryInput", "Case Filter", placeholder = "")
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
    
    
    output$metaQuery = renderUI({
        mdf = metaDataFile()
        wellPanel(
            uiOutput("queryInput"),
            actionButton("queryInclude", "Include Condition"),
            uiOutput("metaQueryApplyOutput"),
            fluidRow(
                column(uiOutput("metaPreview"), width = 2),
                column(renderText(paste("Meta Data File: ", mdf)),
                       width = 8)
            )
        )
    })
    
    observe({
        req(metaData())
        
        fmd = filteredMetaData()
        if(is.null(fmd)) fmd = metaData()
        updateActionButton(session, "metaPreview", 
                           paste("Preview", nrow(fmd),  "Cases"))
    })
    
    filterMetaData = function(qry)
        try(
            qry %>%
                {paste0('metaData() %>% filter(', ., ')')} %>%
                {parse(text = .)} %>%
                eval, 
            silent = T)
    
    
    hasQueryInput = reactive({
        if(is.null(input$queryInput)) return(F)
        if(input$queryInput == "") return(F)
        
        T
    })
    
    
    queryCompiles = reactive({
        #req(hasQueryInput())
        fmd = try(filterMetaData(input$queryInput), silent = T)
        
        return(!(class(fmd) == "try-error"))
    })
    
    filteredMetaData = reactive({
        qc = queryCompiles()
        if(qc)
            filterMetaData(input$queryInput)
        else
            NULL
    })
    
    observeEvent(input$querySubmit, {
        serverState$meta_cases = filterMetaData(input$queryInput)$Case
    })
    
    observeEvent(input$queryClear, {
        serverState$meta_cases = NULL
        updateTextInput(session, "queryInput", value = "")
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
                    selectizeInput("fieldFilter", "",
                                   choices = filter_choices),
                    width = 2),
                column(
                    selectizeInput("fieldValue", "",
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
                actionButton("modalSubmit", "Include"),
                modalButton("Cancel")
            ),
            selectInput("metaField", "Field", 
                        choices = colnames(meta_data)),
            uiOutput("fieldOptions"),
            uiOutput("appendQueryOptions")
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
    
    filteredData <- reactive({
        req(serverState$data_loaded)
        
        d = data()
        ca = case()
        ph = phase()
        fd = flowDisruption()
        
        if(!is.null(filteredMetaData())) {
            d = d %>% filter(Case %in% filteredMetaData()$Case)
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
      showSource(filteredData)
    })
    
    observeEvent(input$calcCPA, {
      callModule(showCpa, "", data=filteredData)
    })
}
