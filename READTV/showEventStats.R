

showEventStats = function(input, output, session, data) {
  ns = session$ns
  
  eventStats <- reactive({
    d = data()
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
  
  summaryStats <- reactive({
    summary(data()$deltaTime)
  })
  
  output$eventStats = renderPrint({
    return(eventStats())
  })
  
  output$summaryStats = renderPrint({
    return(summaryStats())
  })
  
  showModal(modalDialog(
    title = "Event Statistics",
    easyClose = T,
    fluidRow(
      h4("Event Types"),
      verbatimTextOutput(ns("eventStats")),
      h4("Interevent Times Distribution"),
      verbatimTextOutput(ns("summaryStats"))
    )
  ))
}
