

showEventStats = function(input, output, session, data) {
  ns = session$ns

  eventStats <- reactive({
    d = data()
    totalTime = function(da) da %>%
      group_by(Case) %>%
      summarise(TDiff = dplyr::last(Time) - first(Time)) %>%
      {sum(.$TDiff)}

    total_time = totalTime(d)
    d %>%
      group_by(Event.Type) %>%
      summarise(n = n(), rate = n/total_time) %>%
      as.data.frame %>%
      dplyr::add_row(Event.Type = "Combined", n = nrow(d),
                     rate = n/total_time) %>%
      rename(`Event Type` = Event.Type, Count = n, Rate = rate)
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
