

showEventStats = function(input, output, session, data) {
  ns = session$ns

  eventStats <- reactive({
    d = data()

    if('Event.Type' %in% colnames(d))
      d %>%
        group_by(Event.Type) %>%
        summarise(n = n()) %>%
        as.data.frame %>%
        dplyr::add_row(Event.Type = "Combined", n = nrow(d)) %>%
        rename(`Event Type` = Event.Type, Count = n)
    else
      d %>% summarise(Count = n())
  })

  summaryStats <- reactive({
    summary(data()$deltaTime)
  })

  totalTime <- reactive({
    req(data())

    sumTDiff = function(d) d %>%
      summarise(TDiff = dplyr::last(Time) - dplyr::first(Time)) %>%
      {sum(.$TDiff)}

    da = data()
    if('Case' %in% colnames(da)) da %>%
      group_by(Case) %>%
      sumTDiff
    else da %>% sumTDiff
  })

  output$totalTime = renderPrint({
    totalTime()
  })

  output$eventStats = renderPrint({
    req(data())
    d = data()

    eventStats() %>% as.data.frame
  })

  output$summaryStats = renderPrint({
    summaryStats()
  })

  showModal(modalDialog(
    title = "Event Statistics",
    easyClose = T,
    fluidRow(
      h4("Event Counts"),
      verbatimTextOutput(ns("eventStats")),
      h4("Interevent Times Summary"),
      verbatimTextOutput(ns("summaryStats")),
      h4("Total Duration"),
      verbatimTextOutput(ns("totalTime"))
    )
  ))
}
