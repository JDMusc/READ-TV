

eventsLoaderUI = function(id) {
  ns <- NS(id)
  fileWellUI(ns("filewell"))
}


eventsLoader = function(input, output, session) {
  ns = session$ns
  
  eventDataF = callModule(fileWellServer, "filewell", "Event Data", '../data/tc_prepped_events.csv')
  
  return(reactive({
    req(eventDataF())
    
    if(config.testing) {
      data = eventDataF()$datapath %>% loadEventsClean
    } else {
      data = eventDataF()$datapath %>% 
        loadEvents %>% 
        deltaTimes %>% 
        relativeTimes %>%
        filter(RelativeTime >= 0)
    }

    return(list(name = eventDataF()$name, data = data))
  })
  )
}
