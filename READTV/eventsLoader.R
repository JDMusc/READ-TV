

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
      data = eventDataF()$datapath %>% loadSugicalEvents
    } else {
      data = loadEventsWithRelativeAndDeltaTime(eventDataF()$datapath)
    }

    return(list(name = eventDataF()$name, data = data,
		datapath = eventDataF()$datapath))
  })
  )
}
