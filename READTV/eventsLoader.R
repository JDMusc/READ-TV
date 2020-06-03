

eventsLoaderUI = function(id) {
  ns <- NS(id)
  fileWellUI(ns("filewell"))
}


loadWithRelativeAndDelta = function(data_f) 
	data_f %>% 
		loadEvents %>% 
		deltaTimes %>% 
		relativeTimes %>%
		filter(RelativeTime >= 0)


eventsLoader = function(input, output, session) {
  ns = session$ns
  
  eventDataF = callModule(fileWellServer, "filewell", "Event Data", '../data/tc_prepped_events.csv')
  
  return(reactive({
    req(eventDataF())
    
    if(config.testing) {
      data = eventDataF()$datapath %>% loadEventsClean
    } else {
      data = loadWithRelativeAndDelta(eventDataF()$datapath)
    }

    return(list(name = eventDataF()$name, data = data))
  })
  )
}
