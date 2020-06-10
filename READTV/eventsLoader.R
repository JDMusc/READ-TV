

eventsLoaderUI = function(id) {
  ns <- NS(id)
  fileWellUI(ns("filewell"))
}


loadEventsWithRelativeAndDeltaTime = function(data_f, index = 'DateTime', key = NULL) 
	data_f %>% 
		#loadEventsAsTsibble(index = index, key = key) %>% 
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
      data = loadEventsWithRelativeAndDeltaTime(eventDataF()$datapath)
    }

    return(list(name = eventDataF()$name, data = data))
  })
  )
}
