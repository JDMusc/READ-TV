

eventsLoaderUI = function(id) {
  ns <- NS(id)
  fileWellUI(ns("filewell"))
}


eventsLoader = function(input, output, session) {
  ns = session$ns
  
  eventDataF = callModule(fileWellServer, "filewell", "Event Data", 
                          '../data/tc_prepped_events.csv')
  
  name = reactive({
    req(eventDataF())
    eventDataF()$name
  })
  
  datapath = reactive({
    req(eventDataF())
    eventDataF()$datapath
  })
  
  data = reactive({
    req(datapath())
    datap = datapath()
    
    if(config.testing) {
      d = datap %>% loadSugicalEvents
    } else {
      d = datap %>% loadEventsWithRelativeAndDeltaTime
    }
    
    d
  })

  return(
    list(name = name, data = data,
         datapath = datapath)
    )
}
