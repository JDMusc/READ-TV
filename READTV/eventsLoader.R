

eventsLoaderUI = function(id) {
  ns <- NS(id)
  fileWellUI(ns("filewell"))
}


eventsLoader = function(input, output, session, output_sym) {
  ns = session$ns
  f = stringr::str_interp
  
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
    
    eval_tidy(currentCode(), data = list(f_name = datapath()))
  })
  
  
  quickInspect = reactive({
    req(datapath())
    
    quickLoad(datapath())
  })
  
  
  sourceString = reactive({
    req(name())
    f_name = name()
    f_name_var = "f_name"
    
    expressionsToString(
      f("${f_name_var} = \"${f_name}\" #update local file path"),
      currentCode()
    )
  })
  
  currentCode = reactive({
    req(datapath())
    
    datapath() %>% loadEventsWithRelativeAndDeltaTimeCode(output_sym)
  })
  

  return(
    list(name = name, data = data,
         datapath = datapath,
         sourceString = sourceString)
    )
}
