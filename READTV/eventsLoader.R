

eventsLoaderUI = function(id) {
  ns <- NS(id)
  fileWellUI(ns("filewell"))
}


eventsLoader = function(input, output, session, output_sym, eventsPath = NULL) {
  ns = session$ns
  f = stringr::str_interp
  
  pre_transform_data_sym = sym("pre_transform_data")
  
  eventDataF = callModule(fileWellServer, "filewell", "Event Data", eventsPath)
  
  name = reactive({
    req(eventDataF())
    
    eventDataF()$name
  })
  
  datapath = reactive({
    req(eventDataF())
    eventDataF()$datapath
  })
  
  data = reactive({
    if(!isValidRaw())
      req(dataTransform$ready())
    
    c_code = currentCode()
    
    eval_tidy(c_code, list(f_name = datapath()))
  })
  
  
  quickInspect = reactive({
    req(datapath())
    
    quickLoad(datapath(), n_max = 100)
  })
  
  isValidData = function(df) {
    has_cols = ('Case' %in% colnames(df)) &
      ('Time' %in% colnames(df))
    
    if(!has_cols)
      return(FALSE)
    
    is.timepoint(df$Time) | is.numeric(df$Time)
  }
  
  
  isValidRaw = reactive({
    req(datapath())
    
    isValidData(quickInspect())
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
    
    run_data_transform = dataTransform$ready()
    
    if(run_data_transform)
      datapath() %>% 
        loadEventsWithRelativeAndDeltaTimeCode(output_sym, 
                                               cols = dataTransform$mutateCols())
    else
      datapath() %>% 
        loadEventsWithRelativeAndDeltaTimeCode(output_sym)
  })
  
  dataTransform = callModule(dataTransformServer, 'dataTransform',
                             quickInspect, 
                             pre_transform_data_sym, output_sym)
  
  showTransformPopup = reactive({
    dataTransform$popup(TRUE)
  })
  

  return(
    list(name = name, data = data,
         datapath = datapath,
         sourceString = sourceString,
         showTransformPopup = dataTransform$popup,
         quickInspect = quickInspect,
         isValidRaw = isValidRaw)
    )
}
