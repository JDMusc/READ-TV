

eventsLoaderUI = function(id) {
  ns <- NS(id)
  fileWellUI(ns("filewell"))
}


eventsLoader = function(input, output, session, output_sym, eventsPath = NULL,
                        inputData = NULL) {
  ns = session$ns
  f = stringr::str_interp
  
  pre_transform_data_sym = sym("pre_transform_data")
  
  isInputDataSet = reactive({
    !is_null(inputData)
  })
  
  if(!isInputDataSet())
    eventDataF = callModule(
      fileWellServer, "filewell", "Event Data", eventsPath)
  else
    eventDataF = reactive({NULL})
  
  
  name = reactive({
    if(!isInputDataSet()) {
      req(eventDataF())
      eventDataF()$name
    }
    else
      'inputData'
  })
  
  datapath = reactive({
    req(eventDataF())
    eventDataF()$datapath
  })
  
  data = reactive({
    if(!isValidRaw())
      req(dataTransform$ready())
    
    c_code = currentCode()
    
    if(!isInputDataSet())
      eval_tidy(c_code, list(f_name = datapath()))
    else
      eval_tidy(c_code, list(inputData = inputData))
  })
  
  
  quickInspect = reactive({
    n_max = 100
    
    if(!isInputDataSet()) {
      req(datapath())
      quickLoad(datapath(), n_max = n_max)
    } 
    else inputData %>% head(n_max)
  })
  
  isValidData = function(df) {
    has_cols = ('Case' %in% colnames(df)) &
      ('Time' %in% colnames(df))
    
    if(!has_cols)
      return(FALSE)
    
    is.timepoint(df$Time) | is.numeric(df$Time)
  }
  
  
  isValidRaw = reactive({
    if(!isInputDataSet())
      req(datapath())
    
    isValidData(quickInspect())
  })
  
  
  sourceString = reactive({
    req(name())
    f_name = name()
    f_name_var = "f_name"
    
    top_row = if(isInputDataSet())
      "#user set input data" 
    else
      f("${f_name_var} = \"${f_name}\" #update local file path")
    
    expressionsToString(
      top_row,
      currentCode()
    )
  })
  
  currentCode = reactive({
    if(!isInputDataSet()) {
      req(datapath())
      
      input = datapath()
      fn = loadEventsWithRelativeAndDeltaTimeCode
    } else {
      input = sym('inputData')
      fn = appendEventsWithRelativeAndDeltaTimeCode
    }
    
    run_data_transform = dataTransform$ready()
    
    if(run_data_transform)
      input %>% 
        fn(output_sym, cols = dataTransform$mutateCols())
    else
      input %>% fn(output_sym)
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
