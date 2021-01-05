

eventsLoaderUI = function(id) {
  ns <- NS(id)
  fileWellUI(ns("filewell"))
}


eventsLoader = function(input, output, session, output_sym, eventsPath = NULL,
                        inputData = NULL) {
  ns = session$ns
  f = stringr::str_interp

  location = function(msg) f('eventsLoader, ${msg}')

  log_info_el = log_info_module_gen('eventsLoader')
  req_log = req_log_gen(log_info_el)

  raw_data_sym = sym("raw_data")
  mutated_data_sym = sym("mutated_data")

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
      req(eventDataF$fileInfo())

      eventDataF$fileInfo()$name
    }
    else
      'inputData'
  })

  hasFile = reactive({
    !is.null(eventDataF$fileInfo())
  })

  datapath = reactive({
    req_log('datapath', quo(eventDataF$fileInfo()))
    eventDataF$fileInfo()$datapath
  })

  isDataReady = reactive({
    ret = if(!(hasFile() | isInputDataSet())) FALSE
    else isValidRaw() | dataTransform$ready()

    log_info_el(f('isDataReady ${ret}'))

    ret
  })

  data = reactive({
    if(!isValidRaw())
      req(dataTransform$ready())

    runExpressionsLast(currentCode(), rawDataMask(), location='el d')
  })

  rawData = reactive({
    rawDataCodes() %>%
      runExpressionsLast(rawDataMask(), location = location('rawData'))
  })

  rawDataMask = reactive({
    if(!isInputDataSet()) {
      req(datapath())

      list()
    } else {
      set_names(
        list(inputData),
        expr(inputData)
      )
    }
  })

  rawDataCodes = reactive({
    if(!isInputDataSet()) {
      req(datapath())

      exs = list(
        expr(f_name <- !!datapath()),
        loadEventsCode(datapath(), out_pronoun = raw_data_sym)
      )

      set_names(
        exs,
        list(sym('f_name'), raw_data_sym)
      )
    }
    else {
      exs = list(expr(!!raw_data_sym <- inputData))
      rlang::set_names(exs, list(raw_data_sym))
    }

  })

  isValidData = function(df) {
    has_time = 'Time' %in% colnames(df)

    if(!has_time)
      return(FALSE)

    is.timepoint(df$Time) | is.numeric(df$Time)
  }


  isValidRaw = reactive({
    req_log('isValidRaw', quo(rawData()))
    if(!isInputDataSet())
      req(datapath())

    isValidData(rawData())
  })


  sourceString = reactive({
    req_log('sourceString', quo(name()))

    if(!isValidRaw())
      req(dataTransform$ready())

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

  hasCol = function(col, mutate_cols) col %in% c(
    colnames(rawData()),
    mutate_cols
  )

  currentCode = reactive({
    if(isValidRaw()) {
      pre_group_codes = rawDataCodes()
      pre_group_sym = raw_data_sym
    } else {
      run_data_transform = dataTransform$ready()
      req(run_data_transform)

      pre_group_codes = dataTransform$code()
      pre_group_sym = mutated_data_sym
    }

    pre_group_data = runExpressionsLast(pre_group_codes, rawDataMask(),
                                        location=location('currentCode'))
    pre_group_cols = colnames(pre_group_data)
    d_cols = colnames(pre_group_cols)

    output_code = appendEventsWithRelativeAndDeltaTimeCode(pre_group_sym,
                                                               output_sym,
                                                               hasCol('Case', d_cols))

    #output_code = expr(!!output_sym <- !!output_code_rhs)

    c(
      pre_group_codes,
      rlang::set_names(list(output_code), list(output_sym))
    )
  })

  dataTransform = callModule(dataTransformServer, 'dataTransform',
                             rawDataCodes,
                             rawDataMask,
                             raw_data_sym, mutated_data_sym)


  observe({
    req_log('observe raw data', quo(rawData()))

    if(!isValidRaw())
      dataTransform$popup(TRUE)
  });


  return(
    list(name = name,
         mask = rawDataMask,
         code = currentCode,
         isDataReady = isDataReady)
    )
}
