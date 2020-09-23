cpaPreprocessUI = function(id) {
  ns = NS(id)

  fluidRow(
    column(uiOutput(ns("cpaInput")), width = 2),
    uiOutput(ns("regularCheck")),
    column(uiOutput(ns("windowWidth")), width = 2),
    column(uiOutput(ns("windowStride")), width = 2),
    column(selectInput(ns("aggFn"), "Freq. Metric",
                       choices = c("count", "rate", "mean"),
                       selected = "count"),
           width = 2),
    column(actionButton(ns("regularizeSubmit"), label = "Regularize"),
           width = 2)
  )
}


cpaPreprocessServer = function(input, output, session, previousData,
                               previousPlotOpts) {
  ns = session$ns
  f = stringr::str_interp

  #----Do Regularize----
  eventFrequency = "Event Frequency"

  isEventFrequencySelected = reactive({
    input$cpaInputSelect %==% eventFrequency
  })

  output$regularCheck = renderUI({
    if(isEventFrequencySelected())
      return()

    column(selectInput(ns("doRegularize"),
                       "Already Regular Spacing?",
                       choices = c("Yes", "No")),
           width = 2)
  })

  doRegularize = reactive({
    if(isEventFrequencySelected())
      TRUE
    else
      input$doRegularize == "No"
  })

  y = reactive({
    previousPlotOpts$y
  })

  isYcolAnyEvent = reactive(
    y() == previousPlotOpts$anyEvent
  )

  validCpaInputs = reactive({
    vals = c(eventFrequency)

    if(isYcolAnyEvent()) vals
    else y() %>%
      append(vals) %>%
      displayEmptyStrAsAnyEvent(
        previousPlotOpts$anyEvent
      )
  })

  output$cpaInput = renderUI({
    selected = if(isYcolAnyEvent()) eventFrequency else y()
    selectInput(ns("cpaInputSelect"), "CPA Input",
                     choices = validCpaInputs(),
                     selected = selected)
  })

  smoothInputUIs = c("windowWidth", "windowStride", "aggFn", "regularizeSubmit")
  smoothInputs = reactive({
    base = c("windowWidthText", "windowStrideText", "aggFn")

    if(showTimeOptions())
      c(base,
        "windowWidthUnits", "windowStrideUnits")
    else
      base
  })

  isSmoothInputValid = reactiveValues()
  observe({
    req(smoothInputs())

    for(si in smoothInputs()) {
      orig = getElementSafe(si, isSmoothInputValid, FALSE)
      if(str_detect(si, 'Text')) {
        val = input %>%
          extract2(si) %>%
          as.numeric
        val = if(is_null_or_empty(val))
          FALSE
        else
          (val > 0 & rlang::is_integerish(val))
      } else val = TRUE

      if(orig != val)
        isSmoothInputValid[[si]] = val
    }
  })

  toggleSmoothControl = function(id)
    shinyjs::toggle(id = id, condition = doRegularize())

  observe({
    smoothInputUIs %>% sapply(toggleSmoothControl)
  })


  #----Time Options----
  convValues = reactiveValues(windowWidth = 5,
                               windowStride = 2)
  convUnits = reactiveValues(windowWidth = NULL,
                             windowStride = NULL)

  showTimeOptions = reactive({
    req(previousData())

    previousData() %>%
      pull(previousPlotOpts$x) %>%
      {is.difftime(.) | is.timepoint(.)}
  })


  timePrefixes = list(
    pico = 1e-12,
    nano = 1e-9,
    micro = 1e-6,
    milli = 1e-3
  )

  timeUnits = c(
    "seconds",
    "minutes",
    "hours",
    "days",
    "weeks",
    "months",
    "years"
  )

  timeChoices = append(
    timePrefixes %>% names %>% purrr::map(~ paste0(.x, 'seconds')),
    timeUnits
  ) %>%
    purrr::map(~ paste0('d', .x))


  defaultUnits = function(times) {
    tdiff = max(times) - min(times)
    range_units = attr(tdiff, 'units')

    if(range_units == 'secs')
      range_units = 'seconds'
    else if(range_units == 'mins')
      range_units = 'minutes'
    else if(range_units == 'auto')
      return('seconds')

    ix = which(timeUnits == range_units)

    if(ix > 1) return(timeUnits[[ix - 1]])

    tdiff_sec = as.numeric(tdiff, 'secs')
    default = timePrefixes %>%
      keep(~ .x < tdiff_sec) %>%
      names %>%
      map(~ paste0(.x, 'seconds')) %>%
      head(1)

    if(is_empty(default)) 'picoseconds'
    else default
  }


  timeUnitsTextInput = function(tag, text_label, value) {
    ti = textInput(ns(f("${tag}Text")), text_label,
                   value = value)

    if(showTimeOptions()) {
      fluidRow(
        ti,
        selectInput(ns(f("${tag}Units")), "Units",
                    choices = timeChoices,
                    selected = convUnits[[tag]])
      )
    }
    else
      ti
  }


  extractTimeValue = function(tag) {
    val = input %>% extract2(f("${tag}Text")) %>% as.numeric

    if(showTimeOptions()) {
      units = input %>% extract2(f('${tag}Units'))

      expr(!!val %>% !!parse_expr(units)) %>%
        eval_tidy
    }
    else
      val
  }


  #----Window Width----
  output$windowWidth = renderUI({
    req(previousData())

    timeUnitsTextInput("windowWidth", "Interval Width",
              value = isolate(convValues$windowWidth))
  })

  windowWidth = reactive({
    req(input$windowWidthText)

    extractTimeValue('windowWidth')
  })


  observe({
    req(showTimeOptions())

    for(tag in names(convUnits)) {
      val = input %>% extract2(f('${tag}Units'))
      if(is.null(val))
        val = previousData() %>%
          pull(previousPlotOpts$x) %>%
          defaultUnits %>%
          {paste0('d', .)}

      orig = convUnits[[tag]]
      if(orig %!=% val)
        convUnits[[tag]] = val
    }
  })

  observe({
    for(tag in names(convValues)) {
      val = input %>% extract2(f('${tag}Text')) %>% as.numeric
      if(is_null_or_empty(val))
        next

      orig = convValues[[tag]]
      if(orig %!=% val)
        convValues[[tag]] = val
    }
  })

  windowWidthUnits = reactive({
    req(showTimeOptions())
    units = input %>% extract2(f('${tag}Units'))
  })


  #----Window Stride----
  output$windowStride = renderUI({
    req(previousData())

    timeUnitsTextInput("windowStride", "Interval Stride",
                       value = isolate(convValues$windowStride))
  })


  windowStride = reactive({
    req(input$windowStrideText)

    extractTimeValue('windowStride')
  })

  #----Aggregate Function----
  aggregateFunctionExpr = reactive({
    agg_fn = input$aggFn
    if(agg_fn == "count") expr(length(.values))
    else if(agg_fn == "mean") expr(if_else(is_empty(.values), 0, mean(.values)))
    else expr(length(.values)/!!(windowWidth()))
  })

  #----Preprocess Changed----
  observe({
    for(si in setdiff(smoothInputs(), "regularizeSubmit"))
      tmp = input[[si]]

    are_smooth_inputs_valid = isSmoothInputValid %>%
      reactiveValuesToList %>%
      as.logical %>% all

    shinyjs::toggleState(
      'regularizeSubmit',
      condition = are_smooth_inputs_valid)

    shinyjs::toggleClass(
      'regularizeSubmit', 'invalid_query',
      condition = !are_smooth_inputs_valid)
  })

  observe({
    req(smoothInputs())

    for(si in smoothInputs())
      tmp = input[[si]]

    text_inputs = isSmoothInputValid %>% names %>%
      purrr::keep(~ str_detect(.x, 'Text'))

    for(id in text_inputs)
      shinyjs::toggleClass(id, 'invalid_query',
                           cond = !isSmoothInputValid[[id]])
  })

  #----Preprocess Input Column----
  valuesColumn = reactive({
    if(isEventFrequencySelected()) NULL
    else input$cpaInputSelect
  })

  #----Return----
  ret = reactiveValues()

  observeEvent(input$regularizeSubmit, {
    ret$window_width = windowWidth()
    ret$agg_fn_expr = aggregateFunctionExpr()
    ret$agg_fn_label = input$aggFn
    ret$window_stride = windowStride()
    ret$do_regularize = doRegularize()
    ret$values_col = valuesColumn()
    shinyjs::disable("regularizeSubmit")
  })

  return(ret)
}
