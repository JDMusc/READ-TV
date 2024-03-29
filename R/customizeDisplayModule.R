
customizeDisplayUI = function(id) {
  ns = NS(id)
  actionButton(ns("customizeDisplay"), "Customize Display")
}

#' Generate Plot Defaults for Basic Time Plot
#'
#' This function is for internal use. It is exported so that users can view key-value pairs used for plotting. Reveals key names and value defaults. See \code{help(tvOpts)}.
#'
#' See \code{help(tvOpts)} for more information. Should not be used for other reasons besides viewing plot defaults and param names.
#'
#' @export
#'
#' @examples
#' generatePlotDefaults()
generatePlotDefaults = function(overrides = list()){
  anyEvent = getElementSafe('anyEvent', overrides, 'Any Event')
  ret = list(
    anyEvent = anyEvent,
    shape = NULL,
    color = NULL,
    alpha = NULL,
    alpha_pass = 1,
    alhpa_out = .2,
    y = anyEvent,
    x = 'Time',
    facetOn = NULL,
    facetOrder = NULL,
    facetLabels = NULL,
    isFacetPaginated = F,
    facetRowsPerPage = NULL,
    facetPage = 1,
    plotHeight = 400,
    doStemPlot = TRUE,
    geomFunction = "geom_point"
    )

  ret %>%
    purrr::map2(names(ret),
                ~ getElementSafe(.y, overrides, ret[[.y]]))
}

empty_str = ""
displayEmptyStrAsAnyEvent = function(cols, anyEvent) {
  cols %>%
    purrr::map_if(~ .x == empty_str, ~ anyEvent) %>%
    set_names(cols, nm = .)
}


displayEmptyStrAsNone = function(cols) {
  cols %>%
    purrr::map_if(is_empty_str, ~ "None") %>%
    set_names(cols, nm = .)
}


customizeDisplayServer = function(input, output, session, data,
                                  initPlotOpts = list()) {
  ns = session$ns
  f = stringr::str_interp
  ret = do.call(reactiveValues, generatePlotDefaults(initPlotOpts))

  location = function(msg) f('customizeDisplayServer ${msg}')

  log_info_cd = log_info_module_gen('customizeDisplayServer')
  req_log = req_log_gen(log_info_cd)


  props = list(maxShapeN = 6, maxColorN = 21, maxFacetN = 500)

  #----Valid Columns----

  validColumns = function(df, fn) df %>% dplyr::select_if(fn) %>% colnames

  numberLikeColumns = function(df) df %>%
    validColumns(~ is_logical(.x) | is_double(.x) |  is_integer(.x))

  validCountColumns = function(df, n) df %>%
    validColumns(~ n_distinct(.x) <= n)

  validShapeColumns = reactive({
    req_log('validShapeColumns', quo(data()))

    append(empty_str, validCountColumns(data(), props$maxShapeN))
  })


  validYColumns = reactive({
    req_log('validYColumns', quo(data()))

    data() %>%
      numberLikeColumns %>%
      append(empty_str, .)
  })

  validXColumns = reactive({
    req_log('validXColumns', quo(data()))

    data() %>%
      validColumns(~ is.difftime(.x) |
                     is_double(.x)   |
                     is_integer(.x)  |
                     is_logical(.x)  |
                     is.timepoint(.x))
  })

  validColorColumns = reactive({
    req_log('validColorColumns', quo(data()))

    d = data()
    empty_str %>%
      append(validCountColumns(d, props$maxColorN)) %>%
      union(numberLikeColumns(d))
  })

  validFacetColumns = reactive({
    req_log('validFacetColumns', quo(data()))

    data() %>%
      validCountColumns(props$maxFacetN) %>%
      append(empty_str, .)
  })

  #----Update----
  shouldUpdate = function(valid_choices, current_choice) {
    log_info_cd('shouldUpdate')

    if(length(valid_choices) == 0)
      FALSE
    else if(is.null(current_choice))
      TRUE
    else
      current_choice %not in% valid_choices
  }

  observe({
    log_info_cd('observe')

    updateRet = function(valids, col)
      if(shouldUpdate(valids, ret[[col]]))
        ret[[col]] = valids[1]

    updateRet(validShapeColumns(), 'shape')

    updateRet(validColorColumns(), 'color')
  })


  #----Modal----
  observeEvent(input$customizeDisplay, {
    log_info_cd('observe input$customizeDisplay')

    selectText = function(col, maxN, ext = "")
      f("${col} (Max  ${maxN} unique values)${ext}")

    showModal(modalDialog(
      title = "Customize Display",
      footer = fluidRow(
        actionButton(ns("modalSubmit"), "Submit"),
        modalButton("Cancel")
      ),
      easyClose = TRUE,
      selectInput(ns("y"), "Y (numeric/logical)",
                  choices = validYColumns(),
                  selected = ret$y),
      selectInput(ns("x"), "X (numeric/date time)",
                  choices = validXColumns(),
                  selected = ret$x),
      sliderInput(ns("plotHeight"), "Plot Height",
                  value = ret$plotHeight, min = 20, max = 1000, step = 5),
      selectInput(ns("shape"),
                  selectText("Shape", props$maxShapeN),
                  choices = displayEmptyStrAsNone(validShapeColumns()),
                  selected = ret$shape),
      selectInput(ns("color"),
                  selectText(
                    "Color", props$maxColorN, ", or numeric/logical"),
                  choices = displayEmptyStrAsNone(validColorColumns()),
                  selected = ret$color),
      checkboxInput(ns("doStemPlot"), "Stem Plot", value = ret$doStemPlot),
      fluidRow(
        column(6,
               selectInput(ns("facetOn"),
                           selectText("Facet", props$maxFacetN),
                           choices =
                             displayEmptyStrAsNone(validFacetColumns()),
                           selected = ret$facetOn)),
        column(2,
               fluidRow(uiOutput(ns("facetPaginateCheck"))))
      ),
      fluidRow(uiOutput(ns("facetPaginateBucket")))
    ))

    doFacet = reactive({
      is_str_set(input$facetOn)
    })

    output$facetPaginateCheck = renderUI({
      if(doFacet())
        checkboxInput(ns("paginateFacet"),
                      "Paginate", value = ret$isFacetPaginated)
    })

    showFacetPaginateBucket = reactive({
      if(is.null(input$paginateFacet))
        FALSE
      else
        doFacet() & input$paginateFacet
    })

    output$facetPaginateBucket = renderUI({
      if(!showFacetPaginateBucket())
        return()

      sliderInput(ns("facetRowsPerPage"), "Items Per Page",
                  min = 2, max = 20,
                  value = ifelse(is_empty_str(ret$facetRowsPerPage),
                                 10, ret$facetRowsPerPage))
    })

    #----Submit----

    observeEvent(input$modalSubmit, {
      log_info_cd('observe input$modalSubmit')

      ret$x = input$x
      ret$y = input$y
      ret$plotHeight = input$plotHeight

      ret$shape = input$shape
      ret$color = input$color

      ret$doStemPlot = input$doStemPlot

      ret$facetOn = input$facetOn

      ret$isFacetPaginated = showFacetPaginateBucket()
      if(ret$isFacetPaginated) {
        ret$facetRowsPerPage = input$facetRowsPerPage
      }

      removeModal()
    }, ignoreInit = T)
  })

  #----Return----
  ret
}
