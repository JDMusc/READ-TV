dataTransformUI = function(id) {
  ns = NS(id)
}


dataTransformServer = function(input, output, session,
                               rawDataCodes,
                               rawDataMask,
                               pre_transform_sym,
                               data_sym) {
  ns = session$ns
  f = stringr::str_interp

  location = function(msg) f('dataTransformModule, ${msg}')

  un_selected_column_const = -1

  missingCols = reactive({
    req(rawData())

    current_cols = colnames(rawData())
    desired_cols = c("Event.Type", "Time", "Case")

    setdiff(desired_cols, current_cols)
  })

  #----Pre Transform Data----
  rawData = reactive({
    rawDataCodes() %>%
      runExpressionsLast(rawDataMask(), location = location('rawData'))
  })

  #----Column Transforms----
  columnTransforms = reactiveValues()

  observe({
    req(missingCols())

    for(n in missingCols())
      columnTransforms[[n]] = un_selected_column_const
  })

  get_selected = function(col, default_val = un_selected_column_const)
    getElementSafe(col, columnTransforms, default_val)

  #----Optional Column Creates----
  optionalCols = reactive({
    req(missingCols())

    missingCols() %>% setdiff('Time')
  })


  #----Return Value----
  mutateCols = reactive({
    cols = list()

    for(dst in names(columnTransforms)) {
      new_col = columnTransforms[[dst]]

      if(new_col == un_selected_column_const)
        next

      cols[[dst]] = sym(columnTransforms[[dst]])
    }

    cols
  })


  #----Code Generation----
  outputCode = reactive({
    req(rawData())

    pre_codes = rawDataCodes()
    c_code = appendCols(pre_transform_sym, mutateCols(), data_sym)

    c_code = rlang::set_names(list(c_code),
                              list(data_sym))

    c(pre_codes, c_code)
  })


  #----Quick Inspect Preview----
  transformDataPreview = reactive({
    req(rawData())

    #mask = list(expr_text(pre_transform_sym)) %>%
    #  rlang::set_names(list(rawData()), nm = .)

    qi = runExpressionsLast(
      outputCode(),
      mask = rawDataMask(),
      location = location('transformDataPreview'))

    missingCols() %>%
      purrr::keep(~ .x %in% names(qi)) %>%
      purrr::reduce(~ .x %>% select(!!sym(.y), dplyr::everything()),
                    .init = qi)
  })


  transformDataPreviewColumns = reactive({
    req(transformDataPreview())

    colnames(transformDataPreview())
  })


  #----Popup----
  popup = reactiveVal(FALSE)

  rawDataColumns = reactive({
    req(rawData())

    colnames(rawData())
  })

  top5PreviewCols = reactive({
    qip_cols = transformDataPreviewColumns()

    if(length(qip_cols) < 6)
      qip_cols
    else
      qip_cols[1:5]
  })

  columnChoices = function(columns,
                           top_selection_txt, top_selection_val)
    c(top_selection_val %>% set_names(nm = top_selection_txt),
      columns %>% set_names)


  observe({
    if(!popup())
      return()

    req(rawData())

    showModal(modalDialog(
      title = "Data Preview & Load",
      easyClose = FALSE,
      footer = NULL,
      tabsetPanel(
        tabPanel(
          "Data Transform",
          uiOutput(ns("dataTransformTab"))
          ),
        tabPanel(
          "Glimpse into Data",
          dataGlimpseUI(ns("dataGlimpse"))
          ),
        tabPanel(
          'Source Code',
          verbatimTextOutput(ns("sourceCodeView"))
        )
        )
      )
    )
  })

  #----Data Glimpse Tab----
  dataGlimpse = callModule(dataGlimpseServer, 'dataGlimpse', transformDataPreview)


  #----Data Transform Tab----
  output$dataTransformTab = renderUI({
    req(rawData())

    qip = transformDataPreview()

    div(
      uiOutput(ns("TimeColumnSection")),
      fluidPage(
        fluidRow(strong('Optional Columns')),
        fluidRow(
          column(uiOutput(ns("CaseColumn")), width = 4),
          column(uiOutput(ns("Event.TypeColumn")), width = 4)
        )
      ),
      selectInput(
        ns("columnChoices"),
        "Preview Columns (all columns will be loaded)",
        colnames(qip),
        multiple = TRUE,
        selected = top5PreviewCols()
      ),
      actionButton(ns("done"), "Submit"),
      uiOutput(ns("dataTblPreview"))
    )
  })

  output$dataTblPreview = renderUI({
    req(rawData())

    qip = transformDataPreview()

    valid_cols = intersect(input$columnChoices,
                           colnames(qip))

    div(
      renderDataTable(qip[, valid_cols])
    )
  })

  #----Source Code View----
  output$sourceCodeView = renderText({
    expressionsToString(outputCode())
  })


  #----Optional Columns----
  data_column_choice = "Data Column"
  optional_value_choice = "No Column"

  optionalColumnStr = function(col) f('optional${col}')
  optionalColumnValue = function(col) input[[optionalColumnStr(col)]]

  makeOptionalInput = function(col, keep_fn){
    if(col %not in% missingCols()) return()

    qi = rawData()

    choices = rawDataColumns() %>%
      keep(~ keep_fn(qi, .x)) %>%
      columnChoices("None", un_selected_column_const)

    selectInput(ns(col),
                f("${col}"),
                choices = choices,
                selected = get_selected(col, NULL))
  }


  #----Case Column----
  output$CaseColumn = renderUI({
    makeOptionalInput('Case', validCaseCol)
  })


  #----Event.Type Column----
  output$Event.TypeColumn = renderUI({
    makeOptionalInput('Event.Type', validEventTypeCol)
  })


  #----Time Column----
  output$TimeColumnSection = renderUI({
    req(rawData())

    qip = transformDataPreview()
    qip_cols = rawDataColumns()

    must_choose = 'Must Choose a Column'

    time_choices = qip_cols %>%
      keep(~ is.numeric(qip[[.x]]) | is.timepoint(qip[[.x]])) %>%
      columnChoices(must_choose, top_selection_val = un_selected_column_const)

    if('Time' %in% missingCols())
      selectInput(
        ns("Time"),
        "Time Column (must pass is.numeric or lubridate::is.datetime)",
        choices = time_choices,
        selected = get_selected('Time', un_selected_column_const)
      )
  })


  #----Input Checks----
  userSelectedValue = function(col, null_val = un_selected_column_const)
    getElementSafe(col, input, default = null_val)

  didUserUpdateColumn = function(col) {
    user_input = userSelectedValue(col)
    current = columnTransforms[[col]]

    user_input %!=% current
  }

  areInputsValid = reactive({
    time_present = 'Time' %not in% missingCols()

    time_valid = if(time_present)
      TRUE
    else 'Time' %>%
      userSelectedValue %>%
      not_equals(un_selected_column_const)

    time_valid
  })

  didAnyInputChange = reactive({

    missingCols() %>%
      purrr::reduce(~ .x | didUserUpdateColumn(.y), .init = FALSE)
  })

  observe({
    shinyjs::toggleState('done', areInputsValid())
  })


  #----Preview----
  observe({
    update_cols = missingCols() %>%
      keep(didUserUpdateColumn)

    for(col in update_cols)
      columnTransforms[[col]] = input[[col]]
  })


  #----Done----
  ready = reactiveVal(FALSE)
  observeEvent(input$done, {
    ready(TRUE)
    removeModal()
  })

  return(list(popup = popup,
              ready = ready,
              code = outputCode))
}
