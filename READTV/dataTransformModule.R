dataTransformUI = function(id) {
  ns = NS(id)
}


dataTransformServer = function(input, output, session, quickInspect,
                               pre_transform_sym,
                               data_sym) {
  ns = session$ns
  f = stringr::str_interp
  
  un_selected_column_const = -1
  desired_cols = c("Event.Type", "Time", "Case")
  
  #----Column Transforms----
  columnTransforms = desired_cols %>% 
    set_names %>% 
    purrr::map(~ un_selected_column_const) %>% 
    {do.call(reactiveValues, .)}
  
  get_selected = function(col, default_val = un_selected_column_const) 
    getElementSafe(col, columnTransforms, default_val)
  
  
  #----Mock Column Creates----
  mockable_cols = desired_cols %>% setdiff('Time')
  mock_case_val = 1
  mock_event_type_val = 'a'
  columnCreates = reactiveValues('Case' = mock_case_val, 
                                 'Event.Type' = mock_event_type_val)
  doCreateColumn = desired_cols %>% 
    set_names %>% 
    purrr::map(~ .x == 'Case') %>% 
    {do.call(reactiveValues, .)}
  
  #----Return Value----
  mutateCols = reactive({
    cols = list()
    
    for(dst in names(columnTransforms)) {
      new_col = columnTransforms[[dst]]
      
      if(new_col == un_selected_column_const | doCreateColumn[[dst]])
        next
      
      cols[[dst]] = sym(columnTransforms[[dst]])
    }
    
    for(dst in names(columnCreates))
      if(doCreateColumn[[dst]])
        cols[[dst]] = columnCreates[[dst]]
      
    cols
  })
  
  
  #----Code Generation----
  quickInspectPreviewCode = reactive({
    req(quickInspect())
    
    appendColsRhs(expr(data), mutateCols())
  })
  
  
  #----Quick Inspect Preview----
  quickInspectPreview = reactive({
    req(quickInspect())
    
    #mask = list()
    #mask[[expr_text(pre_transform_sym)]] = quickInspect()
    qi = eval_tidy(quickInspectPreviewCode(), data = list(data = quickInspect()))
    
    desired_cols %>% 
      purrr::keep(~ .x %in% names(qi)) %>% 
      purrr::reduce(~ .x %>% select(!!sym(.y), everything()), .init = qi)
  })
  

  quickInspectPreviewColumns = reactive({
    req(quickInspectPreview())
    
    colnames(quickInspectPreview())
  })
  
  
  #----Popup----
  popup = reactiveVal(FALSE)
  
  quickInspectColumns = reactive({
    req(quickInspect())
    
    colnames(quickInspect())
  })
  
  top5PreviewCols = reactive({
    qip_cols = quickInspectPreviewColumns()
    
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
    
    req(quickInspect())
    
    qip = quickInspectPreview()
    qip_cols = quickInspectColumns()
    
    must_chose = 'Must Chose a Column'
    
    time_choices = qip_cols %>% 
      keep(~ is.numeric(qip[[.x]]) | is.timepoint(qip[[.x]])) %>% 
      columnChoices(must_chose, top_selection_val = un_selected_column_const)
    
    showModal(
      modalDialog(
        title = "Data Preview & Load",
        easyClose = FALSE,
        footer = NULL,
        selectInput(ns("rowSkip"), 
                    "Row Skip (assumes top row contains column names)",
                    choices = 0:100,
                    selected = 0),
        selectInput(ns("Time"),
                    "Time Column (must pass is.numeric or lubridate::is.datetime)",
                    choices = time_choices,
                    selected = get_selected('Time', un_selected_column_const)),
        uiOutput(ns("CaseColumnSection")),
        uiOutput(ns("Event.TypeColumnSection")),
        selectInput(ns("columnChoices"), "Columns", colnames(qip), multiple = T,
                    selected = top5PreviewCols()),
        fluidRow(
          column(actionButton(ns("submit"), "Preview"), width = 2, offset = 0),
          column(actionButton(ns("done"), "Done"), width = 2, offset = 0)
        ),
        renderDataTable(qip[, input$columnChoices])
      ))
  })
  
  
  #----Mockable Columns
  data_column_choice = "Data Column"
  mock_value_choice = "Mock Value"
  
  columnTypeStr = function(col) f('${col}ColumnType')
  
  makeMockableInputSection = function(col) {
    fluidRow(
      column(width = 4, 
             selectInput(ns(columnTypeStr(col)), 
                         f("${col} Input"),
                         choices = c(data_column_choice, mock_value_choice),
                         selected = if_else(doCreateColumn[[col]],
                                            mock_value_choice,
                                            data_column_choice
                                            )
             )
      ),
      column(width = 4, uiOutput(ns(f("${col}Column"))))
    )
  }
  
  mockColumnStr = function(col) f('mock${col}')
  mockColumnValue = function(col) input[[mockColumnStr(col)]]
  
  inputColumnType = function(col) input[[columnTypeStr(col)]]
  
  makeMockableInput = function(col, choices){
    make_mock = inputColumnType(col) == mock_value_choice
    
    if(make_mock)
      div(textInput(ns(mockColumnStr(col)),
                    mock_value_choice,
                    value = columnCreates[[col]]))
    else
      selectInput(ns(col),
                  "Column",
                  choices = choices,
                  selected = get_selected(col, NULL))
  }
  
  
  #----Case Column----
  output$CaseColumnSection = renderUI({
    makeMockableInputSection("Case")
  })
  
  output$CaseColumn = renderUI({
    qi = quickInspect()
    makeMockableInput('Case', 
                      choices = quickInspectColumns() %>% 
                        keep(~ n_distinct(qi[[.x]])/nrow(qi) < .2)
                      )
  })
  
  
  #----Event.Type Column----
  output$Event.TypeColumnSection = renderUI({
    makeMockableInputSection("Event.Type")
  })
  
  output$Event.TypeColumn = renderUI({
    qi = quickInspect()
    makeMockableInput('Event.Type', 
                      choices = quickInspectColumns() %>% 
                        keep(~ is.character(qi[[.x]]) | is.factor(qi[[.x]]))
                      )
  })
  
  
  #----Input Checks----
  userSelectedValue = function(col, null_val = un_selected_column_const)
    getElementSafe(col, input, default = null_val)
  
  didUserUpdateColumn = function(col) {
    input_type = inputColumnType(col)
    did_type_change = 
      equals_null_safe(input_type, mock_value_choice) != doCreateColumn[[col]]
    
    if(did_type_change)
      return(TRUE)
    
    if(doCreateColumn[[col]]) {
      user_input = mockColumnValue(col)
      current = columnCreates[[col]]
    } else {
      user_input = userSelectedValue(col)
      current = columnTransforms[[col]]
    }
    
    !equals_null_safe(user_input, current)
  }
  
  areInputsValid = reactive({
    time_valid = 'Time' %>% 
      userSelectedValue %>% 
      not_equals(un_selected_column_const)
    
    time_valid
  })
  
  didAnyInputChange = reactive({
    desired_cols %>% 
      purrr::reduce(~ .x | didUserUpdateColumn(.y), .init = FALSE)
  })
  
  observe({
    valid = areInputsValid()
    change = didAnyInputChange()
    
    shinyjs::toggleState('submit', valid & change)
    shinyjs::toggleState('done', valid & !change)
  })
  
  
  #----Submit----
  observeEvent(input$submit, {
    update_cols = desired_cols %>% 
      keep(didUserUpdateColumn)
    
    for(col in update_cols)
      columnTransforms[[col]] = input[[col]]
    
    for(m in mockable_cols)
      doCreateColumn[[m]] = equals_null_safe(inputColumnType(m), mock_value_choice)
    
    mock_updates = mockable_cols %>% 
      keep(~ doCreateColumn[[.x]]) %>% 
      discard(~ equals_null_safe(mockColumnValue(.x), columnCreates[[.x]]))
    
    for(m in mock_updates)
      columnCreates[[m]] = mockColumnValue(m)
    
    
    shinyjs::disable('submit')
    shinyjs::enable('done')
  })
  
  
  #----Done----
  ready = reactiveVal(FALSE)
  observeEvent(input$done, {
    ready(TRUE)
    removeModal()
  })
  
  return(list(popup = popup, ready = ready, mutateCols = mutateCols))
}