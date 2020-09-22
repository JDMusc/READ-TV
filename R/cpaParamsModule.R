
cpaParamsUI = function(id) {
  ns = NS(id)
  fluidRow(
    column(uiOutput(ns("methodSelect")), width = 2),
    column(uiOutput(ns("penaltySelect")), width = 2),
    column(uiOutput(ns("qSelect")), width = 2),
    actionButton(ns("cpaSubmit"), label = "Calculate CPA")
  )
}

cpaParamsServer = function(input, output, session, cpaData) {
  ns = session$ns

  defaults = generateCpaDefaults()

  #----CPA Method----
  output$methodSelect = renderUI({
    method = defaultOrRet('method')
    selectInput(ns("methodSelect"), "Method", methods, selected = method)
  })

  methods = cpaMethods()

  #----CPA Penalty----
  output$penaltySelect = renderUI({
    penalty = defaultOrRet('penalty')
    selectInput(ns("penaltySelect"), "Penalty", penalties, selected = penalty)
  })

  penalties = cpaPenalties()

  #----Number of Change Points----
  output$qSelect = renderUI({
    req(cpaData())

    grs = dplyr::groups(cpaData())
    if(!is_empty(grs))
      nrow_safe = function(df) dplyr::group_map(df, ~ nrow(.x))
    else
      nrow_safe = nrow

    max_n = cpaData() %>%
      nrow_safe %>%
      as.integer %>%
      max(na.rm = TRUE)

    default = max_n/10 %>%
      as.integer %>%
      {if(. == 0) 1 else .}

    q = defaultOrRet('Q', default)

    selectInput(ns("qSelect"), "# Change Pts", 1:max_n, selected = q)
  })


  #----Can Recalculate----
  observe({
    req(cpaData())

    tmp = input$qSelect
    tmp = input$methodSelect
    tmp = input$penaltySelect
    tmp = input$pen.value
    tmp = cpaData()

    ret$submit_valid = F
    shinyjs::enable('cpaSubmit')
  })

  #----Submit----
  ret = reactiveValues(submit_valid = F)

  defaultOrRet = function(field, def = defaults[[field]])
    getElementSafe(field, ret, def)

  observeEvent(input$cpaSubmit, {
    ret$Q = as.numeric(input$qSelect)
    ret$method = input$methodSelect
    ret$penalty = input$penaltySelect
    ret$pen.value = defaults$pen.value

    updateActionButton(session, "cpaSubmit", label = "Recalculate CPA")
    ret$submit_valid = T
    shinyjs::disable('cpaSubmit')
  })

  return(ret)
}
