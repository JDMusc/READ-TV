
customizeDisplayUI = function(id) {
  ns = NS(id)
  actionButton(ns("customizeDisplay"), "Customize Display")
}


generatePlotDefaults = function(overrides = list()){
  anyEvent = getElementSafe(overrides, anyEvent, 'Any Event')
  ret = list(
    anyEvent = anyEvent,
    shape = NULL,
    color = NULL,
    y = anyEvent,
    x = 'Time',
    facetOn = NULL,
    facetOrder = NULL,
    facetLabels = NULL,
    isFacetCustomized = F,
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
  
  props = list(maxShapeN = 6, maxColorN = 21, maxFacetN = 500)
  
  f = stringr::str_interp

  ret = do.call(reactiveValues, generatePlotDefaults(initPlotOpts))
  
  validColumns = function(df, fn) df %>% select_if(fn) %>% colnames
  
  numberLikeColumns = function(df) df %>% 
    validColumns(~ is_logical(.x) | is_double(.x) |  is_integer(.x))
  
  validCountColumns = function(df, n) df %>% 
    validColumns(~ n_distinct(.x) <= n)
  
  shouldUpdate = function(valid_choices, current_choice) {
    if(length(valid_choices) == 0) 
      FALSE
    else if(is.null(current_choice)) 
      TRUE
    else 
      current_choice %not in% valid_choices
  }
  
  observe({
    updateRet = function(valids, col) 
      if(shouldUpdate(valids, ret[[col]]))
        ret[[col]] = valids[1]
    
    updateRet(validShapeColumns(), 'shape')
    
    updateRet(validColorColumns(), 'color')
  })
  
  validShapeColumns = reactive({
    req(data())
    
    append(empty_str, validCountColumns(data(), props$maxShapeN))
  })
  
  
  validYColumns = reactive({
    req(data())
    
    data() %>%
      numberLikeColumns %>%
      append(empty_str, .)
  })

  validXColumns = reactive({
    req(data())
    
    data() %>%
      validColumns(~ is.difftime(.x) | 
                     is_double(.x)   |
                     is_integer(.x)  |
                     is_logical(.x)  |
                     is.timepoint(.x))
  })
  
  validColorColumns = reactive({
    req(data())
    
    d = data()
    empty_str %>% 
      append(validCountColumns(d, props$maxColorN)) %>%
      union(numberLikeColumns(d))
  })
  
  validFacetColumns = reactive({
    req(data())
    
    data() %>% 
      validCountColumns(props$maxFacetN) %>% 
      append(empty_str, .)
  })
  
  observeEvent(input$customizeDisplay, {
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
               fluidRow(uiOutput(ns("facetCustomizeCheck"))),
               fluidRow(uiOutput(ns("facetPaginateCheck"))))
      ),
      fluidRow(uiOutput(ns("facetPaginateBucket"))),
      fluidRow(uiOutput(ns("facetCustomizeBucket")))
    ))
    
    doFacet = reactive({
      is_str_set(input$facetOn)
    })
    
    output$facetCustomizeCheck = renderUI({
      if(doFacet())
        checkboxInput(ns("customizeFacet"), 
                      "Customize", value = ret$isFacetCustomized)
    })
    
    output$facetPaginateCheck = renderUI({
      if(doFacet())
        checkboxInput(ns("paginateFacet"), 
                      "Paginate", value = ret$isFacetPaginated)
    })
    
    showFacetCustomizeBucket = reactive({
      cf = input$customizeFacet
      if(is.null(cf))
        FALSE
      else
        doFacet() & cf
    })
    
    output$facetCustomizeBucket = renderUI({
      if(!showFacetCustomizeBucket())
        return()

      makeTextInput = function(id, label = id, value = as.character(label))
        textInput(ns(as.character(id)), 
                  label, value = value, placeholder = value)
      
      if(ret$facetOn != input$facetOn)
        facet_values = data() %>% 
          extract2(input$facetOn) %>% 
          unique %>% 
          purrr::map(makeTextInput)
      else
        facet_values = 1:length(ret$facetOrder) %>%
          purrr::map(~ makeTextInput(ret$facetOrder[[.x]],
                                     label = ret$facetLabels[.x])
          )
      
      div(bucket_list(
        header = "Customize Facet",
        add_rank_list(
          text = "Facet Order & Display Values",
          input_id = ns("facet_list"),
          labels = facet_values
        )
      ))
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
    
    observeEvent(input$modalSubmit, {
      ret$x = input$x
      ret$y = input$y
      ret$plotHeight = input$plotHeight
      
      ret$shape = input$shape
      ret$color = input$color
      
      ret$doStemPlot = input$doStemPlot
      
      ret$facetOn = input$facetOn
      if(showFacetCustomizeBucket()) {
        ret$isFacetCustomized = TRUE
        ret$facetOrder = input$facet_list
        
        input$facet_list %>% 
          purrr::map(~ as.character(input[[.x]])) %>% 
          {ret$facetLabels = as.array(.)}
      }
      
      if(showFacetPaginateBucket()) {
        ret$isFacetPaginated = TRUE
        ret$facetRowsPerPage = input$facetRowsPerPage
      }
      
      removeModal()
    }, ignoreInit = T)
  })
  
  return(ret)
}
