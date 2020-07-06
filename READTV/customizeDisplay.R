
customizeDisplayUI = function(id) {
  ns = NS(id)
  actionButton(ns("customizeDisplay"), "Customize Display")
}


generatePlotDefaults = function(no_selection){
  list(no_selection = no_selection,
	     shapeColumn = no_selection,
	     colorColumn = no_selection,
	     yColumn = 'Event',
	     xColumn = 'RelativeTime',
	     facetColumn = no_selection,
	     facetOrder = no_selection,
	     facetLabels = no_selection,
	     facetCustomized = F,
       facetPaginated = F,
	     facetRowsPerPage = no_selection,
	     facetPage = 1,
	     plotHeight = 400,
       doStemPlot = T,
       cpaParams = NULL
  )
}


customizeDisplayServer = function(input, output, session, data) {
  ns = session$ns
  
  props = list(maxShapeN = 6, maxColorN = 21, maxFacetN = 500)

  no_selection = "None"
  ret = do.call(reactiveValues, generatePlotDefaults(no_selection))
  
  validColumns = function(df, fn) df %>% select_if(fn) %>% colnames
  
  validCountGen = function(n) function(df, co) length(unique(co)) <= n
  
  validCountColumns = function(df, n) df %>% 
    validColumns(function(co) length(unique(co)) <= n)
  
  shouldUpdate = function(valid_choices, current_choice) {
    should_update = F
    if(is.null(current_choice)) should_update = T
    else should_update = !(current_choice %in% valid_choices)
    
    return(should_update & length(valid_choices) > 0)
  }
  
  observe({
    update = function(valids, current) 
      if(shouldUpdate(valids, current))
        ret[[current]] = valids[1]
    
    update(validShapeColumns(), ret$shapeColumn)
    
    update(validColorColumns(), ret$colorColumn)
  })
  
  validShapeColumns = reactive({
    req(data())
    
    append(no_selection, validCountColumns(data(), props$maxShapeN))
  })
  
  validYColumns = reactive({
    req(data())
    
    data() %>%
      validColumns(function(co) 
        all(
          class(co) %in% c("logical", "numeric", "integer")
        )) %>%
      {append("Event", .)}
  })

  validXColumns = reactive({
    req(data())
    
    data() %>%
      validColumns(function(co) 
        all(
          class(co) %in% c("numeric", "integer", "POSIXct", "POSIXt", "difftime")
          )
      ) 
  })
  
  validColorColumns = reactive({
    req(data())
    
    no_selection %>% 
      append(validCountColumns(data(), props$maxColorN)) %>%
      union(validYColumns())
  })
  
  validFacetColumns = reactive({
    req(data())
    
    append(no_selection, validCountColumns(data(), props$maxFacetN))
  })
  
  observeEvent(input$customizeDisplay, {
    selectText = function(col, maxN, ext = "") 
      paste0(col, " (Max ", maxN, " unique values", ext,")")
    
    showModal(modalDialog(
      title = "Customize Display",
      footer = fluidRow(
        actionButton(ns("modalSubmit"), "Submit"),
        modalButton("Cancel")
      ),
      easyClose = T,
      selectInput(ns("yColumn"), "Y (numeric/logical)",
                  choices = validYColumns(),
                  selected = ret$yColumn),
      selectInput(ns("xColumn"), "X (numeric/date time)",
                  choices = validXColumns(),
                  selected = ret$xColumn),
      sliderInput(ns("plotHeight"), "Plot Height", 
                  value = ret$plotHeight, min = 20, max = 1000, step = 5),
      selectInput(ns("shapeColumn"), selectText("Shape", props$maxShapeN), 
                  choices = validShapeColumns(),
                  selected = ret$shapeColumn),
      selectInput(ns("colorColumn"), 
                  selectText(
                    "Color", props$maxColorN, ", or numeric/logical"), 
                  choices = validColorColumns(),
                  selected = ret$colorColumn),
      checkboxInput(ns("doStemPlot"), "Stem Plot", value = ret$doStemPlot),
      fluidRow(
        column(6,
               selectInput(ns("facetColumn"), 
                           selectText("Facet", props$maxFacetN),
                           choices = validFacetColumns(),
                           selected = ret$facetColumn)),
        column(2,
               fluidRow(uiOutput(ns("facetCustomizeCheck"))),
               fluidRow(uiOutput(ns("facetPaginateCheck"))))
      ),
      fluidRow(uiOutput(ns("facetPaginateBucket"))),
      fluidRow(uiOutput(ns("facetCustomizeBucket")))
    ))
    
    output$facetCustomizeCheck = renderUI({
      if(input$facetColumn == no_selection) return()
      
      checkboxInput(ns("customizeFacet"), 
                    "Customize", value = ret$facetCustomized)
    })
    
    output$facetPaginateCheck = renderUI({
      if(input$facetColumn == no_selection) return()
      
      checkboxInput(ns("paginateFacet"), 
                    "Paginate", value = ret$facetPaginated)
    })
    
    showFacetCustomizeBucket = reactive({
      cf = input$customizeFacet
      if(is.null(cf))
        return(F)
      
      return(input$facetColumn != no_selection & cf)
    })
    
    output$facetCustomizeBucket = renderUI({
      if(!showFacetCustomizeBucket())
        return()

      if(ret$facetColumn != input$facetColumn)
        facet_values = data()[[input$facetColumn]] %>% 
          unique %>% lapply(
            function(fv) {
              fvc = as.character(fv)
              textInput(ns(fvc), fvc, value = fvc, placeholder = fvc)
            }
          )
      else {
        facet_values = 1:length(ret$facetOrder) %>%
          lapply(
            function(i) {
              fl = as.character(ret$facetOrder[i])
              fv = as.character(ret$facetLabels[i])
              textInput(ns(fl), fl, value = fv)
            }
          )
      }
      
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
        return(F)
      
      return(input$facetColumn != no_selection & input$paginateFacet)
    })
    
    output$facetPaginateBucket = renderUI({
      if(!showFacetPaginateBucket())
        return()
      
      sliderInput(ns("facetRowsPerPage"), "Items Per Page",
                  min = 2, max = 20, value = 10)
    })
    
    observeEvent(input$modalSubmit, {
      ret$xColumn = input$xColumn
      ret$yColumn = input$yColumn
      ret$plotHeight = input$plotHeight
      
      ret$shapeColumn = input$shapeColumn
      ret$colorColumn = input$colorColumn
      
      ret$doStemPlot = input$doStemPlot
      
      ret$facetColumn = input$facetColumn
      if(showFacetCustomizeBucket()) {
        ret$facetCustomized = T
        ret$facetOrder = input$facet_list
        
        input$facet_list %>% 
          lapply(function(f) input[[f]]) %>% 
          as.character %>% 
          {ret$facetLabels = .}
      }
      
      if(showFacetPaginateBucket()) {
        ret$facetPaginated = T
        ret$facetRowsPerPage = input$facetRowsPerPage
      }
      
      removeModal()
    }, ignoreInit = T)
  })
  
  return(ret)
}
