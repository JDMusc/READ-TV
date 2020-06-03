
customizeDisplayUI = function(id) {
  ns = NS(id)
  actionButton(ns("customizeDisplay"), "Customize Display")
}


genDefaults = function(no_selection){
  list(no_selection = no_selection,
	     shapeColumn = no_selection,
	     colorColumn = no_selection,
	     yColumn = 'Event',
	     facetColumn = no_selection,
	     facetOrder = no_selection,
	     facetLabels = no_selection,
	     facetCustomized = F,
	     facetRowN = no_selection,
	     facetPage = 1,
	     plotHeight = 400
  )
}

customizeDisplayServer = function(input, output, session, data) {
  ns = session$ns
  
  props = list(maxShapeN = 6, maxColorN = 21, maxFacetN = 500)

  no_selection = "None"
  ret = do.call(reactiveValues, genDefaults(no_selection))
  
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
      validColumns(function(co) class(co) %in% c("logical", 
                                                 "numeric",
                                                 "integer")) %>%
      {append("Event", .)}
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
      fluidRow(
        column(6,
               selectInput(ns("facetColumn"), 
                           selectText("Facet", props$maxFacetN),
                           choices = validFacetColumns(),
                           selected = ret$facetColumn)),
        column(2, uiOutput(ns("facetCustomizeCheck")))
      ),
      uiOutput(ns("facetCustomizeBucket"))
    ))
    
    output$facetCustomizeCheck = renderUI({
      if(input$facetColumn == no_selection) return()
      
      checkboxInput(ns("customizeFacet"), 
                    "Customize", value = ret$facetCustomized)
    })
    
    showFacetCustomizeBucket = reactive({
      if(is.null(input$customizeFacet))
        return(F)
      
      return(input$facetColumn != no_selection & input$customizeFacet)
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
      
      tabsetPanel(
        tabPanel("Pagination",
                 sliderInput(ns("facetRowN"), "Items Per Tab",
                             min = 2, max = 20, value = 10)
                 ),
        tabPanel(
          "Facet Ordering",
          bucket_list(
            header = "Customize Facet",
            add_rank_list(
              text = "Facet Order & Display Values",
              input_id = ns("facet_list"),
              labels = facet_values
            )
          )
        )
      )
    })
    
    observeEvent(input$modalSubmit, {
      ret$yColumn = input$yColumn
      ret$plotHeight = input$plotHeight
      
      ret$shapeColumn = input$shapeColumn
      ret$colorColumn = input$colorColumn
      
      ret$facetColumn = input$facetColumn
      if(showFacetCustomizeBucket()) {
        ret$facetCustomized = T
        ret$facetRowN = input$facetRowN
        ret$facetOrder = input$facet_list
        
        input$facet_list %>% 
          lapply(function(f) input[[f]]) %>% 
          as.character %>% 
	  {ret$facetLabels = .}
      }
      
      removeModal()
    }, ignoreInit = T)
  })
  
  #return(list(shapeColumn = shapeColumn, colorColumn = colorColumn, 
  #            yColumn = yColumn, 
  #            facetColumn = facetColumn,
  #            facetOrder = facetOrder,
  #            facetLabels = facetLabels,
  #            facetCustomized = facetCustomized,
  #            facetPage = facetPage,
  #            facetRowN = facetRowN,
  #            plotHeight = plotHeight,
  #            no_selection = no_selection))
  return(ret)
}
