
customizeDisplayUI = function(id) {
  ns = NS(id)
  actionButton(ns("customizeDisplay"), "Customize Display")
}

customizeDisplayServer = function(input, output, session, data) {
  ns = session$ns
  
  props = list(maxShapeN = 6, maxColorN = 21, maxFacetN = 21)
  
  no_selection = "None"
  
  shapeColumn = reactiveVal(no_selection)
  
  colorColumn = reactiveVal(no_selection)
  
  yColumn = reactiveVal("Event")
  
  facetColumn = reactiveVal(no_selection)
  
  facetOrder = reactiveVal(no_selection)
  
  facetLabels = reactiveVal(no_selection)
  
  facetCustomized = reactiveVal(F)
  
  plotHeight = reactiveVal(400)
  
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
    update = function(valids, getset) 
      if(shouldUpdate(valids, getset()))
        getset(valids[1])
    
    update(validShapeColumns(), shapeColumn)
    
    update(validColorColumns(), colorColumn)
  })
  
  validShapeColumns = reactive({
    req(data())
    
    append(no_selection, validCountColumns(data(), props$maxShapeN))
  })
  
  validYColumns = reactive({
    req(data())
    
    data() %>%
      validColumns(function(co) class(co) %in% c("logical", "numeric")) %>%
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
                  selected = yColumn()),
      sliderInput(ns("plotHeight"), "Plot Height", 
                  value = plotHeight(), min = 20, max = 1000, step = 5),
      selectInput(ns("shapeColumn"), selectText("Shape", props$maxShapeN), 
                  choices = validShapeColumns(),
                  selected = shapeColumn()),
      selectInput(ns("colorColumn"), 
                  selectText(
                    "Color", props$maxColorN, ", or numeric/logical"), 
                  choices = validColorColumns(),
                  selected = colorColumn()),
      fluidRow(
        column(6,
               selectInput(ns("facetColumn"), 
                           selectText("Facet", props$maxFacetN),
                           choices = validFacetColumns(),
                           selected = facetColumn())),
        column(2, uiOutput(ns("facetCustomizeCheck")))
      ),
      uiOutput(ns("facetCustomizeBucket"))
    ))
    
    output$facetCustomizeCheck = renderUI({
      if(input$facetColumn == no_selection) return()
      
      checkboxInput(ns("customizeFacet"), 
                    "Customize", value = facetCustomized())
    })
    
    showFacetCustomizeBucket = reactive({
      if(is.null(input$customizeFacet))
        return(F)
      
      return(input$facetColumn != no_selection & input$customizeFacet)
    })
    
    output$facetCustomizeBucket = renderUI({
      if(!showFacetCustomizeBucket())
        return()

      if(facetColumn() != input$facetColumn)
        facet_values = data()[[input$facetColumn]] %>% 
          unique %>% lapply(
            function(fv) {
              fvc = as.character(fv)
              textInput(ns(fvc), fvc, value = fvc, placeholder = fvc)
            }
          )
      else {
        facet_values = 1:length(facetOrder()) %>%
          lapply(
            function(i) {
              fl = as.character(facetOrder()[i])
              fv = as.character(facetLabels()[i])
              textInput(ns(fl), fl, value = fv)
            }
          )
      }
      
      bucket_list(
        header = "Customize Facet",
        add_rank_list(
          text = "Facet Order & Display Values",
          input_id = ns("facet_list"),
          labels = facet_values
        )
      )
    })
    
    observeEvent(input$modalSubmit, {
      yColumn(input$yColumn)
      plotHeight(input$plotHeight)
      
      shapeColumn(input$shapeColumn)
      colorColumn(input$colorColumn)
      
      facetColumn(input$facetColumn)
      if(showFacetCustomizeBucket()) {
        facetCustomized(T)
        facetOrder(input$facet_list)
        
        input$facet_list %>% 
          lapply(function(f) input[[f]]) %>% 
          as.character %>% 
          facetLabels
      }
      
      removeModal()
    }, ignoreInit = T)
  })
  
  return(list(shapeColumn = shapeColumn, colorColumn = colorColumn, 
              yColumn = yColumn, facetColumn = facetColumn,
              facetOrder = facetOrder,
              facetLabels = facetLabels,
              facetCustomized = facetCustomized,
              plotHeight = plotHeight,
              no_selection = no_selection))
}