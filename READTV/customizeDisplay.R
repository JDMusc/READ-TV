
customizeDisplayUI = function(id) {
  ns = NS(id)
  actionButton(ns("customizeDisplay"), "Customize Display")
}

customizeDisplayServer = function(input, output, session, data) {
  ns = session$ns
  
  shapeColumn = reactiveVal()
  
  colorColumn = reactiveVal()
  
  yColumn = reactiveVal("Event")
  
  facetColumn = reactiveVal("None")
  
  validColumns = function(df, fn) df %>% select_if(fn) %>% colnames
  
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
    
    validColumns(data(), 
                 function(co) length(unique(co)) < 7)
  })
  
  validColorColumns = reactive({
    req(data())

    validColumns(data(), 
                 function(co) length(unique(co)) < 101 | class(co) != "character")
  })
  
  validYColumns = reactive({
    req(data())
    
    data() %>%
      validColumns(function(co) class(co) %in% c("logical", "numeric")) %>%
      {append("Event", .)}
  })
  
  validFacetColumns = reactive({
    req(data())
    
    append("None", validShapeColumns())
  })
  
  observeEvent(input$customizeDisplay, {
    showModal(modalDialog(
      title = "Display Columns",
      footer = fluidRow(
        actionButton(ns("modalSubmit"), "Submit"),
        modalButton("Cancel")
      ),
      easyClose = T,
      selectInput(ns("yColumn"), "Y",
                  choices = validYColumns(),
                  selected = yColumn()),
      selectInput(ns("shapeColumn"), "Shape", 
                  choices = validShapeColumns(),
                  selected = shapeColumn()),
      selectInput(ns("colorColumn"), "Color", 
                  choices = validColorColumns(),
                  selected = colorColumn()),
      selectInput(ns("facetColumn"), "Facet",
                  choices = validFacetColumns(),
                  selected = facetColumn())
      
    ))
    
    observeEvent(input$modalSubmit, {
      yColumn(input$yColumn)
      shapeColumn(input$shapeColumn)
      colorColumn(input$colorColumn)
      facetColumn(input$facetColumn)
      
      removeModal()
    }, ignoreInit = T)
  })
  
  return(list(shapeColumn = shapeColumn, colorColumn = colorColumn, 
              yColumn = yColumn, facetColumn = facetColumn))
}