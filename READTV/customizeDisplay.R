
customizeDisplayUI = function(id) {
  ns = NS(id)
  actionButton(ns("customizeDisplay"), "Customize Display")
}

customizeDisplayServer = function(input, output, session, data) {
  ns = session$ns
  
  props = list(maxShapeN = 6, maxColorN = 21, maxFacetN = 21)
  
  shapeColumn = reactiveVal()
  
  colorColumn = reactiveVal()
  
  yColumn = reactiveVal("Event")
  
  facetColumn = reactiveVal("None")
  
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
    
    validCountColumns(data(), props$maxShapeN)
  })
  
  validYColumns = reactive({
    req(data())
    
    data() %>%
      validColumns(function(co) class(co) %in% c("logical", "numeric")) %>%
      {append("Event", .)}
  })
  
  validColorColumns = reactive({
    req(data())
    
    union(validCountColumns(data(), props$maxColorN),
          validYColumns())
  })
  
  validFacetColumns = reactive({
    req(data())
    
    append("None", validCountColumns(data(), props$maxFacetN))
  })
  
  observeEvent(input$customizeDisplay, {
    selectText = function(col, maxN, ext = "") 
      paste0(col, " (Max ", maxN, " unique values", ext,")")
    
    showModal(modalDialog(
      title = "Display Columns",
      footer = fluidRow(
        actionButton(ns("modalSubmit"), "Submit"),
        modalButton("Cancel")
      ),
      easyClose = T,
      selectInput(ns("yColumn"), "Y (numeric/logical)",
                  choices = validYColumns(),
                  selected = yColumn()),
      selectInput(ns("shapeColumn"), selectText("Shape", props$maxShapeN), 
                  choices = validShapeColumns(),
                  selected = shapeColumn()),
      selectInput(ns("colorColumn"), 
                  selectText(
                    "Color", props$maxColorN, ", or numeric/logical"), 
                  choices = validColorColumns(),
                  selected = colorColumn()),
      selectInput(ns("facetColumn"), selectText("Facet", props$maxFacetN),
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