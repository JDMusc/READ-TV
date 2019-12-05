
customizeDisplayUI = function(id) {
  ns = NS(id)
  actionButton(ns("customizeDisplay"), "Customize Display")
}

customizeDisplayServer = function(input, output, session, data) {
  ns = session$ns
  
  shapeColumn = reactiveVal()
  
  colorColumn = reactiveVal()
  
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
  
  observeEvent(input$customizeDisplay, {
    hi = validColorColumns()
    showModal(modalDialog(
      title = "Display Columns",
      footer = fluidRow(
        actionButton(ns("modalSubmit"), "Submit"),
        modalButton("Cancel")
      ),
      easyClose = T,
      selectInput(ns("shapeColumn"), "Shape", 
                  choices = validShapeColumns(),
                  selected = shapeColumn()),
      selectInput(ns("colorColumn"), "Color", 
                  choices = validColorColumns(),
                  selected = colorColumn())
    ))
    
    observeEvent(input$modalSubmit, {
      shapeColumn(input$shapeColumn)
      colorColumn(input$colorColumn)
      
      removeModal()
    }, ignoreInit = T)
  })
  
  return(list(shapeColumn = shapeColumn, colorColumn = colorColumn))
}