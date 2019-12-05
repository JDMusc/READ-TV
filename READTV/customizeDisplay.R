
customizeDisplayUI = function(id) {
  ns = NS(id)
  actionButton(ns("customizeDisplay"), "Customize Display")
}

customizeDisplayServer = function(input, output, session, data) {
  ns = session$ns
  
  shapeColumn = reactiveVal("Phase")
  
  colorColumn = reactiveVal("Event.Type")
  
  validColumns = function(df, fn) df %>% select_if(fn) %>% colnames
  
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