
customizeDisplayUI = function(id) {
  ns = NS(id)
  actionButton(ns("customizeDisplay"), "Customize Display")
}

customizeDisplayServer = function(input, output, session, data) {
  ns = session$ns
  
  shapeColumn = reactiveVal()
  
  colorColumn = reactiveVal()
  
  observeEvent(input$customizeDisplay, {
    showModal(modalDialog(
      title = "Display Columns",
      footer = fluidRow(
        actionButton(ns("modalSubmit"), "Submit"),
        modalButton("Cancel")
      ),
      easyClose = T,
      selectInput(ns("shapeColumn"), "Shape", 
                  choices = colnames(data())),
      selectInput(ns("colorColumn"), "Color", 
                  choices = colnames(data()))
    ))
    
    observeEvent(input$shapeColumn, {shapeColumn(input$shapeColumn)})
    
    observeEvent(input$colorColumn, {colorColumn(input$colorColumn)})
  })
  
  return(list(shapeColumn = shapeColumn, colorColumn = colorColumn))
}