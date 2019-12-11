

showSourceServer = function(input, output, session) {
  
  showSource = function(data, default_n = 3){
    d = data
    ns = session$ns
    
    defaultCols = reactive({
      default = colnames(d)
      if(length(default) > default_n)
        default = default[1:default_n]
      
      return(default)
    })
    
    showModal(modalDialog(
      title = "Events",
      selectInput(ns("columnChoices"), "Columns", colnames(d), multiple = T,
                  selected = defaultCols()),
      renderDataTable(d[, input$columnChoices]),
      easyClose = TRUE,
      size = "m"
    ))
  }
  
  return(showSource)
}