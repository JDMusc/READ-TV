testing = config.testing

metaQueryLoaderUI = function(id) {
  ns = NS(id)
  uiOutput(ns("loadMetaData"))
}

metaQueryLoader = function(input, output, session) {
  ns = session$ns
  
  output$loadMetaData = renderUI({
    if(testing)
      actionButton(inputId = ns("loadMetaDataTest"), label = "Load Meta Data")
    else
      fileInput(ns("loadMetaData"), "Load Meta Data", accept = '.csv')
  })
  
  metaDataFileTest = eventReactive(input$loadMetaDataTest, {
    '../data/mockMetaData.csv'
  })
  
  return(reactive({
    if(testing){
      req(metaDataFileTest())
      
      mdf = metaDataFileTest()
      liste(name = mdf, path = mdf)
    } else {
      req(input$loadMetaData)
      
      input$loadMetaData
    }
  }))
}