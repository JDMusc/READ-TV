

metaQueryLoaderUI = function(id) {
  ns = NS(id)
  fileWellUI(ns("filewell"))
}

metaQueryLoader = function(input, output, session) {
  ns = session$ns
  
  metaDataF = callModule(fileWellServer, "filewell", "Meta Data", '../data/mockMetaData.csv')
  
  return(reactive({
    req(metaDataF())
    
    return(metaDataF())
  }))
}
