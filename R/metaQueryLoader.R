

metaQueryLoaderUI = function(id) {
  ns = NS(id)
  fileWellUI(ns("filewell"))
}

metaQueryLoader = function(input, output, session) {
  ns = session$ns

  metaDataF = callModule(fileWellServer, "filewell", "Meta Data")

  return(reactive({
    req(metaDataF$fileInfo())

    return(metaDataF$fileInfo())
  }))
}
