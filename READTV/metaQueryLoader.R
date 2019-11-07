library(shiny)
library(shinyjs)

testing = config.testing

metaQueryLoaderUI = function(id) {
  ns = NS(id)
  fileWellUI(ns("filewell"), "Meta Data")
}

metaQueryLoader = function(input, output, session) {
  ns = session$ns
  
  metaDataF = callModule(fileWellServer, "filewell", "Meta Data", '../data/mockMetaData.csv')
  
  return(reactive({
    req(metaDataF())
    
    return(metaDataF())
  }))
}