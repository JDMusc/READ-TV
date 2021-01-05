dataDownloadUI = function(id) {
  ns = NS(id)
  uiOutput(ns("downloadDataOutput"))
}

dataDownloadServer = function(input, output, session, data, fileName,
                              isDataLoaded, isFilePassed, suffix) {
  ns = session$ns

  output$downloadDataOutput = renderUI({
    if(isDataLoaded())
      fluidRow(
        column(width = 3,
               downloadButton(ns("downloadCsvData"),
                              label = downloadFileName('csv'))),
        column(width = 3,
               downloadButton(ns("downloadRdsData"),
                              label = downloadFileName('RDS')))
      )
  })

  downloadFileName = function(ext)
    if_else(isFilePassed,
        f("${fs::path_ext_remove(fileName())}-${suffix}"),
        suffix) %>%
    fs::path_ext_set(ext)

  writeFnGen = function(ext) {
    fn = if(ext == 'csv')
      write_csv
    else if(ext == 'tsv')
      write_tsv
    else
      write_rds

    function(f) fn(data(), f)
  }

  genDownloadHandler = function(ext)
    downloadHandler(
      filename = downloadFileName(ext),
      content = writeFnGen(ext)
    )

  output$downloadCsvData <- genDownloadHandler('csv')

  output$downloadRdsData <- genDownloadHandler('RDS')
}
