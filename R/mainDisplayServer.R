
mainDisplayServer = function(input, output, session, eventsPath = NULL,
                             inputData = NULL, initPlotOpts = list()){
  ns = session$ns

  #------------Data Upload--------
  dataUploadOutputSym = sym('rtv_data')
  dataUploadTab = callModule(dataUploadTabServer, "dataUpload",
                             eventsPath, inputData, dataUploadOutputSym)
  uploadCode = dataUploadTab$code
  initMask = dataUploadTab$mask
  fileName = dataUploadTab$fileName
  isDataLoaded = dataUploadTab$isDataLoaded
  isFilePassed = is.null(inputData)

  #------------Data Filter--------
  basicDisplayOutputSym = sym('filtered_data')
  basicDisplayTab = callModule(basicDisplayTabServer,
                               "basicDisplay",
                               uploadCode, initMask,
                               fileName,
                               isDataLoaded, isFilePassed,
                               initPlotOpts = initPlotOpts,
                               input_sym = dataUploadOutputSym,
                               output_sym = basicDisplayOutputSym)
  customizeDisplay = basicDisplayTab$customizeDisplay
  filteredData = basicDisplayTab$filteredData
  facetPageN = basicDisplayTab$facetPageN

  #----CPA----
  cpaMarkersSym = sym("cpa_markers")
  cpa = callModule(cpaTabServer, "cpa",
                   filteredData, fileName,
                   isDataLoaded, isFilePassed,
                   customizeDisplay, facetPageN,
                   basicDisplayTab$fullSourceString,
                   input_sym = basicDisplayOutputSym,
                   cpa_markers_sym = cpaMarkersSym)

  #----CPA Overlay----
  cpaOverlay = callModule(cpaOverlayTabServer, "cpaOverlay",
                          uploadCode, initMask, isDataLoaded,
                          cpa, customizeDisplay,
                          cpa$fullSourceString,
                          input_sym = dataUploadOutputSym,
                          cpa_markers_sym = cpaMarkersSym)
}
