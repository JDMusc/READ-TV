
mainDisplayServer = function(input, output, session, eventsPath = NULL,
                             inputData = NULL, initPlotOpts = list()){
  ns = session$ns
  
  #------------Data Upload--------
  dataUploadOutputSym = sym('data')
  dataUploadTab = callModule(dataUploadTabServer, "dataUpload", 
                             eventsPath, inputData, dataUploadOutputSym)
  data = dataUploadTab$data
  fileName = dataUploadTab$fileName
  isDataLoaded = dataUploadTab$isDataLoaded
  
  #------------Data Filter--------
  basicDisplayOutputSym = sym('filtered_data')
  basicDisplayTab = callModule(basicDisplayTabServer, 
                               "basicDisplay", 
                               data, fileName, isDataLoaded,
                               dataUploadTab$fullSourceString,
                               initPlotOpts = initPlotOpts,
                               input_sym = dataUploadOutputSym,
                               output_sym = basicDisplayOutputSym)
  dataFilter = basicDisplayTab$dataFilter
  customizeDisplay = basicDisplayTab$customizeDisplay
  filteredData = basicDisplayTab$filteredData
  facetPageN = basicDisplayTab$facetPageN
  
  #----CPA----
  cpaMarkersSym = sym("cpa_markers")
  cpa = callModule(cpaTabServer, "cpa",
                   filteredData, isDataLoaded, 
                   customizeDisplay, facetPageN,
                   basicDisplayTab$fullSourceString,
                   input_sym = basicDisplayOutputSym,
                   cpa_markers_sym = cpaMarkersSym)
  
  #----CPA Overlay----
  cpaOverlay = callModule(cpaOverlayTabServer, "cpaOverlay",
                          data, isDataLoaded,
                          cpa, customizeDisplay,
                          cpa$fullSourceString,
                          input_sym = dataUploadOutputSym,
                          cpa_markers_sym = cpaMarkersSym)
}
