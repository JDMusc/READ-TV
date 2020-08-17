
mainDisplayServer = function(input, output, session){
  ns = session$ns
  
  #------------Data Upload--------
  dataUploadOutputSym = sym('data')
  dataUploadTab = callModule(dataUploadTabServer, "dataUpload",
                             dataUploadOutputSym)
  data = reactive({dataUploadTab$data()})
  eventsInformation = dataUploadTab$eventsInformation
  fileName = dataUploadTab$fileName
  isDataLoaded = dataUploadTab$isDataLoaded
  
  #------------Data Filter--------
  basicDisplayOutputSym = sym('filtered_data')
  basicDisplayTab = callModule(basicDisplayTabServer, 
                               "basicDisplay", 
                               data, fileName, isDataLoaded,
                               dataUploadTab$mySourceString,
                               input_sym = dataUploadOutputSym,
                               output_sym = basicDisplayOutputSym)
  dataFilter = basicDisplayTab$dataFilter
  customizeDisplay = basicDisplayTab$customizeDisplay
  filteredData = basicDisplayTab$filteredData
  facetPageN = basicDisplayTab$facetPageN
  
  #----CPA----
  cpa = callModule(cpaTabServer, "cpa",
                   filteredData, isDataLoaded, 
                   customizeDisplay, facetPageN,
                   basicDisplayTab$mySourceString,
                   input_sym = basicDisplayOutputSym)
  
  #----CPA Overlay----
  cpaOverlay = callModule(cpaOverlayTabServer, "cpaOverlay",
                          data, isDataLoaded,
                          cpa, customizeDisplay)
  
  #-----Source Code----
  sourceCode <- callModule(sourceCodeServer, "sourcecode", 
			   customizeDisplay, dataFilter,
  			   eventsInformation, isDataLoaded)
}
