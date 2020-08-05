
mainDisplayServer = function(input, output, session){
  ns = session$ns
  
  #------------Data Upload--------
  dataUploadTab = callModule(dataUploadTabServer, "dataUpload")
  data = reactive({dataUploadTab$data()})
  eventsInformation = dataUploadTab$eventsInformation
  headerMinimalInformation = 
    dataUploadTab$headerMinimalInformation
  isDataLoaded = dataUploadTab$isDataLoaded
  
  #------------Data Filter--------
  basicDisplay = callModule(basicDisplayTabServer, "basicDisplay",
                            data, headerMinimalInformation,
                            isDataLoaded)
  dataFilter = basicDisplay$dataFilter
  customizeDisplay = basicDisplay$customizeDisplay
  filteredData = basicDisplay$filteredData
  facetPageN = basicDisplay$facetPageN
  
  #----CPA----
  cpa = callModule(cpaTabServer, "cpa",
                   filteredData, isDataLoaded, 
                   customizeDisplay, facetPageN)
  
  #----CPA Overlay----
  cpaOverlay = callModule(cpaOverlayTabServer, "cpaOverlay",
                          data, isDataLoaded,
                          cpa, customizeDisplay)
  
  #-----Source Code----
  sourceCode <- callModule(sourceCodeServer, "sourcecode", 
			   customizeDisplay, dataFilter,
  			   eventsInformation, isDataLoaded)
}
