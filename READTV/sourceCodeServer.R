
sourceCodeServer = function(input, output, session, 
			    customizeDisplay, dataFilter, doStemPlot,
			    eventsInformation, isDataLoaded){
  ns = session$ns
  
  output$code = renderText({
    if(!isDataLoaded())
       return("This tab will show source code when data is loaded.")
    
    plot_opts = customizeDisplay

    safeFn = function(fn) {
      ret = try(fn(), silent = T)
      if((class(ret) == "try-error"))
        ret = NULL
      ret
    }

    filter_qry = safeFn(dataFilter$query)
    selected_vals = safeFn(dataFilter$selectedVals)
    
    file_name = eventsInformation()$name
    sp = doStemPlot()

    dont_render = is.null(sp)

    if(dont_render) return("")

    generatePlotSourceCode(plot_opts, filter_qry, selected_vals, file_name, 
			   doStemPlot = sp)
  })
}
