
basicDisplaySourceGenerationServer = function(input, output, session) {
}

generatePlotSourceCode = function(plot_options, filter_qry, selected_vals, 
                                  f_name, data_name = "events") {
  f = stringr::str_interp
  
  load_f_src = paste(
    f("f_name = \"${f_name}\" #update to your local path"),
    f("${data_name} = loadEventsWithRelativeAndDeltaTime(f_name)"),
    sep = '\n')
  
  events_src = load_f_src
  
  none_selected = selected_vals %>%
    sapply(function(f) any(f %in% 'All')) %>%
    all
  if(none_selected) selected_vals = NULL
  selected = appendToSrc(data_name, "selected", selected_vals, events_src,
                         selectedValsToSourceCode)
  
  filtered = appendToSrc(selected$output_data, "filtered", data_name,
                         filter_qry, selected$src,
                         filterQryToSourceCode)
  
  plot_opts_src = plotOptsToSourceCode(plot_options)
  
  src = paste(
    "library(readtv)",
    filtered$src,
    plot_opts_src,
    f("p = generateTimePlot(${filtered$output}, plot_options)"),
    "plot(p)",
    sep = '\n\n')
  
  return(src)
}