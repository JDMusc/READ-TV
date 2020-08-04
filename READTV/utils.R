isSelected = function(choice) any(choice != c("All"))
selectableChoices = function(choices) c("All" = "All", choices)


#https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
eventTypeColors <- function() {
  types = sort(validTypes())
  n_types = length(types)
  hues = seq(15, 375, length = n_types + 1)
  colors = hcl(h = hues, l = 65, c = 100)[1:n_types]
  ret = list()
  for(i in 1:n_types){
    ty = types[i]
    col = colors[i]
    ret[[ty]] = col
  }
  return(ret)
}


printWithCountGen <- function(msg) {
  count = reactiveVal(0)
  
  printWithCount <- function(){
    if(config.debug) {
      isolate(count(count() + 1))
      
      print(paste(msg, count()))
    }
  }
  
  return(printWithCount)
}


applyQuery = function(qry, data) {
  try(
    qry %>%
      {paste0('data %>% filter(', ., ')')} %>%
      {parse(text = .)} %>%
      eval, 
    silent = T)
}


doesQueryCompile = function(qry, data) {
  result = try(applyQuery(qry, data), silent = T)
  
  return(!(class(result) == "try-error"))
}


getElementSafe = function(item_name, obj, default = NULL) {
  if(item_name %in% names(obj)) obj[[item_name]]
  else default
}


plotOptsToSourceCode = function(plot_options) {
  no_selection = plot_options$no_selection
  f = stringr::str_interp
  src = f("plot_options = generatePlotDefaults('${no_selection}')")
  
  def_opts = generatePlotDefaults(no_selection)
  for(n in names(plot_options)) {
    val = plot_options[[n]]
    if(val == no_selection | val == def_opts[[n]])
      next
    
    if(is.character(val) & !is.array(val))
      val = f("\"${val}\"")
    
    src = paste(src, f("plot_options$${n} = ${val}"), sep = '\n')
  }
  
  return(src)
}


filterQryToSourceCode = function(filter_qry, data_name, filtered_data_name) {
  f = stringr::str_interp
  src = f("${filtered_data_name} = ${data_name} %>%\n\t\tfilter(${filter_qry})")
  return(src)
}

generateSelectedQuery = function(selected_values) {
    f = stringr::str_interp

    selected_ixs = selected_values %>% names %>%
      sapply(function(n) !('All' %in% selected_values[[n]]))

    if(sum(selected_ixs) == 0)
      return("")

    selected_values = selected_values[selected_ixs]
    
    selected_values %>% 
      names %>% 
      sapply(function(n) {
        values = selected_values[[n]]
        is_char = class(values) == 'character'
        
        values_str = values %>% sapply(function(v) 
          if(is_char)  f("'${v}'") else v) %>%
          paste(collapse = ',')
        
        paste(n, '%in%', paste0('c(', values_str, ')'))
        }) %>%
      paste(collapse = ' & ')
}


selectedValsToSourceCode = function(selected_vals, data_name, selected_data_name) {
  f = stringr::str_interp

  selected_qry = generateSelectedQuery(selected_vals)
  src = f("${selected_data_name} = ${data_name} %>% \n\t\tfilter(${selected_qry})")
  return(src)
}

generatePlotSourceCode = function(plot_options, filter_qry, selected_vals, f_name,
				  data_name = "events") {
	f = stringr::str_interp

	load_f_src = paste(
			   f("f_name = \"${f_name}\" #update to your local path"),
			   f("${data_name} = loadEventsWithRelativeAndDeltaTime(f_name)"),
			   sep = '\n')

	events_src = load_f_src

	appendToSrc = function(input_data, prefix, qry_input, src, fn) {
		ret = list(output_data = input_data, src = src)
		if(is.null(qry_input)) return(ret)

		ret$output_data = f("${prefix}_${data_name}")
		ret$src = paste(src,
				fn(qry_input, input_data, ret$output_data),
				sep = '\n')

		ret
	}

	none_selected = selected_vals %>%
		sapply(function(f) any(f %in% 'All')) %>%
		all
	if(none_selected) selected_vals = NULL
	selected = appendToSrc(data_name, "selected", selected_vals, events_src,
			       selectedValsToSourceCode)

	filtered = appendToSrc(selected$output_data, "filtered", filter_qry, selected$src,
			       filterQryToSourceCode)

	plot_opts_src = plotOptsToSourceCode(plot_options)

	src = paste(
		    "source(readtv)",
		    filtered$src,
		    plot_opts_src,
		    f("p = generateTimePlot(${filtered$output}, plot_options)"),
    "plot(p)",
    sep = '\n\n')
  
  return(src)
}

`%not in%` = function(item, collection) !(item %in% collection)

