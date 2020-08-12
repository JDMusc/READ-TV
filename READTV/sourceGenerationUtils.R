f = stringr::str_interp


generateImports = function()
  "library(changepoint)
library(DT)
library(dplyr)
library(feasts)
library(ggforce)
library(ggplot2)
library(magrittr)
library(readr)
library(shiny)
library(shinyjs)
library(shinythemes)
library(sortable)
library(stringr)
library(tidyr)
library(tools)
library(tsibble)"

appendToSrc = function(input_data, prefix, data_name, qry_input, src, fn) {
  ret = list(output_data = input_data, src = src)
  if(is.null(qry_input)) return(ret)
  
  ret$output_data = f("${prefix}_${data_name}")
  ret$src = paste(src,
                  fn(qry_input, input_data, ret$output_data),
                  sep = '\n')
  
  ret
}

generateLoadFile = function(f_name, data_name = "events")
  load_f_src = paste(
    f("f_name = \"${f_name}\" #update to your local path"),
    f("${data_name} = loadEventsWithRelativeAndDeltaTime(f_name)"),
    sep = '\n')



selectedValsToSourceCode = function(selected_vals, data_name, selected_data_name) {
  f = stringr::str_interp
  
  selected_qry = generateSelectedQuery(selected_vals)
  src = f("${selected_data_name} = ${data_name} %>% \n\t\tfilter(${selected_qry})")
  return(src)
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
