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
