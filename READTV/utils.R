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


applyQuery2 = function(qry_quo, env)
  eval_tidy(qry_quo, env = env)


doesQueryStringCompile = function(qry_string, data) {
  result = try(
    {
      qry_expr = parse_expr(paste0('filter(',qry_string,' )'))
      qry_expr = expr(data %>% !!qry_expr)
      applyQuery2(qry_expr, env(data = data))
    }, 
    silent = T)
  
  return(!(class(result) == "try-error"))
}


doesQueryCompile2 = function(qry_quo, env) {
  result = try(applyQuery2(qry_quo, env), silent = T)
  
  return(!(class(result) == "try-error"))
}


getElementSafe = function(item_name, obj, default = NULL) {
  if(item_name %in% names(obj)) obj[[item_name]]
  else default
}

`%not in%` = function(item, collection) {!(item %in% collection)}

expressionsToString = function(..., width = 50) 
  list(...) %>% 
  {flatten(.)} %>%
  purrr::discard(~ is_empty(.x)) %>%
  purrr::map(~ ifelse(is_string(.x), 
                      .x, 
                      expr_text(.x, width = width)
                      )
             ) %>% 
  purrr::reduce(~ paste(.x, .y, sep = '\n'))


runExpressions = function(exs, mask) {
  for(i in seq_along(exs)) {
    nm = names(exs)[[i]]
    mask[[nm]] = eval_tidy(exs[[i]], data = mask)
  }
  mask
}