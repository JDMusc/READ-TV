

validTypes = function()
  c('COO', 'COM', 'EXT', 'TRN', 'EQ', 'ENV', 'PF', 'SDM', 'IC')


loadFileExpr = function(f_name) f_name %>% 
  file_ext %>% 
  tolower %>% 
  switch(
    'rds' = expr(read_rds),
    'csv' = expr(read_csv)
    )


loadEvents = function(f_name) f_name %>% 
  loadEventsCode %>% 
  eval_tidy(data = list(f_name = f_name))


quickLoadEventsCode = function(f_name, ...)
  expr(f_name %>% (!!(loadFileExpr(f_name)))(!!!(rlang::list2(...))))

quickLoad = function(f_name, n_max = 100, ...)
  eval_tidy(quickLoadEventsCode(f_name, n_max = n_max, ...), 
            data = list(f_name = f_name))


loadEventsCodeRhs = function(f_name, time_col = NULL, case_col = NULL) {
  rhs <- expr(f_name %>% !!(loadFileExpr(f_name)))
  
  rhs
}


loadEventsWithRelativeAndDeltaTime = function(data_f) 
  data_f %>% 
  #loadEventsAsTsibble(index = index, key = key) %>% 
  loadEvents %>%
  deltaTimes %>% 
  relativeTimes %>%
  filter(RelativeTime >= 0)


loadEventsWithRelativeAndDeltaTimeCode = function(data_f, output_sym) {
  base = loadEventsCodeRhs(data_f)
  expr(!!output_sym <- !!base %>% 
         group_by(Case) %>% 
         group_modify(~ .x %>% mutate(deltaTime = Time - lag(Time),
                                      RelativeTime = Time - min(Time[!is.na(Time)]))) %>% 
         ungroup %>% 
         filter(RelativeTime > 0))
}


loadEventsAsTsibble = function(f_name, index = 'DateTime', key = NULL) {
  events = read.csv(f_name, stringsAsFactors = F)
  
  if(!is.numeric(events[[index]]))
  	events[[index]] = as.POSIXct(events[[index]])

  events = as_tsibble(events, index = index, key = key, regular=F)
  
  return(events) }


loadDemo = function(events) events %>% relativeTimes


relativeTimes = function(events) events %>% 
  caseGroupedModify(
    function(df) df %>% 
      mutate(RelativeTime = Time - min(Time[!is.na(Time)])))


deltaTimes = function(events) events %>% 
  caseGroupedModify(
    function(df) df %>% mutate(deltaTime = Time - lag(Time))
  )


deltaTimesCodeRhs = function()
  caseGroupedModifyCodeRhs(.x %>% mutate(deltaTime = Time - lag(Time)))


relativeTimesCodeRhs = function()
  caseGroupedModifyCodeRhs(.x %>% mutate(RelativeTime = Time - min(Time[!is.na(Time)])))


caseGroupedModify = function(events, modify_fn) events %>%
  group_by(Case) %>% 
  group_modify(~ modify_fn(.x)) %>% 
  ungroup


caseGroupedModifyCodeRhs = function(modify_expr)
  expr(events %>% 
         group_by(Case) %>% 
         group_modify(~ !!modify_expr) %>% 
         ungroup
       )