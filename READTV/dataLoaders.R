
loadFileExpr = function(f_name, ...) {
  n_args = nargs()
  
  f_name %>% 
    file_ext %>% 
    tolower %>% 
    switch(
      'rds' = expr(read_rds),
      'csv' = if(n_args > 1)
        expr(read_csv(!!!(rlang::list2(...))))
      else
        expr(read_csv)
    )
}


quickLoadEventsCode = function(f_name, n_max = 100, cols = list(),
                               ...) {
  args = rlang::list2(n_max = n_max) %>% append(rlang::list2(...))
  expr(f_name %>% 
         (!!(loadFileExpr(f_name, !!!args))) %>% 
         slice(1:!!n_max)) %>% 
    appendColsRhs(cols)
}

quickLoad = function(f_name, n_max = 100, ...)
  eval_tidy(quickLoadEventsCode(f_name, n_max = n_max, ...), 
            data = list(f_name = f_name))


loadEventsCodeRhs = function(f_name, cols = list(), ...) {
  rhs <- expr(f_name %>% !!(loadFileExpr(f_name)))
  
  appendColsRhs(rhs, cols)
}


appendColsRhs = function(code, cols) {
  for(i in seq_along(cols)) {
    nm = names(cols)[[i]]
    val = cols[[i]]
    code = expr(!!code %>% mutate(!!sym(nm) := !!val))
  }
  
  code
}


loadEventsWithRelativeAndDeltaTimeCode = function(data_f, output_sym, cols = list()) {
  base = loadEventsCodeRhs(data_f, cols)
  appendEventsWithRelativeAndDeltaTimeCode(base, output_sym)
}


appendEventsWithRelativeAndDeltaTimeCode = function(input_expr, output_sym, cols = list()) {
  base = appendColsRhs(input_expr, cols)
  expr(!!output_sym <- !!base %>% 
         group_by(Case) %>% 
         group_modify(~ .x %>% mutate(deltaTime = Time - lag(Time),
                                      RelativeTime = Time - min(Time, na.rm = TRUE))) %>% 
         ungroup %>% 
         filter(RelativeTime > 0))
}


deltaTimesCodeRhs = function()
  caseGroupedModifyCodeRhs(.x %>% mutate(deltaTime = Time - lag(Time)))


relativeTimesCodeRhs = function()
  caseGroupedModifyCodeRhs(.x %>% mutate(RelativeTime = Time - min(Time, na.rm = TRUE)))


caseGroupedModifyCodeRhs = function(modify_expr)
  expr(events %>% 
         group_by(Case) %>% 
         group_modify(~ !!modify_expr) %>% 
         ungroup
       )
