
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
        expr(read_csv),
      'tsv' = if(n_args > 1)
        expr(read_tsv(!!!(rlang::list2(...))))
      else
        expr(read_tsv)
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


loadEventsCode = function(f_name, out_pronoun = sym('raw_data'), cols = list(), ...) {
  rhs = loadEventsCodeRhs(f_name, cols, ...)
  expr(!!out_pronoun <- !!rhs)
}


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


appendCols = function(code, cols, output_sym) {
  rhs = appendColsRhs(code, cols)
  expr(!!output_sym <- !!rhs)
}


appendEventsWithRelativeAndDeltaTimeCode = function(input_sym, output_sym, has_case = TRUE) {
  rhs = expr(!!input_sym %>% mutate(`Any Event` = 1))

  if(has_case) {
    rhs = expr(!!rhs %>%
                 group_by(Case) %>%
                 group_modify(~ .x %>%
                                arrange(Time) %>%
                                mutate(deltaTime = Time - lag(Time),
                                       RelativeTime = Time - min(Time, na.rm = TRUE))
                              ) %>%
                 ungroup
               )
  }
  else {
    rhs = expr(!!rhs %>%
                 arrange(Time) %>%
                 mutate(deltaTime = Time - lag(Time),
                        RelativeTime = Time - min(Time, na.rm = TRUE)))
  }

  expr(!!output_sym <- !!rhs)
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
