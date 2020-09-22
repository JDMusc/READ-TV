f = stringr::str_interp


generateSelectedQuery = function(in_pronoun_sym, out_pronoun_sym, selected_vals) {
  qry = expr()
  nms = names(selected_vals)
  filtered_fields = nms %>% purrr::map(function(n){
    vals = selected_vals[[n]]
    if(is_empty(vals))
      return(FALSE)

    all(vals != 'All')
  }) %>%
    unlist %>%
    {nms[.]}

  if(length(filtered_fields) == 0)
    rhs = in_pronoun_sym
  else
    rhs = filtered_fields %>%
      purrr::map(~ filterInExpr(.x, selected_vals[[.x]])) %>%
      purrr::reduce( ~ expr(!!.x %>% !!.y), .init = in_pronoun_sym)

  return(expr(!!out_pronoun_sym <- !!rhs))
}


filterInExpr = function(field, arr) {
  expr(filter(!!(sym(field)) %in% !!arr))
}
