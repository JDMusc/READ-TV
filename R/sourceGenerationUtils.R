f = stringr::str_interp


generateSelectedQuery = function(in_pronoun_sym, out_pronoun_sym, selected_vals) {
  rhs = generateSelectedQueryRhs(in_pronoun_sym, selected_vals)
  expr(!!out_pronoun_sym <- !!rhs)
}


generateSelectedQueryRhs = function(in_pronoun_sym, selected_vals) {
  selected_cols = selected_vals %>%
    purrr::discard(~ is_empty(.x) | ('All' %in% .x))

  selected_exs = selected_cols %>%
    purrr::imap(~ expr(!!(sym(.y)) %in% !!.x))

  if(is_empty(selected_exs)) return(in_pronoun_sym)

  qry = purrr::reduce(selected_exs, ~ expr(!!.x & !!.y))
  expr(!!in_pronoun_sym %>% filter(!!qry))
}


mappedExpr = function(in_pronoun_sym, qry, filter_out = FALSE) {
  has_query = !is_null_or_empty(qry)
  if(filter_out) {
    if(has_query)
      expr(!!in_pronoun_sym %>% filter(!!qry))
    else
      in_pronoun_sym
  }
  else {
    qry = if(has_query)
      qry
    else
      TRUE

    expr(!!in_pronoun_sym %>%
           mutate(`read-tv Filtered` = if_else(!!qry, 1.0, .2)))
  }
}
