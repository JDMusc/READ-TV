f = stringr::str_interp


generateSelectedQuery = function(in_pronoun_sym, out_pronoun_sym, selected_vals,
                                 filter_out = TRUE) {
  rhs = generateSelectedQueryRhs(in_pronoun_sym, selected_vals, filter_out)
  expr(!!out_pronoun_sym <- !!rhs)
}


generateSelectedQueryRhs = function(in_pronoun_sym, selected_vals,
                                    filter_out = TRUE)
  selected_vals %>%
    purrr::discard(~ is_empty(.x) | ('All' %in% .x)) %>%
    purrr::imap(~ expr(!!(sym(.y)) %in% !!.x)) %>%
    {
      qry = if(!is_empty(.))
        purrr::reduce(., ~ expr(!!.x & !!.y))
      else
        NULL

      mappedExpr(in_pronoun_sym, qry, filter_out)
    }


mappedExpr = function(in_pronoun_sym, qry, filter_out, append = FALSE) {
  has_query = !is_null_or_empty(qry)
  if(filter_out) {
    if(has_query)
      expr(!!in_pronoun_sym %>% filter(!!qry))
    else
      in_pronoun_sym
  }
  else {
    if(!has_query & append)
      in_pronoun_sym
    else {
      qry = if(has_query)
        qry
      else
        TRUE

      rhs = if(!append)
        expr(mutate(`Read-TV Filtered` = !!qry))
      else
        expr(mutate(`Read-TV Filtered` = `Read-TV Filtered` & !!qry))

      expr(!!in_pronoun_sym %>% !!rhs)
    }
  }
}
