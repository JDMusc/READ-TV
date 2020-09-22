
calcCpa = function(data, values_col = 'CpaInput',
                   cpa_params = generateCpaDefaults())
  data %>%
  arrange(!!index(data)) %>%
  {.[[values_col]]} %>%
  {cpt.mean(., method = cpa_params$method,
           penalty = cpa_params$penalty,
           Q = cpa_params$Q,
           pen.value = cpa_params$pen.value)}


generateCpaDefaults = function()
  list(Q = 4, input_col = 'CpaInput', method = 'BinSeg', penalty = 'BIC',
       pen.value = .05, smooth_window_n = 1)

cpaToHorizontalSegment = function(cpa_df) cpa_df %>%
  group_modify(~ data.frame(xend = .x$cpts,
                            yend = .x$vals,
                            y = .x$vals,
                            x = lag(.x$cpts, default = 0)))

cpaToVerticalSegment = function(cpa_df) cpa_df %>%
  group_modify(~ data.frame(
    xintercept = (
      .x$cpts %>% lag %>% na.omit %>% as.numeric
    ))
  )


cpaPipelineCode = function(data_sym, time_column_sym, values_column_sym,
                           output_sym = sym("cpa_markers"), facet_column_sym = NULL,
                           ...) {
  rhs = data_sym
  is_facet = !is.null(facet_column_sym)
  if(is_facet)
    rhs = expr(!!rhs %>% group_by(!!facet_column_sym))

  cpa_params = list2(...)

  rhs = expr(!!rhs %>%
               filter(n() > 1) %>%
               group_modify(~ arrange(.x, !!time_column_sym)) %>%
               group_modify(~ cpt.mean(pull(.x, !!values_column_sym),
                                       !!!cpa_params) %>%
                              {data.frame(cpts = pull(.x, !!time_column_sym)[.@cpts], vals = .@param.est$mean)}
               )
             )

  return(expr(!!output_sym <- !!rhs))
}


preprocessForCpa = function(data, smooth_window_n,
                            index_col = 'RelativeTime',
                            values_col = NULL,
                            output_col = 'CpaInput',
                            facet_col = NULL,
                            stride = 1,
                            agg_fn_expr = sum(.values)) {
  if(is.null(values_col)) {
    data$IsEvent = 1
    values_col = 'IsEvent'
  }

  agg_fn = new_function(exprs(.values = ), enexpr(agg_fn_expr))
  mutate_fn = function(data) data %>%
    {slidingWindow(.[[index_col]], .[[values_col]],
                     n = smooth_window_n, agg_fn = agg_fn, stride = stride)} %>%
    select(!!sym(output_col) := Value, !!sym(index_col) := Time)

  output_select_fn = function(data) data %>%
    select(!!sym(index_col), !!sym(output_col)) %>%
    distinct

  as_tsibble_fn = function(data) {
    if(is.duration(stride))
      interval = stride %>%
        as.numeric('seconds') %>%
        {new_interval(second = .)}
    else interval = new_interval(unit = stride)

    data %>%
      build_tsibble(index = !!sym(index_col), interval = interval)
  }

  is_facet = !is.null(facet_col)
  if(is_facet) {
    #facet_col = dplyr::groups(data)[[1]] #only supports one facet column for now
    data %<>%
      group_by(!!sym(facet_col)) %>%
      group_modify(~ mutate_fn(.x)) %>%
      group_modify(~ output_select_fn(.x)) %>%
      group_modify(~ as_tsibble(.x, index = index_col))
  }
  else
    data %<>%
      mutate_fn %>%
      output_select_fn %>%
      as_tsibble_fn

  data
}


preprocessForCpaCode = function(data, smooth_window_n,
                                index_col = 'RelativeTime',
                                values_col = NULL,
                                output_col = 'CpaInput',
                                stride = 1,
                                agg_fn_expr = sum(.values)) {
  rhs = expr(data)

  if(is.null(values_col))
    rhs = expr(!!rhs %>% mutate(IsEvent = 1))

  agg_fn = new_function(exprs(.values = ), enexpr(agg_fn_expr))

  mutate_expr = expr(!!rhs %>%
    {slidingWindow(!!index_col, !!values_col,
                      n = !!smooth_window_n,
                      agg_fn = agg_fn,
                      stride = !!stride)} %>%
      select(!!sym(output_col) := Value, !!sym(index_col) := Time)
  )

  output_select_fn = function(data) data %>%
    select(!!sym(index_col), !!sym(output_col)) %>%
    distinct

  as_tsibble_fn = function(data) data %>%
    as_tsibble(index = !!sym(index_col))

  data %<>%
    #input_select_fn %>%
    mutate_fn %>%
    output_select_fn %>%
    as_tsibble_fn
}


quickPlotCpa = function(data, smooth_window_n,
                        index_col = 'RelativeTime',
                        cpa_params = generateCpaDefaults()) {
  smoothed_data = data %>%
    preprocessForCpa(smooth_window_n = smooth_window_n,
                     index_col = index_col)

  p = autoplot(smoothed_data, CpaInput)

  geom_input = smoothed_data %>%
    calcCpa(cpa_params = cpa_params) %>%
     {cpaPtsAndValues(smoothed_data, .)} %>%
    cpaToHorizontalSegment

  p + geom_segment(data = geom_input,
                   aes(x = x, xend = xend, y = y, yend = yend))
}


cpaPenalties = function() c("None", "SIC", "BIC", "MBIC", "AIC",
                            "Hannan-Quinn", "Asymptotic", "CROPS") #add support for Manual


cpaMethods = function() c("AMOC", "PELT", "SegNeigh", "BinSeg")


cpaPtsAndValues = function(data, cpt_out, time_col = 'RelativeTime') {
  cpts = data %>%
    arrange(!!sym(time_col)) %>%
    {.[[time_col]][cpt_out@cpts]}
  data.frame(cpts = cpts, vals = cpt_out@param.est$mean)
}

