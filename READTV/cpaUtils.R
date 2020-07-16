
calcCpa = function(data, values_col = 'CpaInput',
                   cpa_params = generateCpaDefaults())
  data %>%
  arrange(!!index(data)) %>% 
  {.[[values_col]]} %>% 
  cpt.mean(method = cpa_params$method,
           penalty = cpa_params$penalty,
           Q = cpa_params$Q,
           pen.value = cpa_params$pen.value)


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


cpaPipeline = function(data, time_column, values_column, facet_column = NULL, 
                       cpa_params, agg_fn = sum, preprocess = T) {
  is_facet = !is.null(facet_column)
  if(is_facet) data %<>% 
    group_by(!!sym(facet_column))

  if(preprocess) {
    data %<>%
      group_modify(~ preprocessForCpa(.x, cpa_params$smooth_window_n,
                                      index_col = time_column, 
                                      agg_fn = agg_fn,
                                      values_col = values_column,
                                      facet_col = facet_column)
    )
    
    values_column = 'CpaInput'
  }
  
  data %>% 
    group_modify(~ cpaPtsAndValues(
      calcCpa(as_tsibble(.x, index = time_column), 
              cpa_params = cpa_params, values_col = values_column), 
      .x))
}


preprocessForCpa = function(data, smooth_window_n, 
                            index_col = 'RelativeTime', 
                            values_col = NULL,
                            output_col = 'CpaInput',
                            facet_col = NULL,
                            stride = 1,
                            agg_fn = sum) {
  if(is.null(values_col)) {
    data$IsEvent = 1
    values_col = 'IsEvent'
  }

 #input_select_fn = function(data) data %>% 
 #  select(!!sym(index_col), !!sym(values_col)) %>% 
 #  distinct
  
  mutate_fn = function(data) data %>% 
    {withinTimeSeries(.[[index_col]], .[[values_col]],
                     n = smooth_window_n, agg_fn = agg_fn, stride = stride)} %>% 
    select(!!sym(output_col) := Value, !!sym(index_col) := Time)
  
  output_select_fn = function(data) data %>% 
    select(!!sym(index_col), !!sym(output_col)) %>% 
    distinct
  
  as_tsibble_fn = function(data) data %>% 
    as_tsibble(index = !!sym(index_col))
  
  is_facet = !is.null(facet_col)
  if(is_facet) {
    #facet_col = groups(data)[[1]] #only supports one facet column for now
    data %<>% 
      group_by(!!sym(facet_col)) %>% 
      #group_modify ~ input_select_fn(.x) %>% 
      group_modify(~ mutate_fn(.x)) %>%
      group_modify(~ output_select_fn(.x)) %>% 
      as_tsibble(index = !!sym(index_col), key = !!facet_col)
  }
  else
    data %<>%
      #input_select_fn %>% 
      mutate_fn %>% 
      output_select_fn %>% 
      as_tsibble_fn
  
  data
}


quickPlotCpa = function(data, smooth_window_n, 
                        input_col = 'RelativeTime',
                        cpa_params = generateCpaDefaults()) {
  smoothed_data = data %>% 
    preprocessForCpa(smooth_window_n = smooth_window_n, 
               input_col = input_col)
  
  p = autoplot(smoothed_data, CpaInput)
  
  geom_input = smoothed_data %>% 
    calcCpa(cpa_params = cpa_params) %>% 
     cpaPtsAndValues(smoothed_data) %>% 
    cpaToHorizontalSegment
  
  p + geom_segment(data = geom_input, 
                   aes(x = x, xend = xend, y = y, yend = yend))
}


cpaPenalties = function() c("None", "SIC", "BIC", "MBIC", "AIC", 
                            "Hannan-Quinn", "Asymptotic", "CROPS") #add support for Manual


cpaMethods = function() c("AMOC", "PELT", "SegNeigh", "BinSeg")


cpaPtsAndValues = function(cpt_out, data, time_col = 'RelativeTime') {
  cpts = data %>% 
    arrange(!!sym(time_col)) %>% 
    {.[[time_col]][cpt_out@cpts]}
  data.frame(cpts = cpts, vals = cpt_out@param.est$mean)
}


arrowDfFromCpaDf = function(cpa_df, yend, n_heads = 3) {
  arrow_data = cpa_df
  #for(i in 1:n_heads) {
  #  arrow_data[paste0('_a', i)] = i/n_heads * yend
  #}
  
  start_col = ncol(cpa_df) + 1
  end_col = ncol(arrow_data)
  mutate_fn = function(df) df %>% 
    mutate(increases = (lead(vals, default = 0) - vals) > 0)
  filter_last_fn = function(df) df %>% slice(-n())
  arrow_data %>% 
    group_modify(~ mutate_fn(.x)) %>% 
    group_modify(~ filter_last_fn(.x)) %>% 
    #pivot_longer(cols = start_col:end_col, values_to = 'yend') %>% 
    rename(x = cpts) %>%
    mutate(xend = x, y = if_else(increases, 0, yend),
           yend = if_else(increases, yend, 0))
}


textDfFromCpaDf = function(cpa_df, y_offset = 1.1) {
  calc_midpoint = function(cpts) {
    starts = lag(cpts, default = 0)
    (cpts - starts)/2 + starts
  }
  mutate_fn = function(df) df %>% 
    mutate(midpoint = calc_midpoint(cpts))
  
  cpa_df %>% 
    group_modify(~ mutate_fn(.x)) %>% 
    rename(x = midpoint, label = vals) %>% 
    mutate(y = y_offset)
}
