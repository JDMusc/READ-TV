addCpaMarkersToPlot <- function(time_plot, cpa_df, plot_data, time_column = 'Time', y_column = NULL,
                                facet_column = NULL) {
    p = time_plot
    is_facet = !is.null(facet_column)
    getYEnd = function(sub_cpa_df) {
      if(is_null(y_column)) 
        return(1)
      if(y_column %not in% names(plot_data))
        return(1)
      
      plot_data_sub = plot_data
      if(is_facet) plot_data_sub = plot_data %>% 
          filter(!!sym(facet_column) == unique(sub_cpa_df[[facet_column]]))
      
      na_safe = plot_data_sub[y_column] %>% drop_na
      if(nrow(na_safe) == 0) 1
      else max(na_safe)
    }
    
    getStartPoint = function(sub_cpa_df) {
      plot_data_sub = plot_data
      if(is_facet) plot_data_sub = plot_data %>% 
        filter(!!sym(facet_column) == unique(sub_cpa_df[[facet_column]]))
      
      plot_data_sub %>% pull(time_column) %>% min(na.rm = TRUE)
    }
    
    cpa_arrows = cpa_df %>% 
      group_modify(~ arrowDfFromCpaDf(.x, yend = getYEnd(.x) + .5, n_heads = 1))
    cpa_labels = cpa_df %>% 
      group_modify(~ textDfFromCpaDf(.x, y_offset = getYEnd(.x) + .2, 
                   time_start = getStartPoint(.x)))
    
    p = p + geom_segment(data = cpa_arrows, aes(x = x, xend = xend,
                                                y = y, yend = yend),
                         show.legend = F, 
                         arrow = arrow(length = unit(0.15, "cm"), 
                                       type = "closed")) +
      geom_label(data = cpa_labels, 
                 aes(x = x, y = y, label = sprintf("%.2f", label)))
  
  return(p)
}


arrowDfFromCpaDf = function(cpa_df, yend, n_heads = 3) {
  arrow_data = cpa_df
  
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


textDfFromCpaDf = function(cpa_df, y_offset = 1.1, time_start = 0) {
  calc_midpoint = function(cpts) {
    starts = lag(cpts, default = time_start)
    (cpts - starts)/2 + starts
  }
  mutate_fn = function(df) df %>% 
    mutate(midpoint = calc_midpoint(cpts))
  
  cpa_df %>% 
    group_modify(~ mutate_fn(.x)) %>% 
    rename(x = midpoint, label = vals) %>% 
    mutate(y = y_offset)
}


addEventFrequencyToPlotCode = function(p, cpa_input_data,
                                       x, y, frequency_col = 'rate',
                                       p_pronoun = sym('p'),
                                       output_pronoun = sym('p')) {
  f = stringr::str_interp
  original_lab = p$labels$y
  
  ylabel = f("${original_lab}; ${frequency_col} of ${original_lab}")
  x = x
  y = y
  rhs = expr(!!p_pronoun + 
               geom_line(aes(x = !!sym(x), y= !!sym(y)), 
                         data = !!ensym(cpa_input_data),
                         linetype = 'dotdash') +
               scale_y_continuous() +
               ylab(!!ylabel)
             )
  
  expr(!!output_pronoun <- !!rhs)
}
