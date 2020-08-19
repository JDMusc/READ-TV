addCpaMarkersToPlot <- function(time_plot, cpa_df, plot_data, y_column = NULL,
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
    
    cpa_arrows = cpa_df %>% 
      group_modify(~ arrowDfFromCpaDf(.x, yend = getYEnd(.x) + .5, n_heads = 1))
    cpa_labels = cpa_df %>% 
      group_modify(~ textDfFromCpaDf(.x, y_offset = getYEnd(.x) + .2))
    
    p = p + geom_segment(data = cpa_arrows, aes(x = x, xend = xend,
                                                y = y, yend = yend),
                         show.legend = F, 
                         arrow = arrow(length = unit(0.15, "cm"), 
                                       type = "closed")) +
      geom_label(data = cpa_labels, 
                 aes(x = x, y = y, label = sprintf("%.2f", label)))
  
  return(p)
}


addEventFrequencyToPlot = function(time_plot, cpa_input_data, x, y,
                                   frequency_col = 'rate') {
  f = stringr::str_interp
  original_lab = time_plot$labels$y
  
	cpa_input_data %>%
  {time_plot + 
      geom_line(aes(x = !!sym(x), y = !!sym(y)), data = .,
                linetype = "dotdash") + 
      scale_y_continuous() +
      ylab(f("${original_lab}; ${frequency_col} of ${original_lab}"))}
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
