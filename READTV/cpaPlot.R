addCpaMarkersToPlot <- function(time_plot, cpa_df, plot_data, y_column = NULL,
                                facet_column = NULL) {
    p = time_plot
    is_facet = !is.null(facet_column)
    getYEnd = function(sub_cpa_df) {
      plot_data_sub = plot_data
      if(is_facet) plot_data_sub = plot_data %>% 
          filter(!!sym(facet_column) == unique(sub_cpa_df[[facet_column]]))
      
      y_column %>% 
      getElementSafe(plot_data_sub, 1) %>% 
      max
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
                                   frequency_type = 'rate')
	cpa_input_data %>% 
  mutate(color = frequency_type) %>%
  {time_plot + geom_line(aes(x = !!sym(x), y = !!sym(y), color=color), data = .)}
