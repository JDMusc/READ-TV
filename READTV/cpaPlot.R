addCpaMarkersToPlot <- function(time_plot, cpa_df) {
    p = time_plot
    cpa_arrows = cpa_df %>% 
      arrowDfFromCpaDf(yend = 1.5, n_heads = 1)
    cpa_labels = cpa_df %>% 
      textDfFromCpaDf(y_offset = 1.3)
    p = p + geom_segment(data = cpa_arrows, aes(x = x, xend = xend,
                                                y = y, yend = yend),
                         show.legend = F, 
                         arrow = arrow(length = unit(0.15, "cm"), 
                                       type = "closed")) +
      geom_label(data = cpa_labels, 
                 aes(x = x, y = y, label = sprintf("%.2f", label)))
  
  return(p)
}
