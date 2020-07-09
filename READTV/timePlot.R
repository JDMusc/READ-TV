generateTimePlot <- function(data, customizeDisplay, cpaParams = NULL) {
  no_selection = customizeDisplay$no_selection
  shape_col = customizeDisplay$shapeColumn
  color_col = customizeDisplay$colorColumn
  y_col = customizeDisplay$yColumn
  x_col = customizeDisplay$xColumn
  facet_col = customizeDisplay$facetColumn
  facet_order = customizeDisplay$facetOrder
  facet_labels = customizeDisplay$facetLabels
  facet_customized = customizeDisplay$facetCustomized
  facet_paginated = customizeDisplay$facetPaginated
  facet_rows_per_pg = customizeDisplay$facetRowsPerPage
  facet_page = customizeDisplay$facetPage
  do_stem_plot = customizeDisplay$doStemPlot
  
  do_cpa = !is.null(cpaParams)
  
  point_aes = aes_string(y = y_col)
  if(!(shape_col == no_selection))
    point_aes$shape = quo(!!sym(shape_col))
  if(!(color_col == no_selection))
    point_aes$colour = quo(!!sym(color_col))
  
  show_data = data %>%
    mutate(Event = TRUE) %>% {
      if(!(shape_col == no_selection))
        mutate(., !!shape_col := factor(!!sym(shape_col)))
      else .}
  
  if(facet_customized)
    show_data[[facet_col]] = factor(show_data[[facet_col]],
                                    levels = facet_order,
                                    labels = facet_labels)
  else
    if(facet_paginated)
      show_data[[facet_col]] = factor(show_data[[facet_col]])
  
  p = show_data %>%
    ggplot(aes_string(x = x_col)) + 
    geom_point(point_aes)
  
  y_class = class(show_data[[y_col]])
  if(y_class == "logical")
    p = p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  
  if(do_stem_plot){
    if(!(color_col == no_selection))
      p = p + geom_segment(aes_string(xend = x_col, 
                                      yend = 0, 
                                      y = y_col,
                                      colour = color_col))
    else
      p = p + geom_segment(aes_string(xend = x_col, 
                                      yend = 0,
                                      y = y_col))
  }
  
  if(!(facet_col == no_selection)) {
    fm = formula(paste(facet_col, "~ ."))
    if(facet_paginated) {
      p = p + facet_grid_paginate(fm,
                         ncol = 1,
                         nrow = facet_rows_per_pg,
                         page = facet_page)
    }
    else p = p + facet_grid(fm)
  }
  
  do_cpa = !is.null(cpaParams)
  if(do_cpa) {
    axes_params = list(xColumn = customizeDisplay$xColumn,
                       facetColumn = customizeDisplay$facetColumn)
    if(customizeDisplay$facetColumn == customizeDisplay$no_selection)
      axes_params$facetColumn = NULL
    
    cpa_df = data %>% 
      cpaPipeline(axes_params, cpaParams)
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
  }
  
  return(p)
}
