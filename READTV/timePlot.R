generateTimePlot <- function(data, plot_opts) {
  no_selection = plot_opts$no_selection
  shape_col = plot_opts$shapeColumn
  color_col = plot_opts$colorColumn
  y_col = plot_opts$yColumn
  x_col = plot_opts$xColumn
  facet_col = plot_opts$facetColumn
  facet_order = plot_opts$facetOrder
  facet_labels = plot_opts$facetLabels
  facet_customized = plot_opts$facetCustomized
  facet_paginated = plot_opts$facetPaginated
  facet_rows_per_pg = plot_opts$facetRowsPerPage
  facet_page = plot_opts$facetPage
  do_stem_plot = plot_opts$doStemPlot
  
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
  
  return(p)
}
