generateTimePlot <- function(data, plot_opts) {
  #----Mandatory Fields----
  x_col = plot_opts$xColumn
  
  #----Optional Fields----
  no_selection = getElementSafe('no_selection', plot_opts, '_None_')
  
  getSafe = function(item_name, default = no_selection)
    getElementSafe(item_name, plot_opts, default)
  
  y_col = getSafe('yColumn')
  
  shape_col = getSafe('shapeColumn')
  color_col = getSafe('colorColumn')
  
  facet_col = getSafe('facetColumn')
  facet_order = getSafe('facetOrder')
  facet_labels = getSafe('facetLabels')
  
  is_facet_customized = getSafe('isFacetCustomized', F)
  is_facet_paginated = getSafe('isFacetPaginated', F)
  facet_rows_per_pg = getSafe('facetRowsPerPage')
  facet_page = getSafe('facetPage', 1)
  do_stem_plot = getSafe('doStemPlot', T)
  geom_function = getSafe('geomFunction', geom_point)

  
  #----Plot Data----
  show_data = data
  if(y_col == no_selection) {
    show_data = show_data %>% 
      mutate(Event = T)
    y_col = 'Event'
  }
  
  is_y_bool = show_data %>% 
    pull(y_col) %>% 
    is.logical
  
  if(is_y_bool)
    show_data[[y_col]] = as.numeric(show_data[[y_col]])
  
  if(shape_col != no_selection)
    show_data = show_data %>% 
      mutate(!!shape_col := factor(!!sym(shape_col)))
  
  
  #---Color & Shape----
  data_aes = aes_string(y = y_col)
  if(!(shape_col == no_selection))
    data_aes$shape = quo(!!sym(shape_col))
  if(!(color_col == no_selection))
    data_aes$colour = quo(!!sym(color_col))
  
  
  #----Facet----
  if(is_facet_customized)
    show_data[[facet_col]] = factor(show_data[[facet_col]],
                                    levels = facet_order,
                                    labels = facet_labels)
  else
    if(is_facet_paginated)
      show_data[[facet_col]] = factor(show_data[[facet_col]])
  
  
  #----Base Plot----
  p = show_data %>%
    ggplot(aes_string(x = x_col)) + 
    geom_function(data_aes)
  
  y_class = class(show_data[[y_col]])
  if(y_class == "logical")
    p = p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  
  
  #----Do Stem----
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
  
  #----Facet Pagination----
  if(!(facet_col == no_selection)) {
    fm = formula(paste(facet_col, "~ ."))
    if(is_facet_paginated) {
      p = p + facet_grid_paginate(fm,
                         ncol = 1,
                         nrow = facet_rows_per_pg,
                         page = facet_page)
    }
    else p = p + facet_grid(fm)
  }
  
  #----Scale for event pulse----
  if(is_y_bool)
    p = p + scale_y_continuous(breaks = 1)
  
  #----Return----
  return(p)
}
