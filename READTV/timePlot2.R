generatePreparePlotCode <- function(data_quo, plot_opts,
                                    in_pronoun_sym, out_pronoun_sym) {
  #----Field Extractors---
  no_selection = getElementSafe('no_selection', plot_opts, '_None_')
  getSafe = function(item_name, default = no_selection)
    getElementSafe(item_name, plot_opts, default)
  
  
  #----Mandatory Fields----
  x_col = getSafe("xColumn")
  stopifnot(!(x_col == no_selection))
  
  
  #----Optional Fields----
  y_col = getSafe('yColumn')
  
  shape_col = getSafe('shapeColumn')
  
  facet_col = getSafe('facetColumn')
  facet_order = getSafe('facetOrder')
  facet_labels = getSafe('facetLabels')
  
  is_facet_customized = getSafe('isFacetCustomized', F)
  is_facet_paginated = getSafe('isFacetPaginated', F)
  
  
  #----Y Column----
  pre_plot_rhs = get_expr(data_quo)
  
  
  if(y_col == no_selection) {
    pre_plot_rhs = expr(!!pre_plot_rhs %>% 
                          mutate(Event = 1)
    )
    is_y_bool = FALSE
    y_col = 'Event'
  } else
    is_y_bool = eval_tidy(data_quo) %>% 
    pull(y_col) %>% 
    is.logical
  
  y_col_sym = sym(y_col)
  if(is_y_bool)
    pre_plot_rhs = expr(
      !!pre_plot_rhs %>% 
        mutate(!!y_col_sym := as.numeric(!!y_col_sym)))
  
  
  #----Shape Column----
  if(shape_col != no_selection) {
    data = eval_tidy(data_quo)
    if(!is.factor(data[[shape_col]]))
      pre_plot_rhs = expr(
        !!pre_plot_rhs %>% 
          mutate(!!sym(shape_col) := factor(!!sym(shape_col))))
  }
  
  
  #----Facet----
  base_facet = function(...) {
    ex_options = rlang::list2(...)
    expr(!!pre_plot_rhs %>% 
           mutate(
             !!sym(facet_col) := factor(
               !!sym(facet_col), !!!ex_options
             )
           )
    )
  }
  if(is_facet_customized)
    pre_plot_rhs = base_facet(
      levels = facet_order, labels = facet_labels)
  else
    if(is_facet_paginated)
      pre_plot_rhs = base_facet()
  
  
  return(expr(plot_data <- !!pre_plot_rhs))
}


generateTimePlotCode <- function(plot_data, plot_opts) {
  #----Field Extractors---
  no_selection = getElementSafe('no_selection', plot_opts, '_None_')
  getSafe = function(item_name, default = no_selection)
    getElementSafe(item_name, plot_opts, default)
  
  
  #----Mandatory Fields----
  x_col = getSafe("xColumn")
  stopifnot(!(x_col == no_selection))
  
  
  #----Optional Fields----
  y_col = getSafe('yColumn')
  if(y_col == no_selection) y_col = 'Event'
  
  shape_col = getSafe('shapeColumn')
  color_col = getSafe('colorColumn')
  
  facet_col = getSafe('facetColumn')
  
  is_facet_paginated = getSafe('isFacetPaginated', F)
  facet_rows_per_pg = getSafe('facetRowsPerPage')
  facet_page = getSafe('facetPage', 1)
  
  do_stem_plot = getSafe('doStemPlot', T)
  geom_function = getSafe('geomFunction', "geom_point")
  
  
  #---Color & Shape----
  aes_inputs = list(y = sym(y_col), x = sym(x_col))
  #data_aes = aes_string(y = y_col)
  if(!(shape_col == no_selection))
    aes_inputs$shape = sym(shape_col)
  if(!(color_col == no_selection))
    aes_inputs$colour = sym(color_col)
  
  
  #----Base Plot----
  p_rhs = expr(
    !!enexpr(plot_data) %>%
      ggplot(aes(!!!aes_inputs)) + 
      (!!geom_function)())
  
  if(is.logical(plot_data[[y_col]]))
    p_rhs = expr(!!p_rhs + 
                   theme(axis.text.y = element_blank(), 
                         axis.ticks.y = element_blank()))
  
  
  #----Do Stem----
  if(do_stem_plot){
    if(!(color_col == no_selection))
      p_rhs = expr(!!p_rhs + 
                     geom_segment(
                       aes(
                         xend = !!sym(x_col), 
                         yend = 0, y = !!sym(y_col),
                         colour = !!sym(color_col)))
      )
    else
      p_rhs = expr(!!p_rhs + 
                     geom_segment(
                       aes(xend = !!sym(x_col), 
                                  yend = 0,
                                  y = !!sym(y_col)))
      )
  }
  
  
  #----Facet Pagination----
  if(!(facet_col == no_selection)) {
    if(is_facet_paginated) {
      p_rhs = expr(!!p_rhs + 
                     facet_grid_paginate(
                       !!sym(facet_col) ~ .,
                       ncol = 1,
                       nrow = !!facet_rows_per_pg,
                       page = !!facet_page)
      )
    }
    else p_rhs = expr(!!p_rhs + 
                        facet_grid(!!sym(facet_col) ~ .))
  }
  
  
  #----Scale for event pulse----
  is_y_bool = plot_data[[y_col]] %>% is.logical
  is_all_one_value = plot_data[[y_col]] %>% unique %>% {length(.) == 1}
  
  if(is_y_bool || is_all_one_value)
    p_rhs = expr(!!p_rhs + 
                   scale_y_continuous(breaks = 1))
  
  
  #----Return----
  return(expr(p <- !!p_rhs))
}
