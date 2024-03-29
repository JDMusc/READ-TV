cpaTabLogic.basePlotCode = function(cpa_plot_df = NULL, 
                                    cpa_plot_opts = NULL,
                                    cpa_plot_df_pronoun = sym("cpa_plot_df"),
                                    base_plot_df = NULL, 
                                    plot_opts = NULL, 
                                    base_plot_df_pronoun = sym("base_plot_df"),
                                    show_original = F,
                                    out_p_pronoun = sym("p")) {
  if(is.null(cpa_plot_df) & is.null(base_plot_df))
    stop("either cpa_plot_df or base_plot_df must be set")
  
  if(!is.null(base_plot_df) & is.null(plot_opts))
    stop("plot_opts must be set if base_plot_df is set")
  
  if(!is.null(cpa_plot_df) & is.null(cpa_plot_opts))
    stop("cpa_plot_opts must be set if cpa_plot_df is set")
  
  gtp = function(plot_df, opts, plot_df_pronoun)
    generateTimePlotCode(plot_df, opts, plot_data_pronoun = plot_df_pronoun,
                         out_p_pronoun = out_p_pronoun)
  
  if(show_original)
    gtp(base_plot_df, plot_opts, base_plot_df_pronoun)
  else
    gtp(cpa_plot_df, cpa_plot_opts, cpa_plot_df_pronoun)
}


cpaTabLogic.addEventFrequencyCode = function(plot_opts,
                                             p_pronoun,
                                             cpa_plot_opts,
                                             cpa_plot_df_pronoun,
                                             show_original_and_event_frequency,
                                             smooth_fn_name,
                                             out_p_pronoun = sym("p")) {
  
  rhs = p_pronoun
  original_y = if(is_str_set(plot_opts$y)) plot_opts$y else plot_opts$anyEvent
  if(show_original_and_event_frequency) {
    f = stringr::str_interp
    
    ylabel = f("${original_y}; ${smooth_fn_name} of ${original_y}")
    rhs = expr(!!p_pronoun + 
                 geom_line(aes(x = !!(sym(cpa_plot_opts$x)), 
                               y= !!(sym(cpa_plot_opts$y))), 
                           data = !!(cpa_plot_df_pronoun),
                           linetype = 'dotdash') +
                 scale_y_continuous() +
                 ylab(!!ylabel)
               )
  }
  
  expr(!!out_p_pronoun <- !!rhs)
}


cpaTabLogic.addCpaMarkersCode = function(
  p_pronoun, 
  cpa_plot_df, 
  cpa_plot_opts, 
  cpa_plot_df_pronoun, 
  base_plot_df, 
  plot_opts, 
  base_plot_df_pronoun, 
  cpa_markers_pronoun, 
  add_markers, show_original, show_original_and_event_frequency) {
  
  x_col = plot_opts$x
  if(add_markers) {
    use_cpa_y = !show_original
    if(show_original_and_event_frequency) {
      mx_cpa = cpa_plot_df %>% 
        pull(!!sym(cpa_plot_opts$y)) %>% 
        max(na.rm = T)
      
      mx_prev = base_plot_df %>% 
        getElementSafe(plot_opts$y, 1) %>% 
        max(na.rm = T)
      
      use_cpa_y = mx_cpa > mx_prev
    }
    
    if(use_cpa_y) {
      ref_data = cpa_plot_df_pronoun
      y_col = cpa_plot_opts$y
      x_col = cpa_plot_opts$x
    } else {
      ref_data = base_plot_df_pronoun
      y_col = plot_opts$y
    }
    
    rhs = expr(addCpaMarkersToPlot(!!p_pronoun, !!cpa_markers_pronoun,
                                   !!ref_data, !!x_col, !!y_col)
    )
  }
  else
    rhs = expr(!!p_pronoun)
  
  expr(p <- !!rhs)
}

