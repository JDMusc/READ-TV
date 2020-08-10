cpaTabLogic.timePlot = function(cpa_input_data = NULL, cpa_markers = NULL, 
                               cpa_plot_opts = NULL, do_plot_cpa = F, 
                               orig_data = NULL, plot_opts = NULL, 
                               show_original = F, 
                               show_original_and_event_frequency = F,
                               smooth_fn_name = NULL,
                               smoothed_plot_opts = NULL) {
  
  if(is.null(cpa_input_data) & is.null(orig_data))
    stop("either cpa_input_data or orig_data must be set")
  
  if(!is.null(orig_data) & is.null(plot_opts))
    stop("plot_opts must be set if orig_data is set")
  
  if(!is.null(cpa_input_data) & is.null(cpa_plot_opts))
    stop("cpa_plot_opts must be set if cpa_input_data is set")
  
  if(!show_original) {
    p = generateTimePlot(cpa_input_data, cpa_plot_opts)
  }
  else
    p = generateTimePlot(orig_data, plot_opts)
  
  if(show_original_and_event_frequency){
    p = addEventFrequencyToPlot(
      p, cpa_input_data, 
      cpa_plot_opts$xColumn, cpa_plot_opts$yColumn,
      smooth_fn_name
    )
  }
  
  if(do_plot_cpa) {
    use_cpa_y = !show_original
    if(show_original_and_event_frequency) {
      mx_cpa = cpa_input_data %>% 
        pull(!!sym(cpa_plot_opts$yColumn)) %>% 
        max(na.rm = T)
      
      mx_prev = orig_data %>% 
        getElementSafe(plot_opts$yColumn, 1) %>% 
        max(na.rm = T)
      
      use_cpa_y = mx_cpa > mx_prev
    }
    
    if(use_cpa_y) {
      plot_data = cpa_input_data
      y_col = cpa_plot_opts$yColumn
    } else {
      plot_data = orig_data
      y_col = plot_opts$yColumn
    }
    
    p = addCpaMarkersToPlot(p, cpa_markers,
                            plot_data, y_col)
  }
  
  p
}