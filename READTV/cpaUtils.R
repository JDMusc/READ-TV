
calcCpa = function(data, col = 'CpaInput',
                   cpa_params = generateCpaDefaults())
  cpt.mean(data[[col]], 
           method = cpa_params$method,
           penalty = cpa_params$penalty,
           Q = cpa_params$Q,
           pen.value = cpa_params$pen.value)


generateCpaDefaults = function()
  list(Q = 4, col = 'CpaInput', method = 'BinSeg', penalty = 'BIC',
       pen.value = .05)

cpaToHorizontalSegment = function(cpa_pts)
  data.frame(xend = cpa_pts$cpts,
             x = lag(cpa_pts$cpts, default = 0),
             yend = cpa_pts$vals,
             y = cpa_pts$vals)

cpaToVerticalSegment = function(cpa_pts)
  cpa_pts$cpts %>% 
  lag %>% 
  na.omit %>% 
  as.numeric %>% 
  {data.frame(xintercept = .)}


prepForCpa = function(data, sum_window_length, 
                      input_col = 'RelativeTime', 
                      output_col = 'CpaInput') data %>% 
  mutate(!!sym(output_col) := withinTimeSeries(
    !!sym(input_col), n = sum_window_length)
    ) %>% 
  as.tsibble(index = !!sym(input_col))


quickPlotCpa = function(data, sum_window_length, 
                        input_col = 'RelativeTime',
                        cpa_params = generateCpaDefaults()) {
  prepped_data = data %>% 
    prepForCpa(sum_window_length = sum_window_length, 
               input_col = input_col)
  
  p = autoplot(prepped_data, CpaInput)
  
  geom_input = prepped_data %>% 
    calcCpa(cpa_params = cpa_params) %>% 
     cpaPtsAndValues(prepped_data) %>% 
    cpaToHorizontalSegment
  
  p + geom_segment(data = geom_input, 
                   aes(x = x, xend = xend, y = y, yend = yend))
}


cpaPenalties = function() c("None", "SIC", "BIC", "MBIC", "AIC", 
                            "Hannan-Quinn", "Asymptotic", "Manual", "CROPS")


cpaMethods = function() c("AMOC", "PELT", "SegNeigh", "BinSeg")


cpaPtsAndValues = function(cpt_out, data) {
  index_col = tsibble::index(data)
  cpts = data[[index_col]][cpt_out@cpts]
  list(cpts = cpts, vals = cpt_out@param.est$mean)
}
