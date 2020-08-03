

withinTime = function(time_data, time, n) time_data %>% 
  between(time - n, time + n)

withinTimeSeries = function(time_data, values_data, 
                            n = 4, agg_fn = sum, stride = 1) {
  if(class(time_data) == 'difftime')
    time_data = as.numeric(time_data)
  
  time_pts = seq(from = min(time_data), to = max(time_data),
                 by = stride)
  vals = sapply(
    time_pts, 
    function(t) agg_fn(values_data[abs(time_data - t) <= n]))
  
  data.frame(Time = time_pts, Value = vals)
}


interEventTime = function(data, event_type, prev_event_type){
  data %>% 
    filter(Event.Type == event_type, Prev.Event.Type == prev_event_type) %>%
    group_by(Case) %>%
    mutate(n_case = n()) %>%
    filter(n_case > 1) %>%
    ungroup() %>%
    arrange(Case, Time) %>%
    group_by(Case) %>%
    mutate(TimeDiff = Time - lag(Time)) %>%
    filter(!is.na(TimeDiff)) %>%
    summarise(mnTime = weighted.mean(mean(TimeDiff), n_case),
              sdTime = weighted.mean(sd(TimeDiff), n_case))
}


eventCount = function(data, time_n) {
  n_rows = nrow(data)
  data$WithinN = sapply(1:n_rows, 
                        function(i) {
                          nrow(withinTime(data, data$Time[i], time_n))
                        }
                        )
  return(data)
}


cusum = function(arr) arr %>% {. - mean(.)} %>% cumsum %>% {append(0, .)}
cusumDiff = function(arr) arr %>% cusum %>% {max(.) - min(.)}


bootstrapCusum = function(arr, n = 1000){
  #paper samples without replacement
  n_arr = length(arr)
  bootstraps = sapply(1:n,function(i) arr %>% 
                        sample(n_arr) %>%
                        cusumDiff
                      )
  X = sum(cusumDiff(arr) > bootstraps)
  return(100 * X/n)
}
