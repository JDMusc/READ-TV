

withinTime = function(time_data, time, n) time_data %>% 
  between(time - n, time + n)

withinTimeSeries = function(time_data, values_data, 
                            n = 4, agg_fn = sum, stride = 1) {
  un_sequenced_time = time_data
  
  is_diff_time_input = is.difftime(time_data)
  if(is_diff_time_input) {
    base = lubridate::parse_date_time('01/01/1970', 'mdy')
    units = attr(time_data, 'units')
    un_sequenced_time = as.duration(time_data) + base
  }
  
  time_pts_seq = seq(from = min(un_sequenced_time), to = max(un_sequenced_time),
                 by = stride)
  
  vals = sapply(
    time_pts_seq, 
    function(t) agg_fn(values_data[abs(un_sequenced_time - t) <= n]))
  
  if(is_diff_time_input)
    time_pts_seq = as.numeric(time_pts_seq - base, units)
  
  data.frame(Time = time_pts_seq, Value = vals)
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
    drop_na(TimeDiff) %>%
    summarise(mnTime = weighted.mean(mean(TimeDiff), n_case),
              sdTime = weighted.mean(sd(TimeDiff), n_case))
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
