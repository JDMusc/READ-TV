

withinTime = function(time_data, time, n) time_data %>% 
  between(time - n, time + n)

withinTimeSeries = function(time_data, n = 4){
  #sapply(min(time_data):max(time_data), 
  sapply(time_data, 
         function(t) time_data %>% withinTime(t, n) %>% sum) %>%
  ts(start = min(time_data))
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
