

validTypes = function()
  c('COO', 'COM', 'EXT', 'TRN', 'EQ', 'ENV', 'PF', 'SDM', 'IC')


loadEvents = function() {
  tc_events = read.csv("data/tc_prepped_events.csv")
  
  #Subcode is always NA, therefore will remove
  tc_events$Subcode = NULL
  
  tc_events %>% resetDeltaTimes %>% return
}


loadEventsClean = function(f = "data/tc_prepped_events.csv") f %>% 
  read.csv %>% 
  filter(Event.Type %in% validTypes()) %>% 
  droplevels %>%
  resetDeltaTimes %>%
  resetPrevEvents %>%
  relativeTimes %>%
  select(Case, Phase, Code, Time, 
         deltaTime, RelativeTime, Event.Type, Prev.Event.Type, 
         Notes)


resetPrevEvents = function(events) {
  events$Prev.Event.Type.Orig = events$Prev.Event.Type
  
  events$Prev.Event.Type = events$Event.Type %>% 
    as.character %>% {.[1:nrow(events) - 1]} %>% {append('START', .)} %>% factor
  
  prev_cases = append(-1, events$Case[1:nrow(events) - 1])
  switches = events$Case != prev_cases
  
  events$Prev.Event.Type[switches] = 'START'
  
  return(events)
}


loadDemo = function(events) events %>% relativeTimes


relativeTimes = function(events) {
  case_starts = events %>% 
    {.$Case - lag(.$Case, default = -1)} %>%
    {which(. > 0)}
  n_cases = length(case_starts)
  
  if(n_cases > 1)
    case_ends = c(case_starts[2:n_cases] - 1, nrow(events))
  else
    case_ends = nrow(events)
  
  events$RelativeTime = events$Time
  
  for(i in 1:n_cases) {
    ixs = case_starts[i]:case_ends[i]
    case_times = events$Time[ixs]
    
    is_valid_time_ix = !is.na(case_times)
    valid_times = case_times[is_valid_time_ix]
    
    lowest_time = ifelse(length(valid_times) > 0, valid_times[1], NA)
    
    events$RelativeTime[ixs] = case_times - lowest_time
  }
  
  return(events)
}

deltaTimes = function(events) {
  n_events = length(events$Time)
  prev_time = append(0, events$Time[1:(n_events-1)])
  events$deltaTime = events$Time - prev_time
  
  cases = unique(events$Case)
  for(ca in cases){
    ix = match(ca, events$Case)
    events$deltaTime[ix] = NA
  }
  
  return(events)
}

resetDeltaTimes = function(events) {
  events$deltaTimeOrig = events$deltaTime
  
  n_events = length(events$Time)
  prev_time = append(0, events$Time[1:(n_events-1)])
  events$deltaTime = events$Time - prev_time
  
  cases = unique(events$Case)
  for(ca in cases){
    ix = match(ca, events$Case)
    events$deltaTime[ix] = events$deltaTimeOrig[ix]
  }
  
  return(events)
}
