library(dplyr)

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
  filter(FD.Type %in% validTypes()) %>% 
  droplevels %>%
  resetDeltaTimes %>%
  resetPrevFds %>%
  relativeTimes %>%
  select(Case, Phase, Code, Time, 
         deltaTime, RelativeTime, FD.Type, Prev.FD.Type, 
         Notes)


resetPrevFds = function(events) {
  events$Prev.FD.Type.Orig = events$Prev.FD.Type
  
  events$Prev.FD.Type = events$FD.Type %>% 
    as.character %>% {.[1:nrow(events) - 1]} %>% {append('START', .)} %>% factor
  
  prev_cases = append(-1, events$Case[1:nrow(events) - 1])
  switches = events$Case != prev_cases
  
  events$Prev.FD.Type[switches] = 'START'
  
  return(events)
}


loadDemo = function(events) events %>% relativeTimes


relativeTimes = function(events) {
  case_starts = events %>% 
    select(-Notes) %>% 
    {.$Case - lag(.$Case, default = -1)} %>%
    {which(. > 0)}
  n_cases = length(case_starts)
  case_ends = c(case_starts[2:n_cases] - 1, nrow(events))
  
  events$RelativeTime = events$deltaTime
  for(i in 1:n_cases) {
    ixs = case_starts[i]:case_ends[i]
    events$RelativeTime[ixs] = cumsum(events$deltaTime[ixs])
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
