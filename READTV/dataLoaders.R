

validTypes = function()
  c('COO', 'COM', 'EXT', 'TRN', 'EQ', 'ENV', 'PF', 'SDM', 'IC')


loadEvents = function(f_name) {
  is_rds = f_name %>% 
    file_ext %>% 
    tolower %>% 
    {. == 'rds'}
  if(is_rds)
    read_rds(f_name)
  else
    read_csv(f_name, stringsAsFactors = F)
}


loadEventsWithRelativeAndDeltaTime = function(data_f, index = 'DateTime', key = NULL) 
  data_f %>% 
  #loadEventsAsTsibble(index = index, key = key) %>% 
  loadEvents %>%
  deltaTimes %>% 
  relativeTimes %>%
  filter(RelativeTime >= 0)


loadEventsAsTsibble = function(f_name, index = 'DateTime', key = NULL) {
  events = read.csv(f_name, stringsAsFactors = F)
  
  if(!is.numeric(events[[index]]))
  	events[[index]] = as.POSIXct(events[[index]])

  events = as_tsibble(events, index = index, key = key, regular=F)
  
  return(events) }


loadDemo = function(events) events %>% relativeTimes


relativeTimes = function(events) events %>% 
  caseGroupedModify(
    function(df) df %>% 
      mutate(RelativeTime = Time - min(Time[!is.na(Time)])))


deltaTimes = function(events) events %>% 
  caseGroupedModify(
    function(df) df %>% mutate(deltaTime = Time - lag(Time))
  )


caseGroupedModify = function(events, modify_fn) events %>%
  group_by(Case) %>% 
  group_modify(~ modify_fn(.x)) %>% 
  ungroup
