library(fpp3)


loadData = function()
  fpp3::aus_arrivals %>% 
    mutate(Case = Origin, Event.Type = Origin) %>%
    group_by(Case) %>% 
    mutate(Time = Quarter - min(Quarter)) %>%
    ungroup

writeData = function()
  loadData() %>% saveRDS('processed/aus_arrivals.RDS')