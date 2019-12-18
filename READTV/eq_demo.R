library(lubridate)
orig_eq_data = read.csv('demo/query_3_11.csv', stringsAsFactors = F)

start_dt = '2011-03-10T00:00:00'
orders = 'ymdHMS'
orig_eq_data$TimeAbs = parse_date_time(orig_eq_data$time, orders)
orig_eq_data = orig_eq_data %>% arrange(TimeAbs)

orig_eq_data$Time = as.numeric(
  difftime(orig_eq_data$TimeAbs, parse_date_time(start_dt, orders))
)

magToClass = function(mag) {
  if(mag < 4) return("minor")
  else if(mag < 5) return("light")
  else if(mag < 6) return("moderate")
  else if(mag < 7) return("strong")
  else if(mag < 8) return("major")
  else return("great")
}

orig_eq_data$Case = 1

orig_eq_data$Event.Type = as.character(sapply(orig_eq_data$mag, magToClass))

write.csv(orig_eq_data, 'demo/query_3_11_events.csv', row.names = F)
