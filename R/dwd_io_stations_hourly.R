#' LOAD DWD STATION DATA
#' @export dwd_io_stations_hourly
dwd_io_stations_hourly <- function(filepath, ...){
  dwd_stat <- read.table(filepath, header = TRUE, sep = ";", dec = ".")
  dwd_stat$MESS_DATUM = strptime(dwd_stat$MESS_DATUM, format = "%Y%m%d%H", tz = "UTC")
  colnames(dwd_stat)[c(2,4,5)] = c("datetime", "Ta_200", "rH_200")
  return(dwd_stat)
}
