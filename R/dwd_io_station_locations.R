#' LOAD DWD STATION LOCATIONS
#' @export dwd_io_station_locations
dwd_io_station_locations <- function(filepath, ...){
  dwd_stat <- read.table(filepath, header = TRUE, sep = ";", dec = ",")
  lat = which(colnames(dwd_stat) %in% c("geoBreite"))
  lon = which(colnames(dwd_stat) %in% c("geoLaenge"))
  
  dwd_stat_spdf = SpatialPointsDataFrame(coords = dwd_stat[, c(lon,lat)], 
                                         data = dwd_stat,
                                         proj4string = CRS('+init=EPSG:4326'))
  
  mapview(dwd_stat_spdf)
  return(dwd_stat_spdf)
}