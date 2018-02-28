#' Aggregate climate parameters in time and by station
#' @description Aggregate climate parameters in time and by station
#' @param dat data.frame including Dates, climate parameters for different plots
#' @param dts Character. Indicates which column of dat includes the Dates
#' @param plotID Character. Indicates which column of dat includes the Plot identifier
#' @param variable Character. Indicates which column of dat includes the variable to be aggregated
#' @param fun Character. How to aggregate the data. e.g. "mean" or "sum".
#' @param agg Character. defines the aggregation level. Either "week","month" or "year"
#' @return A data.frame with aggregated climate information
#' @author Hanna Meyer
#' @seealso \code{\link{GDD}}
#' @examples
#' #Calculate GDD from MODIS day/night data for several plots and aggregate it per month
#' dat <- get(load(system.file("extdata","ClimateData.RData",package="metTools")))
#' dat$GDD <- GDD(tmax=dat$tday,tmin=dat$tnight)
#' GDD_agg <- aggregateClimate(dat,"Date","Plot","GDD","sum")
#' @export aggregateClimate


aggregateClimate <- function(dat,dts,plotID,variable,fun,agg="month"){
  require(lubridate)

  ######### Aggregate on a yearly scale
  if(agg=="year"){
    df <- aggregate(dat[,variable],
                    by=list(dat[,plotID],year(dat[,dts])),
                    fun,na.rm=T)

    result <- lapply(unique(year(dat[,dts])),function(year){
      aggregate(df$x[df$Group.2==year],
                by=list(df$Group.1[df$Group.2==year],
                        df$Group.2[df$Group.2==year]),
                "mean",na.rm=T)$x})
    names(result) <- paste0(variable,"_",unique(year(dat[,dts])))
  }
  ######### Aggregate on a monhtly scale
  if(agg=="month"){
    df <- aggregate(dat[,variable],
                    by=list(dat[,plotID],year(dat[,dts]),month(dat[,dts])),
                    fun,na.rm=T)

    result <- lapply(1:12,function(month){
      aggregate(df$x[df$Group.3==month],
                by=list(df$Group.1[df$Group.3==month],
                        df$Group.2[df$Group.3==month]),
                "mean",na.rm=T)$x})
    names(result) <- paste0(variable,"_",month(1:12, label=TRUE,
                                               locale="en_US.UTF-8"))

  }
  ######### Aggregate on a weekly scale
  if(agg=="week"){
    df <- aggregate(dat[,variable],
                    by=list(dat[,plotID],year(dat[,dts]),week(dat[,dts])),
                    fun,na.rm=T)

    result <- lapply(1:53,function(week){
      aggregate(df$x[df$Group.3==week],
                by=list(df$Group.1[df$Group.3==week],
                        df$Group.2[df$Group.3==week]),
                "mean",na.rm=T)$x})
    names(result) <- paste0(variable,paste0("_week_",1:53))
  }
  ######### Reformat output
  if(agg=="year"){
    result <- data.frame("Plot"= df$Group.1, "Date"= df$Group.2,
                         unlist(result))
  }else{
    result <- data.frame(unique(df[,1:2]),result)
  }
  names(result)[1:2] <- c("Plot","Year")
  return(result)
}
