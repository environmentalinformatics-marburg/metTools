#' Read meteorological data
#'
#' @description
#' Read meteorological data from files provided in the format of our
#' climate database.
#'
#' @param filepath Full path and name of the csv file containing the data.
#'
#' @return
#' A \code{data.frame} object.
#'
#' @author
#' Thomas Nauss
#'
#' @references
#' NONE
#'
#' @examples
#' \notrun{
#' }
#'
#' @export readMetData
#' @name readMetData
#'
readMetData <- function(filepath, ...){
  df_met <- read.table(filepath, header = TRUE, sep = ",", dec = ".")

  # Define grouping by year
  df_met$g_a <- substr(as.character(df_met$datetime), 1, 4)
  df_met$g_a <- as.numeric(df_met$g_a)

  # Define grouping by exploratory and land cover type
  df_met$g_belc <- substr(as.character(df_met$plotID), 1, 3)

  # Define grouping by plot and year
  df_met$g_pa <- paste0(as.character(df_met$plotID), "_",
                        substr(as.character(df_met$datetime), 1, 4))
  return(df_met)
}
