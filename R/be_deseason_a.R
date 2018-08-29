#'
#' @export be_deseason_a

be_deseason_a <- function(df_met, ...){

  # Deseason annual temperature data
  df_ta_am <- aggregate(df_met$Ta_200, by = list(df_met$plotID),
                        FUN = mean, na.rm = TRUE)
  colnames(df_ta_am) <- c("plotID", "Ta_200_avgam")
  df_met <- merge(df_met, df_ta_am, by = "plotID")
  df_met$Ta_200_am_ds <- df_met$Ta_200 - df_met$Ta_200_avgam

  # Deseason annual rainfall data
  df_p_am <- aggregate(df_met$P_RT_NRT, by = list(df_met$g_pa),
                       FUN = mean, na.rm=TRUE)
  colnames(df_p_am) <- c("g_pa", "P_RT_NRT_avgam")
  df_met <- merge(df_met, df_p_am, by = "g_pa")
  df_met$P_RT_NRT_as_ds <- df_met$P_RT_NRT - df_met$P_RT_NRT_avgam
  return(df_met)
}
