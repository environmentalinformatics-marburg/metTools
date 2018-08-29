#' DESEASON
#' @export be_deseason_m
be_deseason_m <- function(df_met, ...){

  # Deseason monthly temperature data
  df_ta_mm <- aggregate(df_met$Ta_200, by = list(df_met$g_pm),
                        FUN = mean, na.rm = TRUE)
  colnames(df_ta_mm) <- c("g_pm", "Ta_200_avgmm")
  df_met <- merge(df_met, df_ta_mm, by = "g_pm")
  df_met$Ta_200_mm_ds <- df_met$Ta_200 - df_met$Ta_200_avgmm

  #   df_ta_max_mm <- aggregate(df_met$Ta_200_max, by = list(df_met$g_pm),
  #                         FUN = mean, na.rm = TRUE)
  #   colnames(df_ta_max_mm) <- c("g_pm", "Ta_200_max_avgmm")
  #   df_met <- merge(df_met, df_ta_max_mm, by = "g_pm")
  #   df_met$Ta_200_max_mm_ds <- df_met$Ta_200_max - df_met$Ta_200_max_avgmm
  #
  #   df_ta_min_mm <- aggregate(df_met$Ta_200_min, by = list(df_met$g_pm),
  #                             FUN = mean, na.rm = TRUE)
  #   colnames(df_ta_min_mm) <- c("g_pm", "Ta_200_min_avgmm")
  #   df_met <- merge(df_met, df_ta_min_mm, by = "g_pm")
  #   df_met$Ta_200_min_mm_ds <- df_met$Ta_200_min - df_met$Ta_200_min_avgmm

  # Deseason monthly rainfall data
  df_p_mm <- aggregate(df_met$P_RT_NRT, by = list(df_met$g_pm), FUN = mean,
                       na.rm=TRUE)
  colnames(df_p_mm) <- c("g_pm", "P_RT_NRT_avgmm")
  df_met <- merge(df_met, df_p_mm, by = "g_pm")
  df_met$P_RT_NRT_ms_ds <- df_met$P_RT_NRT - df_met$P_RT_NRT_avgmm
  return(df_met)
}


