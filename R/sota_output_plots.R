#' Function for reproducing the CD plot figures of the PAKDD'25 paper
#'
#' @param OUTPUT_DATA_FOLDER filepath to the output data folder
#' @return nothing, only output the two CD plots as pdfs
#' @export
plot_cd_figures = function(OUTPUT_DATA_FOLDER) {
  logger::log_info(paste0("Saving CD plots"))
  my_CD_oracle = average_rank_oracle(OUTPUT_DATA_FOLDER)
  my_CD_single_best = average_rank_best_single(OUTPUT_DATA_FOLDER)

  cd_oracle_filepath = file.path(OUTPUT_DATA_FOLDER, "sota", "CD_oracle.pdf")
  pdf(cd_oracle_filepath, width = 5, height = 1.25)
  par(mar = c(0, 0, 0, 0))
  scmamp::plotCD(my_CD_oracle)
  dev.off()
  logger::log_info(paste0("CD plot for the oracle setting saved in ", cd_oracle_filepath))

  cd_single_best_filepath = file.path(OUTPUT_DATA_FOLDER, "sota", "CD_single_best.pdf")
  pdf(cd_single_best_filepath, width = 5, height = 1.25)
  par(mar = c(0, 0, 0, 0))
  scmamp::plotCD(my_CD_single_best)
  dev.off()
  logger::log_info(paste0("CD plot for the single best setting saved in ", cd_single_best_filepath))
}

#' Prepared CD data frame ready for plotting, for the oracle setting
#'
#' @param OUTPUT_DATA_FOLDER filepath to the output data folder
#' @return data frame ready for plotting for the oracle setting
average_rank_oracle = function(OUTPUT_DATA_FOLDER) {
  df_ours = get_oracle(metric = "F1", OUTPUT_DATA_FOLDER, full_results = TRUE)
  df_sota = get_oracle_sota(metric = "F1", OUTPUT_DATA_FOLDER, full_results = TRUE)
  my_CD = tidyr::spread(bind_rows(df_sota, df_ours), key = dataset_name, value = eval)
  future_colnames = my_CD$algorithm_name
  my_CD = t(my_CD[,-1])
  colnames(my_CD) = future_colnames
  my_CD = data.frame(my_CD, check.names = FALSE)
  return(my_CD)
}

#' Prepared CD data frame ready for plotting, for the single-best setting
#'
#' @param OUTPUT_DATA_FOLDER filepath to the output data folder
#' @return data frame ready for plotting for the single-best setting
average_rank_best_single = function(OUTPUT_DATA_FOLDER) {
  df_ours = get_single_best_table(OUTPUT_DATA_FOLDER, full_results = TRUE)
  df_sota = get_single_best_table_sota(OUTPUT_DATA_FOLDER, full_results = TRUE)
  my_CD = tidyr::spread(bind_rows(df_sota, df_ours), key = dataset_name, value = F1)
  future_colnames = my_CD$algorithm_name
  my_CD = t(my_CD[,-1])
  colnames(my_CD) = future_colnames
  my_CD = data.frame(my_CD, check.names = FALSE)
  return(my_CD)
}
