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

  # Additional checks using autorank package in python, report below
  # R code:
  # write.csv(my_CD_oracle, file.path(OUTPUT_DATA_FOLDER, "sota", "data_for_CD_oracle.csv"))
  # write.csv(my_CD_single_best, file.path(OUTPUT_DATA_FOLDER, "sota", "data_for_CD_single_best.csv"))
  # 
  # Python code:
  # from autorank import autorank, plot_stats, create_report, latex_table
  # import pandas as pd
  # import matplotlib.pyplot as plt
  # filepaths = ["filepath/to/data_for_CD_oracle.csv", "filepath/to/data_for_CD_single_best.csv"]
  # for filepath in filepaths:
  #   data = pd.read_csv(filepath).set_index('Unnamed: 0').reset_index(drop=True)
  #   result = autorank(data, alpha=0.05, verbose=False)
  #   create_report(result)
  #   plot_stats(result)
  #   plt.show()
  # 
  # Report for oracle:
  # The statistical analysis was conducted for 4 populations with 42 paired samples.
  # The family-wise significance level of the tests is alpha=0.050.
  # We rejected the null hypothesis that the population is normal for the populations Solution Path (p=0.000), Penalty (p=0.000), Misc (p=0.000), and Ours (p=0.000). Therefore, we assume that not all populations are normal.
  # Because we have more than two populations and the populations and some of them are not normal, we use the non-parametric Friedman test as omnibus test to determine if there are any significant differences between the median values of the populations. We use the post-hoc Nemenyi test to infer which differences are significant. We report the median (MD), the median absolute deviation (MAD) and the mean rank (MR) among all populations over the samples. Differences between populations are significant, if the difference of the mean rank is greater than the critical distance CD=0.724 of the Nemenyi test.
  # We reject the null hypothesis (p=0.026) of the Friedman test that there is no difference in the central tendency of the populations Solution Path (MD=0.857+-0.233, MAD=0.143, MR=2.833), Penalty (MD=1.000+-0.180, MAD=0.000, MR=2.500), Misc (MD=1.000+-0.167, MAD=0.000, MR=2.393), and Ours (MD=1.000+-0.100, MAD=0.000, MR=2.274). Therefore, we assume that there is a statistically significant difference between the median values of the populations.
  # Based on the post-hoc Nemenyi test, we assume that there are no significant differences within the following groups: Solution Path, Penalty, Misc, and Ours. All other differences are significant.
  #
  # Report for single best:
  # The statistical analysis was conducted for 4 populations with 42 paired samples.
  # The family-wise significance level of the tests is alpha=0.050.
  # We rejected the null hypothesis that the population is normal for the populations Solution Path (p=0.000), Penalty (p=0.001), Misc (p=0.000), and Ours (p=0.001). Therefore, we assume that not all populations are normal.
  # Because we have more than two populations and the populations and some of them are not normal, we use the non-parametric Friedman test as omnibus test to determine if there are any significant differences between the median values of the populations. We use the post-hoc Nemenyi test to infer which differences are significant. We report the median (MD), the median absolute deviation (MAD) and the mean rank (MR) among all populations over the samples. Differences between populations are significant, if the difference of the mean rank is greater than the critical distance CD=0.724 of the Nemenyi test.
  # We reject the null hypothesis (p=0.000) of the Friedman test that there is no difference in the central tendency of the populations Solution Path (MD=0.450+-0.400, MAD=0.314, MR=2.964), Penalty (MD=0.243+-0.389, MAD=0.243, MR=2.833), Misc (MD=0.500+-0.441, MAD=0.357, MR=2.488), and Ours (MD=0.800+-0.180, MAD=0.200, MR=1.714). Therefore, we assume that there is a statistically significant difference between the median values of the populations.
  # Based on the post-hoc Nemenyi test, we assume that there are no significant differences within the following groups: Solution Path, Penalty, and Misc. All other differences are significant.
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
