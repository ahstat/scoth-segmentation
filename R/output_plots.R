#' Function for reproducing the figure of the PAKDD'25 paper regarding the qualitative results
#'
#' @param OUTPUT_DATA_FOLDER filepath to the output data folder
#' @param groundtruth data frame with ground truth indexes obtained with `load_groundtruths`
#' @param outfile filepath to the output plot
#' @param width plot size (width)
#' @param height plot size (height)
#' @return nothing, only output the figure to the `outfile` file path, as a pdf
#' @export
plot_figure_qualitative_results = function(OUTPUT_DATA_FOLDER, groundtruth,
                                           outfile = file.path(OUTPUT_DATA_FOLDER, "figure_qualitative_results.pdf"),
                                           width = 9, height = 6) {
  logger::log_info(paste0("Generating figure of the PAKDD 2025 paper"))
  nodes = c("scanline_42049", "unemployment_nl", "construction")
  levels = 0:3

  # Visualization parameters ----
  # (only for good alignment and positioning of the elements)
  params = list()

  # labels for the score plots (for alignment with the subsequent plots)
  params[["scores_y_labels"]] = list(scanline_42049 = c("0", "0.2", "0.4", "0.6"),
                                     unemployment_nl = c("", "", "20", ""),
                                     construction = c("", "", "75K", ""))
  # hiding y-axis scores labels for the score plots (except for column one)
  params[["scores_y_labels_colors"]] = list(scanline_42049 = "#000000",
                                            unemployment_nl = "#00000000",
                                            construction = "#00000000")
  # y-axis range of the change plots
  params[["changes_y_lims"]] = list(scanline_42049 = c(8,232),
                                    unemployment_nl = c(0,20),
                                    construction = c(20000,95000))
  # y-axis breaks of the change plots
  params[["changes_y_breaks"]] = list(scanline_42049 = c(0, 100, 200),
                                      unemployment_nl = c(0, 10, 20),
                                      construction = c(25000, 50000, 75000))
  # y-axis labels of the change plots
  params[["changes_y_labels"]] = list(scanline_42049 = c("0", "100", "200"),
                                      unemployment_nl = c("0", "10", ""),
                                      construction = c("25K", "50K", "75K"))
  # x-axis position of the annotation of K the number of detected changes for the change plots
  params[["changes_x_positions_for_K"]] = list(scanline_42049 = -80,
                                               unemployment_nl = -33,
                                               construction = -50)
  # function for the blue overlay boxes
  params[["changes_ggplot_funcs"]] = list(scanline_42049 = ggplot_steps,
                                          unemployment_nl = ggplot_steps,
                                          construction = ggplot_steps)
  # labels for the ground truth plots (for alignment with the previous plots)
  params[["gt_y_labels"]] = list(scanline_42049 = c(rep("", 4), "200"),
                                 unemployment_nl = c(rep("", 4), "20"),
                                 construction = c(rep("", 4), "50K"))
  # y-axis names for the ground truth plots (hidden, only for alignment with the previous plots)
  params[["gt_y_axis_names"]] = list(scanline_42049 = "level",
                                     unemployment_nl = NULL,
                                     construction = NULL)

  # Listing of the files ----
  folder_scoring = file.path(OUTPUT_DATA_FOLDER, "algorithm_scoring_results")
  folder_selection = file.path(OUTPUT_DATA_FOLDER, "algorithm_selection_results")
  files = list.files(folder_scoring)

  # Scoring plots (first row of the grid plot) ----
  p_list_scores = list()
  for(node in nodes) {
    score_y_label = params$scores_y_labels[[node]]
    score_y_label_color = params$scores_y_labels_colors[[node]]
    file = file.path(folder_scoring, files[grepl(node, files)])
    p_list_scores[[node]] = p_plot_scores(file, score_y_label, score_y_label_color) +
      ggtitle(label = NULL, subtitle = node)
  }

  # Change point plots (second to fifth rows of the grid plot) ----
  length_x_list = list()
  p_list_list = list()
  for(node in nodes) {
    p_list = list()
    for(level in levels) {
      change_ggplot_func = params$changes_ggplot_funcs[[node]]
      change_y_lim = params$changes_y_lims[[node]]
      change_x_position_for_K = params$changes_x_positions_for_K[[node]]
      change_y_break = params$changes_y_breaks[[node]]
      change_y_label = params$changes_y_labels[[node]]
      file = file.path(folder_selection, files[grepl(node, files)])
      current_results = extract_results(level, file)
      x = current_results$x
      partitions = current_results$partitions
      p = p_plot_changes(x, partitions, level, change_ggplot_func, change_y_lim, change_x_position_for_K, change_y_break, change_y_label)
      p_list[[length(p_list) + 1]] = p
    }
    length_x_list[[node]] = length(x)
    p_list_list[[node]] = p_list
  }

  # Ground truth plots (last row of the grid plot) ----
  p_gt = list()

  for(node in nodes) {
    length_x = length_x_list[[node]]
    gt_y_axis_name = params$gt_y_axis_names[[node]]
    gt_y_label = params$gt_y_labels[[node]]
    p_gt[[node]] = p_plot_groundtruth(node, groundtruth, length_x, gt_y_axis_name, gt_y_label)
  }

  # Assemble the plots ----
  gs <- lapply(1:18, function(ii)
    grobTree(rectGrob(gp=gpar(fill=ii, alpha=0.5)), textGrob(ii)))
  lay <- rbind(
    c(1,2,3),
    c(4,8,12),
    c(5,9,13),
    c(6,10,14),
    c(7,11,15),
    c(16,17,18))

  gs = list(
    grid_update_margins(p_list_scores[["scanline_42049"]], remove_y_labs = FALSE),
    grid_update_margins(p_list_scores[["unemployment_nl"]]),
    grid_update_margins(p_list_scores[["construction"]]),
    grid_update_margins(p_list_list[["scanline_42049"]][[1]], remove_y_labs = FALSE),
    grid_update_margins(p_list_list[["scanline_42049"]][[2]], remove_y_labs = FALSE),
    grid_update_margins(p_list_list[["scanline_42049"]][[3]], remove_y_labs = FALSE),
    grid_update_margins(p_list_list[["scanline_42049"]][[4]], remove_y_labs = FALSE),
    grid_update_margins(p_list_list[["unemployment_nl"]][[1]]),
    grid_update_margins(p_list_list[["unemployment_nl"]][[2]]),
    grid_update_margins(p_list_list[["unemployment_nl"]][[3]]),
    grid_update_margins(p_list_list[["unemployment_nl"]][[4]]),
    grid_update_margins(p_list_list[["construction"]][[1]]),
    grid_update_margins(p_list_list[["construction"]][[2]]),
    grid_update_margins(p_list_list[["construction"]][[3]]),
    grid_update_margins(p_list_list[["construction"]][[4]]),
    p_gt[[1]],
    p_gt[[2]],
    p_gt[[3]]
  )
  figureout = grid.arrange(grobs = gs, layout_matrix = lay, heights = c(1.2,1,1,1,1,1))
  dir.create(dirname(outfile), showWarnings = FALSE)
  ggsave(outfile, figureout, width = width, height = height)
  logger::log_info(paste0("Figure saved in ", outfile))
}

# All the following functions are helpers for reproducing the figure

p_plot_scores = function(file, score_y_label, score_y_label_color) {
  my_output = readRDS(file)
  my_s = ((my_output %>% filter(cost_method == "reg_cusum_L2") %>% pull(scoring))[[1]])$score
  my_s = data.frame(x = 1:length(my_s), y = my_s)
  p_scores = ggplot() +
    geom_line(data=my_s, aes(x=x,y=y), linewidth = 0.5) +
    theme_bw() +
    ylab("score") +
    xlab(NULL) +
    geom_hline(yintercept = 0.1, color = "red", linetype = "dashed") +
    scale_y_continuous(labels = score_y_label,
                       breaks = c(0, 0.2, 0.4, 0.6),
                       minor_breaks = c(0.1, 0.3, 0.5),
                       limits = c(0,0.645)) +
    theme(axis.text.y = element_text(color=score_y_label_color))
  return(p_scores)
}

extract_results = function(level, file, threshold = 0.1, cost_method = "reg_cusum_L2") {
  if(level == 0) {
    level = 1
    threshold = 1
  }

  results_current = readRDS(file) %>%
    filter(cost_method == !!cost_method) %>%
    filter(selection_level == !!level, selection_threshold_included >= !!threshold) %>%
    arrange(-selection_threshold_included) %>%
    filter(row_number()==n())

  # extract the current time series
  x = results_current$x[[1]]

  # extract the change positions and their corresponding levels
  partitions = results_current$selection_additional[[1]]$partitions

  return(list(x=x, partitions=partitions))
}

p_plot_changes = function(x, partitions, level, change_ggplot_func, change_y_lim, change_x_position_for_K, change_y_break, change_y_label) {
  # main plot
  p = change_ggplot_func(x, partitions, cost_func)

  # adding the K annotation
  K_value = nrow(partitions[[length(partitions)]])-1
  p = p +
    coord_cartesian(ylim = change_y_lim, clip = 'off')+
    annotation_custom(
      grob = textGrob(label = paste0("K=",K_value), hjust = 0, gp = gpar(cex = 0.7, col="red", fontface="italic")),
      ymin = change_y_lim[2],
      ymax = change_y_lim[2],
      xmin = change_x_position_for_K-10*(K_value>10),
      xmax = change_x_position_for_K-10*(K_value>10))

  # additional minor changes
  p = p +
    scale_y_continuous(breaks = change_y_break, labels = change_y_label, minor_breaks = NULL) +
    ylab(paste0("level ", level))

  return(p)
}

get_current_groundtruth = function(node, groundtruth) {
  # extract the ground truth information
  gt = sapply(groundtruth, function(x){x[[node]]})[-1]
  # remove users that did not label that series
  gt = gt[which(sapply(gt, class) != "NULL")]
  # order by number of annotations
  sorted_gt = sort(sapply(gt, length))
  gt_new = list()
  for(k in 1:length(gt)) {
    gt_new[[names(sorted_gt)[k]]] = gt[[names(sorted_gt)[k]]]
  }
  gt = gt_new
  return(gt)
}

get_gt_names = function(gt, char_shift = "                  ") {
  gt_numbers = as.numeric(sapply(gt, length))
  names_gt = names(gt)
  names_gt = gsub("user_", "u", names_gt)
  names_gt = paste0(names_gt, " K=", gt_numbers)
  names_gt = paste(names_gt, char_shift)
  return(names_gt)
}

p_plot_groundtruth = function(node, groundtruth, length_x, gt_y_axis_name, gt_y_label) {
  gt = get_current_groundtruth(node, groundtruth)
  gt_names = get_gt_names(gt)
  p_gt = ggplot() +
    geom_point(data = data.frame(x = gt[[1]], y = -rep(1, length(gt[[1]]))), aes(x=x, y=y), col = "#00BA38", pch = 2) +
    geom_point(data = data.frame(x = gt[[2]], y = -rep(2, length(gt[[2]]))), aes(x=x, y=y), col = "#00BA38", pch = 2) +
    geom_point(data = data.frame(x = gt[[3]], y = -rep(3, length(gt[[3]]))), aes(x=x, y=y), col = "#00BA38", pch = 2) +
    geom_point(data = data.frame(x = gt[[4]], y = -rep(4, length(gt[[4]]))), aes(x=x, y=y), col = "#00BA38", pch = 2) +
    geom_point(data = data.frame(x = gt[[5]], y = -rep(5, length(gt[[5]]))), aes(x=x, y=y), col = "#00BA38", pch = 2) +
    theme_minimal() +
    scale_y_continuous(limits = -c(5,1), minor_breaks = NULL, name = gt_y_axis_name, breaks = -1:-5, labels = gt_y_label) +
    theme(axis.text.y=element_text(colour="#00000000"), axis.title.y = element_text(colour="#00000000")) +
    xlim(c(1, length_x)) +
    xlab("time index") +
    annotate("text", x = 1, y = -c(1:5), label = gt_names, size = 8/.pt, color = "grey30") +
    coord_cartesian(clip = "off")
  return(p_gt)
}

grid_update_margins = function(p, remove_x_labs = TRUE, remove_y_labs = TRUE, mar = c(5.5, 5.5, 0, 5.5)) {
  p = p + theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.title.x = element_blank(),
                plot.margin = unit(mar, "pt"))
  if(remove_x_labs) {
    p = p + labs(x=NULL)
  }
  if(remove_y_labs) {
    p = p + labs(y=NULL)
  }
  return(p)
}

plot_from_x = function(x = c(0, 1, 0, 6, 4), p = NULL, style = "step", t = NULL, linewidth = 0.5) {
  # style among c("cadlag", "segment", "step")

  if(is.null(t)) {
    t = 1:length(x)
  }
  # data part
  df = data.frame(x = t, y = x)
  xlast = tail(df$x,1)
  ylast = tail(df$y,1)

  if((style == "cadlag")|(style == "segment")) {
    eps = 1e-9
    df_eps = df
    df_eps$x = df_eps$x-eps
    df_eps$y = NA
    df = rbind(df, df_eps) %>% arrange(x)
    df = df[-1,]
  }

  # plot part
  if(is.null(p)) {
    p = ggplot()
  }

  p = p +
    geom_step(data=df, aes(x=x,y=y), linewidth = linewidth) +
    geom_segment(aes(x = xlast, xend = xlast+diff(tail(df$x,2)),
                     y = ylast, yend = ylast), linewidth = linewidth) +
    xlab("t") +
    ylab("x(t)") +
    theme_bw()

  if(style == "cadlag") {
    p = p + geom_point(data=df, aes(x=x,y=y), na.rm = TRUE)
  }

  return(p)
}

ggplot_steps = function(x, partitions, cost_func, style = "step", p = NULL) {
  K = length(partitions)

  iterated_lines_color = paste0("#FF0000", c("DD", "BB", "88", rep("88", 10))) # lighter and lighter
  iterated_lines_size = c(1, 0.5, 0.3, rep(0.3, 10)) # thiner and thiner

  # Building data frames before plotting them ----
  for(k in 1:K) {
    partition = partitions[[k]]
  }

  if(is.null(p)) {
    p = ggplot()
  }

  ## Plotting the lines
  p = plot_from_x(x, p, style)

  ## Plotting vlines
  my_xintercept_previous = c()
  for(k in 1:K) {
    partition = partitions[[k]]
    my_xintercept = partition$s[-1]
    remove_previous_idx = which(my_xintercept %in% my_xintercept_previous)
    if(length(remove_previous_idx) > 0) {
      my_xintercept = my_xintercept[-remove_previous_idx]
    }
    my_xintercept_previous = c(my_xintercept_previous, my_xintercept)

    p = p + geom_vline(data=data.frame(xintercept=my_xintercept), aes(xintercept = xintercept),
                       col = iterated_lines_color[k], linetype = 2,
                       size = iterated_lines_size[k])
  }

  p
}
