#' Main function for loading data set KPIs. Load as a data frame with one row per series
#'
#' @param PUBLIC_DATA_FOLDER filepath to the public-data folder
#' @return a data frame with two columns:
#' - the `dataset_name`,
#' - a `kpis` data frame with as number of columns the number of features
#' @export
load_kpis = function(PUBLIC_DATA_FOLDER = "~/anomaly-detection/scoth/public-data") {
  readRDS(file.path(PUBLIC_DATA_FOLDER, "kpis.RDS"))
}

#' Main function for loading change point ground truths
#' Load the ground truth with one row per series
#'
#' @param PUBLIC_DATA_FOLDER filepath to the public-data folder
#' @return Data frame with each row being a node (shanghai_license, well_log...) and
#' each column being a different user labeling each series. The users are user_X with X
#' a certain number. The first column `groundtruth` is, for each series, one of the user
#' that has the best Jaccard with the other users (~ the "median" user of each series).
#' Each cell is the index of change point selected by this user on this series
#' - If the cell is NULL, the user did not label this series,
#' - If the cell is integer(0), the user decided that no change point is present
#' - If the cell is a vector of index, those are the corresponding change point indexes
#' as hinted by the user.
#' The smallest possible index is 2 (meaning that a change point has happened at 2)
#' @export
load_groundtruths = function(PUBLIC_DATA_FOLDER = "~/anomaly-detection/scoth/public-data") {
  # Load and convert the original json file to a data frame
  l_annotations = jsonlite::read_json(file.path(PUBLIC_DATA_FOLDER, "annotations.json"))
  user_names = unique(unlist(lapply(l_annotations, names)))
  l = list()
  for(user in user_names) {
    lab_user = lapply(l_annotations, function(x){x[[user]]})
    # allow to disentangle NULL output (user not looking at that series) from
    # list() output (user looking but telling that there is no change point)
    lab_user[sapply(lab_user, function(x){identical(x, list())})] = list(integer(0))
    lab_user = sapply(lab_user, unlist)
    l[[paste0("user_", user)]] = lab_user
  }
  df = data.frame(bind_cols(as_tibble(l)))
  rownames(df) = names(l_annotations)

  # Manage missing values
  # Only `uk_coal_employ` data set has missing values (2 missing values),
  # and there are all located before any ground truth label, so we consider the
  # series w/o missing values and we shift the corresponding label indexes
  row = which(rownames(df) == "uk_coal_employ")
  for(col in 1:ncol(df)) {
    if(length(df[[col]][[row]]) >= 1) {
      df[[col]][[row]] = df[[col]][[row]] - 2
    }
  }

  # Shift all the indexes by 1 to match our index convention, and convert to integers
  for(col in 1:ncol(df)) {
    for(row in 1:nrow(df)) {
      if(length(df[[col]][[row]]) >= 1) {
        df[[col]][[row]] = as.integer(df[[col]][[row]] + 1)
      }
    }
  }

  # Re-arrange alphabetically
  df$name = rownames(df)
  df = df %>% arrange(name) %>% select(-name)

  # Adding median ground truth based on the Jaccard index, for each data set
  df$groundtruth = compute_median_user_groundtruth(df)

  # Re-order the columns to put the median ground truth at first
  df = df %>% select(groundtruth, everything())

  return(df)
}

#' Compute the Jaccard index of two vectors
#'
#' @param x vector of index where a change is detected for a certain user
#' @param y vector of index where a change is detected for another user
#' @return the Jaccard index computed as the size of the intersection over the
#' size of the union
jaccard_func = function(x, y) {
  if(length(x) == 0) {
    return(0)
  }
  sum(x %in% y) / length(unique(c(x,y)))
}

#' Compute the median ground truth user given the labels given by multiple users
#'
#' @param df containing the ground truth of each user (columns) for each data
#' set (rows)
#' @return a list (one element per data set) of the median ground truth user
compute_median_user_groundtruth = function(df) {
  df_gt = list()
  for(dataset in rownames(df)) {
    # data frame of Jaccard index between each couple of users
    df_j = list()
    for(i in 1:(ncol(df)-1)) {
      for(j in (i+1):ncol(df)) {
        label_i = df[[i]][[dataset]]
        label_j = df[[j]][[dataset]]
        if(!is.null(label_i) & !is.null(label_j)) {
          jac_cur = jaccard_func(label_i, label_j)
          df_j[[length(df_j) + 1]] = data.frame(user1 = i, user2 = j, jaccard = jac_cur)
        }
      }
    }
    df_j = bind_rows(df_j)
    users = unique(c(df_j$user1, df_j$user2))
    # compute the average Jaccard of user[i] against all other users
    val = rep(NA, length(users))
    for(i in 1:length(users)) {
      user = users[i]
      val[i] = mean(df_j[df_j$user1 == user | df_j$user2 == user,]$jaccard)
    }
    idx_max = users[which(val == max(val))] # users maximizing the Jaccard index
    res = df[[idx_max[1]]][[dataset]]
    df_gt[[dataset]] = res
  }
  return(df_gt)
}
