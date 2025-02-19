test_that("the scoring of metrics_TCPD is consistent with the python code from the original source", {
  # The original source for metrics_TCPD is:
  # https://github.com/alan-turing-institute/TCPDBench/blob/master/analysis/scripts/metrics.py
  #
  # This file contains check points regarding the output of the functions that we reproduce below.

  ## metrics_TCPD_true_positives ----
  expect_equal(metrics_TCPD_true_positives(c(1, 10, 20, 23), c(3, 8, 20)), c(1,10,20))
  expect_equal(metrics_TCPD_true_positives(c(1, 10, 20, 23), c(1, 3, 8, 20)), c(1,10,20))
  expect_equal(metrics_TCPD_true_positives(c(1, 10, 20, 23), c(1, 3, 5, 8, 20)), c(1,10,20))
  expect_equal(metrics_TCPD_true_positives(c(), c(1,2,3)), c())
  expect_equal(metrics_TCPD_true_positives(c(1,2,3), c()), c())

  ## metrics_TCPD_f_measure ----
  annotations = list(user_1 = c(10, 20), user_2 = c(11, 20), user_3 = c(10), user_4 = c(0, 5))
  predictions = c(10, 20)
  expect_equal(metrics_TCPD_f_measure(annotations, predictions), 1)
  annotations = list(user_1 = c(), user_2 = c(10), user_3 = c(50))
  predictions = c(10)
  expect_equal(metrics_TCPD_f_measure(annotations, predictions), 10/11)
  annotations = list(user_1 = c(), user_2 = c(10), user_3 = c(50))
  predictions = c()
  expect_equal(metrics_TCPD_f_measure(annotations, predictions), 0.8)

  ## metrics_TCPD_overlap ----
  expect_equal(metrics_TCPD_overlap(c(1,2,3), c()), 0)
  expect_equal(metrics_TCPD_overlap(c(1,2,3), c(2,5)), 0.25)
  expect_equal(metrics_TCPD_overlap(c(), c(1,2,3)), 0)
  expect_equal(metrics_TCPD_overlap(c(1,2,3), c(1,2,3)), 1)

  ## metrics_TCPD_partition_from_cps ----
  expect_equal(metrics_TCPD_partition_from_cps(c(), 5), list(c(0,1,2,3,4)))
  expect_equal(metrics_TCPD_partition_from_cps(c(3,5), 8), list(c(0,1,2), c(3,4), c(5,6,7)))
  expect_equal(metrics_TCPD_partition_from_cps(c(1,2,7), 8), list(c(0), c(1), c(2,3,4,5,6), c(7)))
  expect_equal(metrics_TCPD_partition_from_cps(c(0,4), 6), list(c(0,1,2,3), c(4,5)))

  ## metrics_TCPD_cover_single ----
  S = list(c(1, 2, 3), c(4, 5, 6))
  Sprime = list(c(1,2,3), c(4,5), 6)
  expect_equal(metrics_TCPD_cover_single(S, Sprime), 5/6)

  S = list(c(1, 2, 3, 4, 5, 6))
  Sprime = list(c(1,2,3,4), c(5,6))
  expect_equal(metrics_TCPD_cover_single(S, Sprime), 2/3)

  S = list(c(1, 2, 3), c(4,5,6))
  Sprime = list(c(1,2), c(3,4), c(5,6))
  expect_equal(metrics_TCPD_cover_single(S, Sprime), 2/3)

  S = list(1,2,3, c(4,5,6))
  Sprime = list(c(1,2,3,4,5,6))
  expect_equal(metrics_TCPD_cover_single(S, Sprime), 1/3)

  ## metrics_TCPD_covering ----
  annotations = list(user_1 = c(10, 20), user_2 = 10, user_3 = c(0,5))
  predictions = c(10, 20)
  n_obs = 45
  expect_equal(metrics_TCPD_covering(annotations, predictions, n_obs)$covering, 0.7962962962962963)

  annotations = list(user_1=c(), user_2 = 10, user_3 = 40)
  predictions = 10
  n_obs = 45
  expect_equal(metrics_TCPD_covering(annotations, predictions, n_obs)$covering, 0.7954144620811286)

  annotations = list(user_1=c(), user_2 = 10, user_3 = 40)
  predictions = c()
  n_obs = 45
  expect_equal(metrics_TCPD_covering(annotations, predictions, n_obs)$covering, 0.8189300411522634)
})
