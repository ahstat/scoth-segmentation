test_that("metrics_F1_TP_func is correct in all corner cases", {
  # W=1 corner case:
  #        1 2 3 4 5
  # gt:      x   o
  # gtW=1: x x ? o o
  # pred       X
  #
  # There is decision to make, since either:
  # - the pred is fine for both: TP=2, or
  # - the pred is fine for one of them: TP=1 (*), or
  # - the pred is fine for none of them: TP=0 (option=1)



  # https://ceur-ws.org/Vol-1226/paper31.pdf

  # All options are OK but will give different results
  # Recall are resp.   : 1, 0.5, 0
  # Precision are resp.: 2,   1, 0 (the 2 can be understood after, saying the pred is dedoubled, then giving precision=1 instead)
  # The option 2 is selected below

  # with gt_idx = 0, always 0 TP ----
  gt_idx = c()
  for(W in 0:2) {
    expect_equal(metrics_F1_TP_func(c(), gt_idx, W), 0)
    expect_equal(metrics_F1_TP_func(c(1000), gt_idx, W), 0)
    expect_equal(metrics_F1_TP_func(c(1000, 1001), gt_idx, W), 0)
  }

  # with gt_idx of length 1, TP is 0 or 1 (with 1 or 2 predictions) ----
  gt_idx = 10
  for(W in 0:5) {
    expect_equal(metrics_F1_TP_func(10, gt_idx, W), 1)
  }
  W = 0
  expect_equal(metrics_F1_TP_func(c(), gt_idx, W), 0)
  expect_equal(metrics_F1_TP_func(9, gt_idx, W), 0)
  W = 1
  expect_equal(metrics_F1_TP_func(c(), gt_idx, W), 0)
  expect_equal(metrics_F1_TP_func(9, gt_idx, W), 1)
  expect_equal(metrics_F1_TP_func(8, gt_idx, W), 0)
  W = 1
  expect_equal(metrics_F1_TP_func(c(8,10), gt_idx, W), 1)
  expect_equal(metrics_F1_TP_func(c(8,9,10,11,12), gt_idx, W), 1)
  W = 0
  expect_equal(metrics_F1_TP_func(c(8,9,10,11,12), gt_idx, W), 1)
  expect_equal(metrics_F1_TP_func(c(8,9,11,12), gt_idx, W), 0)

  # with gt_idx of length 2, TP can be 0, 1 or 2, but care is taken
  # for the possible intersection of windows
  gt_idx = c(5, 10)
  #      1 2 3 4 5 6 7 8 9 10
  # W=0          x         o
  # W=1        x x x     o o o
  # W=2      x x x x x o o o o o
  # W=3    x x x x x x o o o o o o (overlap)
  W = 1
  expect_equal(metrics_F1_TP_func(c(7,8), gt_idx, W), 0)
  expect_equal(metrics_F1_TP_func(c(7,8,9), gt_idx, W), 1)
  expect_equal(metrics_F1_TP_func(c(6,7,8,9), gt_idx, W), 2)
  W = 2
  expect_equal(metrics_F1_TP_func(c(7,8), gt_idx, W), 2)
  W = 3
  expect_equal(metrics_F1_TP_func(c(7,8), gt_idx, W), 2)

  gt_idx = c(6, 10)
  #      1 2 3 4 5 6 7 8 9 10
  # W=0            x       o
  # W=1          x x x   o o o
  # W=2        x x x x ? o o o o   (overlap)
  # W=3      x x x x x ? o o o o o (overlap)
  W = 3
  # expect_equal(metrics_F1_TP_func(c(8), gt_idx, W), 0) # very conservative
  expect_equal(metrics_F1_TP_func(c(8), gt_idx, W), 1) # ok take one as fine
  W = 50
  # expect_equal(metrics_F1_TP_func(c(8), gt_idx, W), 0) # very conservative
  expect_equal(metrics_F1_TP_func(c(8), gt_idx, W), 1) # ok take one as fine
})

