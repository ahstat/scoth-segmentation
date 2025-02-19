test_that("pipeline_scoring_series is correct for a simple univariate series", {
  set.seed(1234)
  x = c(rnorm(100), rnorm(100, 3))

  cost_methods = c("cusum_L2", "reg_cusum_L2")
  output = pipeline_scoring_series(x, cost_methods)

  for(i in 1:nrow(output)) {
    scores = output$scoring[[i]]
    largest_score_idx = (scores %>% arrange(-score) %>% slice(1))$index
    # 101 is the true change point, and we check it is within the right interval
    expect_true(largest_score_idx > 95)
    expect_true(largest_score_idx < 105)
  }
})

test_that("pipeline_scoring_series is correct for a simple multivariate series", {
  set.seed(1234)
  x = data.frame(V1 = c(rnorm(100), rnorm(100, 3)), V2 = c(rnorm(100), rnorm(100, 3)))

  cost_methods = c("cusum_L2", "reg_cusum_L2")
  output = pipeline_scoring_series(x, cost_methods)

  for(i in 1:nrow(output)) {
    scores = output$scoring[[i]]
    largest_score_idx = (scores %>% arrange(-score) %>% slice(1))$index
    # 101 is the true change point, and we check it is within the right interval
    expect_true(largest_score_idx > 95)
    expect_true(largest_score_idx < 105)
  }
})
