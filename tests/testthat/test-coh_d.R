# test_data <- data.frame(g = c(rep(1, 1e4), rep(2, 1e4)),
# 						score = c(rnorm(1e4), rnorm(1e4, 1)))

# test_that("Cohen's d computes correctly", {
# 	expect_equal(round(coh_d(score ~ g, test_data, FALSE)), 1)
# 	expect_equal(dim(coh_d(score ~ g, test_data)), c(2, 2))
# 	expect_warning(coh_d(mean ~ grade, seda, FALSE))
# })

