test_data1 <- data.frame(g = c(rep(1, 1e4), rep(2, 1e4)),
						score = c(rnorm(1e4), rnorm(1e4)))
test_data2 <- data.frame(g = c(rep(1, 1e4), rep(2, 1e4)),
						score = c(rnorm(1e4), rnorm(1e4, 1)))

test_that("Cohen's d computes correctly", {
	expect_equal(round(auc(score ~ g, test_data1, FALSE), 1), .5)
	expect_equal(round(auc(score ~ g, test_data2, FALSE), 2), .75, 
		tolerance = .011)
	expect_equal(dim(auc(score ~ g, test_data1)), c(2, 2))
	expect_equal(dim(auc(mean ~ grade, seda)), c(6, 6))
})
