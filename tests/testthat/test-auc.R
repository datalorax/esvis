set.seed(100)
test_data1 <- data.frame(g = c(rep(1, 1e4), rep(2, 1e4)),
						score = c(rnorm(1e4), rnorm(1e4)))
test_data2 <- data.frame(g = c(rep(1, 1e4), rep(2, 1e4)),
						score = c(rnorm(1e4), rnorm(1e4, 1)))

test_that("Area under the curve computes and outputs correctly", {
	expect_true(round(auc(score ~ g, test_data1, 1, tidy = FALSE), 1) == .5)
	expect_output(str(auc(score ~ g, test_data1)), "data.frame")
	expect_output(str(auc(score ~ g, test_data1, tidy = FALSE)), "Named num")
	expect_equal(names(auc(score ~ g, test_data1, tidy = FALSE)), 
		c("1-2", "2-1"))
	expect_equal(nrow(auc(mean ~ grade, seda, 8)), 5)
})
