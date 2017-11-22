set.seed(100)
test_data1 <- data.frame(g = c(rep(1, 1e4), rep(2, 1e4)),
						score = c(rnorm(1e4), rnorm(1e4)))
test_data2 <- data.frame(g = c(rep(1, 1e4), rep(2, 1e4)),
						score = c(rnorm(1e4), rnorm(1e4, 1)))

test_that("Area under the curve computes and outputs correctly", {
	expect_true(round(hedg_g(score ~ g, test_data1, 1, tidy = FALSE), 1) == 0)
	expect_true(round(hedg_g(score ~ g, test_data2, 1, tidy = FALSE), 1) == 1)
	expect_output(str(hedg_g(score ~ g, test_data1)), "data.frame")
	expect_output(str(hedg_g(score ~ g, test_data1, tidy = FALSE)), 
		"Named num")
	expect_equal(names(hedg_g(score ~ g, test_data1, tidy = FALSE)), 
		c("1-2", "2-1"))
	expect_equal(nrow(hedg_g(mean ~ grade, seda, 8)), 5)
})
