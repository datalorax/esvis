test_data <- data.frame(g = c(rep(1, 1e4), rep(2, 1e4)),
						score = c(rnorm(1e4), rnorm(1e4, 1)))

test_that("Hedges' g computes correctly", {
	expect_equal(round(hedg_g(score ~ g, test_data, FALSE)), 1)
	expect_equal(dim(hedg_g(score ~ g, test_data)), c(2, 2))
	expect_warning(hedg_g(mean ~ grade, seda, FALSE))
	expect_equal(coh_d(score ~ g, test_data)[1, 2] >
		    	 hedg_g(score ~ g, test_data)[1, 2], TRUE)
})

