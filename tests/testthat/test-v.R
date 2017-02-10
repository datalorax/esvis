test_data1 <- data.frame(g = c(rep(1, 1e4), rep(2, 1e4)),
						score = c(rnorm(1e4), rnorm(1e4)))
test_data2 <- data.frame(g = c(rep(1, 1e4), rep(2, 1e4)),
						score = c(rnorm(1e4), rnorm(1e4, 1)))

test_that("v returns expected output", {
	expect_equal(round(v(score ~ g, test_data1), 1), 0, tolerance = 0.11)
	expect_equal(dim(v(score ~ g, test_data1)), NULL)
	expect_equal(dim(v(mean ~ grade, seda)), c(6, 6))
})
