set.seed(100)
test_data1 <- data.frame(g = c(rep(1, 1e4), rep(2, 1e4)),
						score = c(rnorm(1e4), rnorm(1e4)))
test_data2 <- data.frame(g = c(rep(1, 1e4), rep(2, 1e4)),
						score = c(rnorm(1e4, sd = 5), rnorm(1e4, sd = 5)))

test_that("`pooled_sd` produces expected output", {
	expect_equal(
		round(pooled_sd(score ~ g, test_data1)$estimate[1]), 1)
	expect_equal(
		round(pooled_sd(score ~ g, test_data2)$estimate[1]), 5)
})

test_data3 <- data.frame(g = c(rep(1, 1e4), rep(2, 1e4)),
						score = c(rnorm(1e4, 100, 1), rnorm(1e4, 0, 1)))

test_that("`qtile_mean_diffs` produces expected output", {
	expect_equal(
		round(qtile_mean_diffs(score ~ g, test_data3)$estimate[1:3]), 
			rep(-100, 3))
	expect_equal(
		round(qtile_mean_diffs(score ~ g, test_data3, 
			qtiles = seq(0, 1, .2))$estimate[1:5]), 
			rep(-100, 5))
})

test_that("`qtile_es` produces expected output", {
	expect_equal(round(qtile_es(score ~ g, test_data1)$es[1]), 0)
})

test_that("`binned_plot` produces expected output", {
	expect_equal(binned_plot(mean ~ grade, seda)$type, "n")
	
	expect_equal(binned_plot(mean ~ grade, seda)$xlab, 
				"Quantiles (ref group: 8)")
	
	expect_equal(binned_plot(mean ~ grade, seda)$ylab, 
				"Effect Size")
	
	expect_equal(binned_plot(mean ~ subject, seda)$xlab, 
				"Quantiles (ref group: math)")

	expect_equal(length(binned_plot(mean ~ grade, seda)$col), 
				5)

	expect_equal(length(binned_plot(math ~ tch_experience, star)$col), 
				24)

	expect_equal(length(binned_plot(mean ~ grade, seda, 
					col = "blue")$col), 
				1)

	expect_equal(length(binned_plot(mean ~ subject, seda)$col), 
				1)

	expect_equal(binned_plot(mean ~ grade, seda, 
					3)$ref_group, 
				3)
	expect_equal(binned_plot(mean ~ grade, seda, 
					3, theme = "dark")$theme, 
				"dark")

	par(mfrow = c(1, 1))
	expect_equal(binned_plot(mean ~ grade, seda, legend = "base")$legend,
		"base")
	expect_equal(binned_plot(mean ~ grade, seda, annotate = TRUE)$annotate,
		TRUE)
})

test_that("Partial matching for `binned_plot` works", {
	expect_equal(binned_plot(mean ~ grade,  seda, 
					m = "new title")$main,
				"new title")
	
	expect_equal(binned_plot(mean ~ grade,  seda, 
					xla = "new title")$xlab,
				"new title")

	expect_equal(binned_plot(mean ~ grade,  seda, 
					yla = "new title")$ylab,
				"new title")
})
