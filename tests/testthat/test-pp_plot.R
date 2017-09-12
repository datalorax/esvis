
test_that("`pp_plot` produces expected output", {
	expect_equal(pp_plot(mean ~ grade, seda)$type, 
				"n")
	
	expect_equal(pp_plot(mean ~ grade, seda)$xlab, 
				"p(8)")
	
	expect_equal(pp_plot(mean ~ grade, seda)$ylab, 
				"p(Focal Group)")
	
	par(mfrow = c(1, 1))
	expect_equal(pp_plot(mean ~ subject, seda)$ylab, 
				"p(ela)")

	expect_equal(length(pp_plot(mean ~ grade, seda)$col), 
				5)
	
	expect_equal(length(pp_plot(mean ~ stateabb, seda)$col), 
				50)

	expect_equal(length(pp_plot(mean ~ grade, seda, 
					col = "blue")$col), 
				1)

	par(mfrow = c(1, 1))
	expect_equal(length(pp_plot(mean ~ subject, seda)$col), 
				1)

	expect_equal(pp_plot(mean ~ grade, seda, 
					3)$ref_group, 
				3)

	expect_equal(pp_plot(mean ~ grade, seda, 
					3)$xlab, 
				"p(3)")
	expect_equal(pp_plot(mean ~ grade, seda, leg = "base")$leg,
		"base")

	expect_equal(pp_plot(mean ~ grade, seda, scheme = "viridis")$scheme,
		"viridis")
	expect_equal(pp_plot(mean ~ grade, seda, scheme = "inferno")$scheme,
		"inferno")
	expect_equal(pp_plot(mean ~ grade, seda, scheme = "magma")$scheme,
		"magma")
	expect_equal(pp_plot(mean ~ grade, seda, scheme = "plasma")$scheme,
		"plasma")

	par(mfrow = c(1, 1))
	expect_equal(pp_plot(math ~ frl, benchmarks, scheme = "viridis")$scheme,
		"viridis")
	expect_equal(pp_plot(math ~ frl, benchmarks, scheme = "inferno")$scheme,
		"inferno")
	expect_equal(pp_plot(math ~ frl, benchmarks, scheme = "magma")$scheme,
		"magma")
	expect_equal(pp_plot(math ~ frl, benchmarks, scheme = "plasma")$scheme,
		"plasma")
})

test_that("Partial matching for `pp_plot` works", {
	expect_equal(pp_plot(mean ~ grade,  seda, 
					m = "new title")$main,
				"new title")
	
	expect_equal(pp_plot(mean ~ grade,  seda, 
					xla = "new title")$xlab,
				"new title")

	expect_equal(pp_plot(mean ~ grade,  seda, 
					yla = "new title")$ylab,
				"new title")
})

test_that("`pp_plot` throws warnings when it should", {
	expect_warning(pp_plot(mean ~ grade, seda,
					shade = TRUE))

	expect_warning(pp_plot(mean ~ grade, seda,
					text = TRUE))

	expect_warning(pp_plot(mean ~ grade, seda,
					lty = 1:2))

	expect_warning(pp_plot(mean ~ grade, seda,
					col = 1:2))
	par(mfrow = c(1, 2))
	expect_warning(pp_plot(mean ~ subject, seda))

	par(mfrow = c(4, 2))
	expect_message(pp_plot(mean ~ subject, seda, leg = "side"))
})

test_that("dark theme works for `pp_plot`", {
	par(mfrow = c(1, 1))
	expect_equal(pp_plot(mean ~ grade, seda, theme = "dark")$theme, "dark")
	
	par(mfrow = c(1, 1))
	expect_equal(pp_plot(mean ~ subject, seda, theme = "dark")$theme, "dark")
	
	expect_equal(pp_plot(mean ~ grade, seda, 
		theme = "dark", 
		leg = "base")$leg,
		"base")
})
