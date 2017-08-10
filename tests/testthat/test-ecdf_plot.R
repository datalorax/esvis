test_that("`ecdf_plot` produces expected output", {
	expect_equal(ecdf_plot(mean ~ grade, seda)$type, "n")

	expect_equal(ecdf_plot(mean ~ grade, seda)$xlab, 
				"mean")
	
	expect_equal(ecdf_plot(mean ~ grade, seda)$ylab, 
				"Proportion")
	
	expect_equal(ecdf_plot(mean ~ subject, seda, xlab = "aye")$xlab, 
				"aye")

	expect_equal(length(ecdf_plot(mean ~ grade, seda)$col), 
				6)

	expect_equal(length(ecdf_plot(math ~ condition, star)$col), 
				3)

	expect_equal(length(ecdf_plot(mean ~ grade, seda, 
					col = "blue")$col), 
				1)

	expect_equal(ecdf_plot(mean ~ grade, seda, c(200, 210, 220))$ref_cut,
				quote(c(200, 210, 220)))
	expect_equal(ecdf_plot(mean ~ grade, seda, theme = "dark")$theme, 
				"dark")
	expect_equal(ecdf_plot(mean ~ grade, seda, 
		c(200, 210, 220), 
		theme = "dark")$theme, 
				"dark")

	par(mfrow = c(1, 1))
	expect_equal(ecdf_plot(mean ~ grade, seda, legend = "base")$legend,
		"base")
	expect_equal(ecdf_plot(mean ~ grade, seda, annotate = TRUE)$annotate,
		TRUE)

	expect_equal(ecdf_plot(mean ~ grade, seda, 
		ref_cut = c(150, 200, 250),
		hor_ref = TRUE)$hor_ref, TRUE)

	expect_equal(ecdf_plot(mean ~ grade, seda, 
		legend = "base",
		theme = "dark")$theme, "dark")

})

test_that("Partial matching for `ecdf_plot` works", {
	expect_equal(ecdf_plot(mean ~ grade,  seda, 
					m = "new title")$main,
				"new title")
	
	expect_equal(ecdf_plot(mean ~ grade,  seda, 
					xla = "new title")$xlab,
				"new title")

	expect_equal(ecdf_plot(mean ~ grade,  seda, 
					yla = "new title")$ylab,
				"new title")
})
