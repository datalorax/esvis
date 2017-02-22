
test_that("`pp_plot` produces expected output", {
	expect_equal(pp_plot(mean ~ grade, seda, 
					return = TRUE)$type, 
				"n")
	
	expect_equal(pp_plot(mean ~ grade, seda, 
					return = TRUE)$xlab, 
				"p(8)")
	
	expect_equal(pp_plot(mean ~ grade, seda, 
					return = TRUE)$ylab, 
				"p(Focal Group)")
	
	expect_equal(pp_plot(mean ~ subject, seda, 
					return = TRUE)$ylab, 
				"p(ela)")

	expect_equal(length(pp_plot(mean ~ grade, seda, 
					return = TRUE)$col), 
				5)
	
	expect_equal(length(pp_plot(mean ~ grade, seda, 
					col = "blue", 
					return = TRUE)$col), 
				1)

	expect_equal(length(pp_plot(mean ~ subject, seda, 
					return = TRUE)$col), 
				1)

	expect_equal(pp_plot(mean ~ grade, seda, 
					3,
					return = TRUE)$ref_group, 
				3)

	expect_equal(pp_plot(mean ~ grade, seda, 
					3,
					return = TRUE)$xlab, 
				"p(3)")
})

test_that("Partial matching for `pp_plot` works", {
	expect_equal(pp_plot(mean ~ grade,  seda, 
					m = "new title", 
					return = TRUE)$main,
				"new title")
	
	expect_equal(pp_plot(mean ~ grade,  seda, 
					xla = "new title", 
					return = TRUE)$xlab,
				"new title")

	expect_equal(pp_plot(mean ~ grade,  seda, 
					yla = "new title", 
					return = TRUE)$ylab,
				"new title")
})

test_that("`pp_plot` throws warnings when it should", {
	expect_warning(pp_plot(mean ~ grade, seda,
					shade = TRUE,
					return = TRUE))

	expect_warning(pp_plot(mean ~ grade, seda,
					text = TRUE,
					return = TRUE))

	expect_warning(pp_plot(mean ~ grade, seda,
					lty = 1:2,
					return = TRUE))

	expect_warning(pp_plot(mean ~ grade, seda,
					col = 1:2,
					return = TRUE))
})
