test_that("`ecdf_plot` produces expected output", {
  p <- ecdf_plot(benchmarks, math ~ ell)

  expect_equal(p$labels$x, "math")
  expect_equal(ecdf_plot(star, reading ~ race)$labels$x, "reading")

	expect_equal(length(p$layers), 1)

	expect_equal(length(ecdf_plot(benchmarks, math ~ ell,
	                            cuts = c(180, 190))$layers), 
				3)
	expect_equal(length(ecdf_plot(benchmarks, math ~ ell,
	                            cuts = c(180, 190),
	                            ref_rect = FALSE)$layers), 
				2)
	
	expect_null(p$facet$params$facets$panel)
	expect_null(p$facet$params$rows)
	expect_null(p$facet$params$cols)
	
	p2 <- ecdf_plot(benchmarks, math ~ ell + season)
	expect_false(is.null(p2$facet$params$facets))
	
	p3 <- ecdf_plot(benchmarks, math ~ ell + season + frl)
  expect_false(is.null(p3$facet$params$rows))
	expect_false(is.null(p3$facet$params$cols))
})
