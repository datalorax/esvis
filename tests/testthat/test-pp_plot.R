test_that("`pp_plot` produces expected output", {
	p1 <- pp_plot(benchmarks, math ~ ell)
  expect_equal(p1$labels$y, "Monitor")
	
	expect_equal(pp_plot(benchmarks, math ~ ell,
	                     ref_group = "Non-ELL")$labels$y, 
				"Non-ELL")
	
	expect_equal(length(p1$layers), 3)
	
	expect_equal(length(pp_plot(benchmarks, math ~ ell,
	                     shade = FALSE)$layers), 
				2)
	
	expect_equal(length(pp_plot(benchmarks, math ~ ell,
	                     lines = FALSE)$layers), 
				2)
	expect_equal(length(pp_plot(benchmarks, math ~ ell,
	                     refline = FALSE)$layers), 
				2)
	
	expect_equal(length(pp_plot(benchmarks, math ~ ell,
	                            shade = FALSE,
	                            refline = FALSE)$layers), 
				1)
	
	expect_equal(length(pp_plot(benchmarks, math ~ ell,
	                            cuts = c(180, 190),
	                            shade = FALSE,
	                            refline = FALSE)$layers), 
				4)
	
	expect_null(p1$facet$params$facets)
	expect_null(p1$facet$params$rows)
	expect_null(p1$facet$params$cols)
	
	p2 <- pp_plot(benchmarks, math ~ ell + season)
	expect_false(is.null(p2$facet$params$facets))
	
	p3 <- pp_plot(benchmarks, math ~ ell + season + frl)
	expect_false(is.null(p3$facet$params$rows))
	expect_false(is.null(p3$facet$params$cols))
	
	p4 <- pp_plot(benchmarks, math ~ ell, cuts = c(180, 190))
  expect_equal(length(p4$layers), 6)
  
  p5 <- pp_plot(benchmarks, math ~ ell + frl, cuts = c(180, 190))
  expect_equal(length(p5$layers), 6)
  
  p6 <- pp_plot(benchmarks, math ~ ell, 
                cuts = c(180, 190), 
                cut_labels = FALSE)
  expect_equal(length(p6$layers), 5)
	
})
