
test_that("cdfs function returns as expected", {
	expect_equal(length(cdfs(mean ~ grade, seda)) == 6, TRUE)
	expect_equal(length(cdfs(mean ~ subject, seda)) == 2, TRUE)
	expect_equal(
		unique(
			vapply(
				cdfs(mean ~ grade, seda), 
				typeof, 
				character(1)
				)
			), 
		"closure")
})

