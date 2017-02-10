
test_that("cdfs function returns as expected", {
	expect_equal(length(cdfs(mean ~ grade, seda)) == 6, TRUE)
	expect_equal(length(cdfs(mean ~ subject, seda)) == 2, TRUE)
	expect_equal(unique(sapply(cdfs(mean ~ grade, seda), typeof)), "closure")
})

