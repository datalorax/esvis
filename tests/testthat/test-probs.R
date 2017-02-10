
test_that("Cohen's d computes correctly", {
	expect_equal(ncol(probs(mean ~ grade, seda)), 6)
	expect_equal(colnames(probs(mean ~ year, seda)) %in% 
		unique(seda$year), rep(TRUE, 5))
})
