tmp <- data.frame(group = rep(c(1, 2), each = 100),
				  score = c(1:100, 51:150))
seda$constant <- 1

test_that("Transformed percentage above  cut computes and outputs correctly", {
	expect_equal(tpac(score ~ group, tmp, 75, 1)$estimate, 
		qnorm(.25) - qnorm(.75))
	
	expect_equal(nrow(tpac(mean ~ grade, seda, 225)), (6^2) - 6)
	expect_equal(nrow(tpac(mean ~ grade, seda, 225, 3)), 5)

	expect_output(str(tpac(score ~ group, tmp, 75)), "data.frame")

	expect_warning(tpac(mean ~ grade, seda, 10))
	expect_warning(tpac(mean ~ constant, seda, 225))
})
