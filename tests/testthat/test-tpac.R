tmp <- data.frame(group = rep(c(1, 2), each = 100),
				  score = c(1:100, 51:150))
seda$constant <- 1

test_that("Transformed percentage above  cut computes and outputs correctly", {
	expect_equal(tpac(score ~ group, tmp, 75, 1)$estimate, 
		qnorm(.25) - qnorm(.75))
	
	expect_equal(nrow(tpac(mean ~ grade, seda, 225)), (6^2) - 6)
	expect_equal(nrow(tpac(mean ~ grade, seda, 225, 3)), 5)

	expect_output(str(tpac(score ~ group, tmp, 75)), "data.frame")
	expect_output(str(tpac(score ~ group, tmp, 75, tidy = FALSE)), "Named num")
	expect_output(str(tpac(score ~ group, tmp, 75, diff = FALSE)), 
		"data.frame")
	expect_output(str(tpac(score ~ group, tmp, 75, 
							ref_group = 2, 
							diff = FALSE, 
							tidy = FALSE)), 
					"Named num")

	expect_warning(tpac(mean ~ grade, seda, 10))
	expect_warning(tpac(mean ~ constant, seda, 225))
})


test_that("Multiple cuts work as expected", {
	expect_output(str(tpac(math ~ sex, star, c(480, 500, 520), 
							ref_group = "girl",
							diff = FALSE)), 
				"data.frame")
	expect_equal(nrow(tpac(math ~ sex, star, c(480, 500, 520), 
							ref_group = "girl",
							diff = FALSE)), 
				3)
	expect_equal(ncol(tpac(math ~ sex, star, c(480, 500, 520), 
							ref_group = "girl",
							diff = FALSE)), 
				3)

	expect_output(str(tpac(math ~ sex, star, c(480, 500, 520), 
							ref_group = "girl")), 
				"data.frame")
	expect_equal(nrow(tpac(math ~ sex, star, c(480, 500, 520), 
							ref_group = "girl")), 
				3)
	expect_equal(ncol(tpac(math ~ condition, star, c(480, 500, 520))), 
				4)
	expect_output(str(tpac(math ~ condition, star, c(480, 500, 520), 
					tidy = FALSE)), "num")

})
