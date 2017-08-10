tmp <- data.frame(group = rep(c(1, 2), each = 100),
				  score = c(1:100, 51:150))

test_that("Percentage above the cut computes and outputs correctly", {
	expect_equal(pac(score ~ group, tmp, 50, 2)$estimate, .5)
	expect_equal(nrow(pac(mean ~ grade, seda, 50)), (6^2) - 6)
	expect_equal(nrow(pac(mean ~ grade, seda, 50, 3)), 5)
	expect_equal(nrow(pac(score ~ group, tmp, 50, 2)), 1)

	expect_output(str(pac(score ~ group, tmp, 50)), "data.frame")
	expect_output(str(pac(score ~ group, tmp, 50, diff = FALSE)), "data.frame")
	expect_output(str(pac(score ~ group, tmp, 50, 2)), "data.frame")	
	
	expect_output(str(pac(score ~ group, tmp, 50, 
							diff = FALSE, 
							tidy = FALSE)), 
				"Named num")
	
	expect_output(str(pac(score ~ group, tmp, 50, 
							ref_group = 2,
							diff = FALSE, 
							tidy = FALSE)), 
				"Named num")
	
	expect_output(str(pac(score ~ group, tmp, 50, 
							ref_group = 2,
							diff = FALSE)), 
				"data.frame")

	expect_output(str(pac(score ~ group, tmp, 50, 
							tidy = FALSE)), 
				"Named num")
	
	expect_output(str(pac(score ~ group, tmp, 50, 2,
							tidy = FALSE)), 
				"Named num")
})

test_that("Multiple cuts work as expected", {
	expect_output(str(pac(math ~ sex, star, c(480, 500, 520), 
							ref_group = "girl",
							diff = FALSE)), 
				"data.frame")
	expect_equal(nrow(pac(math ~ sex, star, c(480, 500, 520), 
							ref_group = "girl",
							diff = FALSE)), 
				3)
	expect_equal(ncol(pac(math ~ sex, star, c(480, 500, 520), 
							ref_group = "girl",
							diff = FALSE)), 
				3)

	expect_output(str(pac(math ~ sex, star, c(480, 500, 520), 
							ref_group = "girl")), 
				"data.frame")
	expect_equal(nrow(pac(math ~ sex, star, c(480, 500, 520), 
							ref_group = "girl")), 
				3)
	expect_equal(ncol(pac(math ~ sex, star, c(480, 500, 520), 
							ref_group = "girl")), 
				4)

})
