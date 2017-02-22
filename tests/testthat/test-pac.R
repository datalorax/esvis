tmp <- data.frame(group = rep(c(1, 2), each = 100),
				  score = c(1:100, 51:150))

test_that("Percentage above the cut computes and outputs correctly", {
	expect_equal(pac(score ~ group, tmp, 50, 2)$estimate, .5)
	expect_equal(nrow(pac(mean ~ grade, seda, 50)), (6^2) - 6)
	expect_output(str(pac(score ~ group, tmp, 50)), "data.frame")
})
