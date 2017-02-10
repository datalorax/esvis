test_that("formula parser works as expected", {
	expect_equal(length(parse_form(mean ~ subject, seda)), 2)
	expect_equal(length(parse_form(mean ~ grade, seda)), 6)
})

