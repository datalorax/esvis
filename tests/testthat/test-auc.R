set.seed(100)
test_data1 <- data.frame(g     = c(rep(1, 1e4), rep(2, 1e4)),
                         score = c(round(rnorm(1e4), 5), 
                                   round(rnorm(1e4), 5)))
test_data2 <- data.frame(g     = c(rep(1, 1e4), rep(2, 1e4)),
                         score = c(round(rnorm(1e4), 5), 
                                   round(rnorm(1e4, 1), 5)))

test_that("Area under the curve computes and outputs correctly", {
	expect_equal(auc(test_data1, score ~ g)$auc[1], .50, tolerance = 0.03)
	expect_equal(auc(test_data2, score ~ g)$auc[1], .75, tolerance = 0.03)
})

# ((Levels 1 * Levels 2) * (Levels 1 * Levels 2)) - (Levels 1 * Levels 2) 
test_that("Reference group subsetting works correctly", {
  expect_equal(nrow(auc(seda, mean ~ grade)), 6*5)
  expect_equal(nrow(auc(seda, mean ~ grade, ~`8`)), 5)
  expect_equal(nrow(auc(seda, mean ~ grade, "8")), 5)
  expect_equal(nrow(auc(benchmarks, math ~ season, "Fall")), 2)
  expect_equal(nrow(auc(benchmarks, math ~ season, ~Winter)), 2)
  expect_equal(nrow(auc(benchmarks, math ~ season + ell)), 
               ((3*3)*(3*3)) - (3*3))
  expect_equal(nrow(auc(benchmarks, math ~ season + ell, 
                        ~Fall + `Non-ELL`)), 
               (3*3) - 1)
  expect_equal(nrow(auc(benchmarks, math ~ season + ell, 
                        c("Fall", "Non-ELL"))), 
               (3*3) - 1)
  expect_equal(nrow(auc(benchmarks, math ~ season + ell, 
                        ~Fall)), 
               (3*3*3) - 3)
  expect_equal(nrow(auc(benchmarks, math ~ season + ell, 
                        c("Fall"))), 
               (3*3*3) - 3)
  
  expect_equal(nrow(auc(benchmarks, math ~ season + frl + ethnicity)), 
               ((3*2*6)*(3*2*6)) - (3*2*6))
  expect_equal(nrow(auc(benchmarks, math ~ season + frl + ethnicity,
                        ~Fall + `Non-FRL` + White)), 
               (3*2*6) - 1)
  expect_equal(nrow(auc(benchmarks, math ~ season + frl + ethnicity,
                        ~Fall + `Non-FRL`)), 
               ((3*2*6)*6) - 6)
               
})
