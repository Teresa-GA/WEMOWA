test_that("shaply_wm returns a vector of length n", {
            expect_equal(length(shapley_wm(10, r=2)), 10)
          })
