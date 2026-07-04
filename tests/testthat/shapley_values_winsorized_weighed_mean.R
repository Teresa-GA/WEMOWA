test_that("shapley_values_winsorized_weighed_mean returns a vector of length n", {
            expect_equal(length(shapley_values_winsorized_weighed_mean(10, r=2)), 10)
          })
