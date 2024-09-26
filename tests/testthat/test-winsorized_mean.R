test_that("winsorized mean returns a numerical value", {
  expect_type(winsorized_mean(1:10), "double")
  })

test_that("winsorised mean returns NA if input constains
          NA  and na.rm is not written", {
     expect_equal(winsorized_mean(c(1:10, NA), r=2), NA_real_)
          })
