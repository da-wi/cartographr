test_that("function returns current output size when size is NULL", {
  cartographr_env$output_size <- c(210, 297) # A4 in mm
  expect_equal(set_output_size(), c(210, 297))
})

test_that("function sets output size to standard paper dimensions", {
  expect_null(set_output_size("A3"))
  expect_equal(set_output_size(), c(297,420))
})

test_that("function throws error for unrecognized orientation", {
  expect_error(set_output_size("A4", "diagonal"), "Orientation not recognized")
})

test_that("function throws error for unrecognized paper size", {
  expect_error(set_output_size("B4"), "Format not recognized")
})

test_that("function throws error for incorrect custom dimensions length", {
  expect_error(set_output_size(c(300)), "Lenght of vector `format` must be exactly 2")
})

test_that("function updates orientation based on custom dimensions", {
  set_output_size(c(300, 200))
  expect_equal(cartographr_env$orientation, "landscape")
  set_output_size(c(200, 300))
  expect_equal(cartographr_env$orientation, "portrait")
})

test_that("function calculates scale factor correctly", {
  set_output_size("A0")
  ref_dims <- c(841, 1189)
  ref_scale <- sqrt(ref_dims[1]^2 + ref_dims[2]^2)
  expected_scale <- sqrt(cartographr_env$output_size[1]^2 +
                           cartographr_env$output_size[2]^2) / ref_scale
  expect_equal(cartographr_env$scale_factor, expected_scale)
})
