test_that("set_output_size returns current output size when size is NULL", {
  cartographr_env$output_size <- c(210, 297) # A4 in mm
  expect_equal(set_output_size(), c(210, 297))
})

test_that("set_output_size sets output size to standard paper dimensions", {
  expect_null(set_output_size("A3"))
  expect_equal(set_output_size(), c(297,420))
})

test_that("set_output_size throws error for unrecognized orientation", {
  expect_error(set_output_size("A4", "diagonal"), "Orientation not recognized")
})

test_that("set_output_size throws error for unrecognized paper size", {
  expect_error(set_output_size("B4"), "Format not recognized")
})

test_that("set_output_size throws error for incorrect custom dimensions length", {
  expect_error(set_output_size(c(300)), "Lenght of vector `format` must be exactly 2")
})

test_that("reversing output size for landscape orientation", {
  set_output_size("A4")
  result <- set_output_size()

  set_output_size("A4", orientation = "landscape")
  result_landscape <- set_output_size()

  expect_true(all(result == rev(result_landscape)))
})

test_that("handling grid unit size", {
  set_output_size(c(300, 200))
  set_output_size(grid::unit(c(100, 200), "mm"))
  expect_equal(cartographr_env$output_size, c(100, 200))
})

test_that("set_output_size updates orientation based on custom dimensions", {
  set_output_size(c(300, 200))
  expect_equal(cartographr_env$orientation, "landscape")
  set_output_size(c(200, 300))
  expect_equal(cartographr_env$orientation, "portrait")
})

test_that("set_output_size calculates scale factor correctly", {
  set_output_size("A0")
  ref_dims <- c(841, 1189)
  ref_scale <- sqrt(ref_dims[1]^2 + ref_dims[2]^2)
  expected_scale <- sqrt(cartographr_env$output_size[1]^2 +
                           cartographr_env$output_size[2]^2) / ref_scale
  expect_equal(cartographr_env$scale_factor, expected_scale)
})

test_that("set_attribution sets and retrieves attribution correctly", {
  set_attribution(TRUE)
  expect_true(cartographr_env$attribution)

  set_attribution(FALSE)
  expect_false(cartographr_env$attribution)

  result <- set_attribution(NULL)
  expect_false(result)
})

test_that("set_attribution throws an error for non-logical argument", {
  expect_error(set_attribution("invalid_argument"))
})

test_that("print_config prints correct configuration details", {
  cartographr_env$variable1 <- 42
  cartographr_env$variable2 <- "hello"
  cartographr_env$variable3 <- c(1, 2)

  result <- cli::cli_fmt(print_config())

  expect_true(any(grepl(": 42", result)))
  expect_true(any(grepl(": hello", result)))
  expect_true(any(grepl(": 1 2", result)))
})
