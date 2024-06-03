# Test for predefined palette names
test_that("get_palette returns correct color settings for predefined palettes", {
  expect_type(get_palette("gray"), "list")
  expect_named(get_palette("gray"))
  expect_true(all(names(get_palette("gray")) != ""))
  expect_equal(length(get_palette("gray")), 11)
})

# Test for custom palette as a named list
test_that("get_palette returns correct color settings for custom palettes", {
  custom_palette <- list(
    palette_building = c("#000000", "#FFFFFF"),
    water = "#000000",
    green = "#FFFFFF",
    beach = "#000000",
    parking = "#FFFFFF",
    street = "#000000",
    background = "#FFFFFF",
    railway = "#000000",
    hatched = TRUE
  )
  expect_type(get_palette(custom_palette), "list")
  expect_named(get_palette(custom_palette))
  expect_true(all(names(get_palette(custom_palette)) != ""))
})

# Test for error handling with invalid palette names
test_that("get_palette throws an error for invalid palette names", {
  expect_error(get_palette("invalid_palette_name"))
})
