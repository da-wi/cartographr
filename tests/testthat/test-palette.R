# Test for valid palettes
valid_palettes <- c("alphabet", "arctic","autumn", "bw","dotted",
                    "desert","evening", "gray", "iberia", "imhof","lines","midnight",
                    "mikimal","minimal","metropolitan","swiss","tropical")

for (palette in valid_palettes) {
  test_that(paste("Palette", palette, "returns a list"), {
    result <- get_palette(palette)
    expect_type(result, "list")
    expect_named(result)
  })
}

# Test for invalid palette
test_that("Invalid palette throws an error", {
  expect_error(get_palette("invalid_palette"))
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
