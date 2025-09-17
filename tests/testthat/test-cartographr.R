# Test that preprocess_map returns a list
test_that("preprocess_map returns a list", {
  data("osm")
  expect_type(preprocess_map(osm), "list")
})

# Test that preprocess_map returns an object with expected names
test_that("preprocess_map returns an object with expected names", {
  data("osm")
  result <- preprocess_map(osm)
  expected_names <- c('x','sf_street', 'sf_building', 'sf_water', 'sf_sea', 'sf_green', 'sf_beach',
                      'sf_parking', 'sf_railway', 'bbox', 'y_distance', 'x_distance',
                      'lat', 'lon', 'sf_water_combined','sf_buildings_combined','sf_green_combined',
                      'crop','crop_extent', 'preprocessed')
  expect_named(result, expected_names, ignore.case = TRUE, ignore.order = TRUE)
})

# Test that water bodies and buildings are correctly preprocessed
test_that("water bodies and buildings are correctly preprocessed", {
  data("osm")
  result <- preprocess_map(osm)
  expect_true(!is.null(result$sf_water_combined))
  expect_true(!is.null(result$sf_buildings_combined))
})

# Test that preprocessing information is added
test_that("preprocessing information is added", {
  data("osm")
  result <- preprocess_map(osm)
  expect_true(result$preprocessed)
})

# Test that the function handles invalid inputs gracefully
test_that("preprocess_map handles invalid inputs gracefully", {
  data("osm")
  expect_error(preprocess_map(NULL))
  expect_error(preprocess_map(list()))
  expect_error(preprocess_map(data.frame()))
})

# Test that the function does not modify the input object
test_that("preprocess_map does not modify the input object", {
  data("osm")
  original_osm <- osm
  result <- preprocess_map(osm)
  expect_equal(original_osm, osm)
})


test_that("crop function sets crop variable correctly", {
  data("osm")
  data("soho_boundary")

  result_rect <- crop(osm, boundary = "rect")
  expect_equal(result_rect$crop, "rect")
  expect_true(all(c(-74.02,  40.71, -73.99,  40.73 ) == round(result_rect$bbox,2)))

  result_circle <- crop(osm, boundary = "circle")
  expect_equal(result_circle$crop, "circle")

  result_hex <- crop(osm, boundary = "hex")
  expect_equal(result_hex$crop, "hex")

  result_custom_sf <- crop(osm, boundary = soho_boundary)
  expect_equal(result_custom_sf$crop, "sf")
})


test_that("crop function throws error for invalid boundary input", {
  data("osm")
  expect_error(crop(osm, boundary = "invalid_input"))
})

test_that("crop function throws error for NULL input", {
  data("osm")
  expect_error(crop(osm, boundary = NULL))
})

test_that("crop function throws error for invalid osm input", {
  data("osm")
  expect_error(crop(NULL, boundary = "rect"))
})

test_that("plot_map returns a plot object", {
  data("osm")
  result <- plot_map(osm)
  expect_true(ggplot2::is_ggplot(result))
})

test_that("plot_map stops with an error for incorrect input", {
  data("osm")
  expect_error(plot_map(123))
  expect_error(plot_map(NULL))
})

test_that("get_osmdata stops with an error when no arguments are provided", {
  data("osm")
  expect_error(get_osmdata())
})


#test_that("get_osmdata calculates x_distance and y_distance correctly", {
#  result <- get_osmdata(lat = 40.0, lon = -74.0, y_distance = 100, quiet = T)
#  expect_equal(result$x_distance, result$y_distance * result$aspect_ratio)
#  expect_equal(result$y_distance, 100)
#})

test_that("x/y distance calculations are correct on projected data", {
  pts <- sf::st_as_sf(
    data.frame(x = c(0, 1), y = c(0, 0)),
    coords = c("x", "y"), crs = 4326
  )
  pts_3857 <- sf::st_transform(pts, 3857)
  coords <- sf::st_coordinates(pts_3857)

  dx <- abs(diff(coords[, 1]))
  dy <- abs(diff(coords[, 2]))

  expect_equal(dx, 111319.490793, tolerance = 1e-3)
  expect_lt(dy, 1e-6)
})

test_that("get_osmdata retrieves data without errors", {
  skip_if_no_overpass()
  result <- get_osmdata(lat = 44.1348, lon=9.683,  x_distance = 100, y_distance = 50, quiet = T)
  expect_true(inherits(result$x, "osmdata"))
})

test_that("save_map saves a map object to a file", {
  data("osm")
  set_output_size("A4")
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file))

  p <- osm |>
    plot_map(palette="swiss")

  expect_false(file.exists(temp_file))
  save_map(p, temp_file)
  expect_true(file.exists(temp_file))
})


