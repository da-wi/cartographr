data("osm")

# Test that preprocess_map returns a list
test_that("preprocess_map returns a list", {
  expect_type(preprocess_map(osm), "list")
})

# Test that preprocess_map returns an object with expected names
test_that("preprocess_map returns an object with expected names", {
  result <- preprocess_map(osm)
  expected_names <- c('x','x.street', 'x.building', 'x.water', 'x.sea', 'x.green', 'x.beach',
                      'x.parking', 'x.railway', 'bbox', 'y_distance', 'x_distance',
                      'lat', 'lon', 'water.dis','buildings.dis','green.dis', 'crop', 'preprocessed')
  expect_named(result, expected_names, ignore.case = T, ignore.order = T)
})

# Test that water bodies and buildings are correctly preprocessed
test_that("water bodies and buildings are correctly preprocessed", {
  result <- preprocess_map(osm)
  expect_true(!is.null(result$water))
  expect_true(!is.null(result$buildings.dis))
})

# Test that preprocessing information is added
test_that("preprocessing information is added", {
  result <- preprocess_map(osm)
  expect_true(result$preprocessed)
})

# Test that the function handles invalid inputs gracefully
test_that("preprocess_map handles invalid inputs gracefully", {
  expect_error(preprocess_map(NULL))
  expect_error(preprocess_map(list()))
  expect_error(preprocess_map(data.frame()))
})

# Test that the function does not modify the input object
test_that("preprocess_map does not modify the input object", {
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
  expect_error(crop(osm, boundary = "invalid_input"))
})

test_that("crop function throws error for NULL input", {
  expect_error(crop(osm, boundary = NULL))
})

test_that("crop function throws error for invalid osm input", {
  expect_error(crop(NULL, boundary = "rect"))
})

test_that("plot_map works with a named argument 'osm'", {
  expect_silent(plot_map(osm))
})

test_that("plot_map returns a plot object", {
  result <- plot_map(osm)
  expect_true(ggplot2::is.ggplot(result))
})

test_that("plot_map stops with an error for incorrect input", {
  expect_error(plot_map(123))
  expect_error(plot_map(NULL))
})

test_that("get_osmdata stops with an error when no arguments are provided", {
  expect_error(get_osmdata())
})


test_that("get_osmdata calculates x_distance and y_distance correctly", {
  result <- get_osmdata(lat = 40.0, lon = -74.0, y_distance = 100, quiet = T)
  expect_equal(result$x_distance, result$y_distance * result$aspect_ratio)
  expect_equal(result$y_distance, 100)
})

test_that("get_osmdata retrieves data without errors", {
  result <- get_osmdata(lat = 44.1348, lon=9.683,  x_distance = 100, y_distance = 50, quiet = T)
  expect_silent(result)
  expect_equal(result$aspect_ratio, 2)
  expect_equal(result$x_distance, 100)
  expect_equal(result$y_distance, 50)
})


test_that("save_map saves a map object to a file", {
  set_output_size("A4")
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file))

  p <- osm |>
    plot_map(palette="swiss")

  expect_false(file.exists(temp_file))
  save_map(p, temp_file)
  expect_true(file.exists(temp_file))
})


