data("osm")

# Test that preprocess_map returns a list
test_that("preprocess_map returns a list", {
  expect_type(preprocess_map(osm), "list")
})

# Test that preprocess_map returns an object with expected names
test_that("preprocess_map returns an object with expected names", {
  result <- preprocess_map(osm)
  expected_names <- c('x.street', 'x.building', 'x.water', 'x.sea', 'x.green', 'x.beach',
                      'x.parking', 'x.railway', 'bbox', 'y_distance', 'x_distance',
                      'lat', 'lon', 'water', 'water.dis', 'buildings',
                      'buildings.dis', 'crop', 'preprocessed')
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
test_that("function handles invalid inputs gracefully", {
  expect_error(preprocess_map(NULL))
  expect_error(preprocess_map(list()))
  expect_error(preprocess_map(data.frame()))
})

# Test that the function does not modify the input object
test_that("function does not modify the input object", {
  original_osm <- osm
  result <- preprocess_map(osm)
  expect_equal(original_osm, osm)
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


