osm_test_object <- list(bbox = c(10, 20, 30, 40))

test_that("adjust_viewport returns correct ggplot2 coord_sf object", {
  result <- adjust_viewport(osm_test_object)
  expect_s3_class(result, "CoordSf")
  expect_equal(round(result$limits$x,2), c(10.91, 29.09))
  expect_equal(round(result$limits$y,2), c(20.91, 39.09))
})

test_that("add_attribution returns correct ggplot2 labs object", {
  cartographr_env$attribution <- TRUE
  result <- add_attribution()
  expect_true(all(c("ggplot2::labels", "gg", "S7_object") %in% class(result)))
  expect_equal(result$caption, "CARTOGRAPHR   |   OPENSTREETMAP")
})

test_that("add_attribution returns NULL when attribution is disabled", {
  cartographr_env$attribution <- FALSE
  result <- add_attribution()
  expect_null(result)
  cartographr_env$attribution <- TRUE
})

test_that("get_circle returns a circle sf object", {
  result <- get_circle(50, 10, 1000, 1000)
  expect_s3_class(result, "sf")
  expect_true(sf::st_is(result, "POLYGON"))
})

test_that("get_hexagon returns a hexagon sf object", {
  result <- get_hexagon(50, 10, 1000, 1000)
  expect_s3_class(result, "sfc_POLYGON")
  expect_true(sf::st_is(result, "POLYGON"))
})

test_that("get_border returns correct map borders", {
  result <- get_border(50, 10, 1000, 1000)
  expect_type(result, "double")
  expect_length(result, 4)
  expect_true(all(result[1:2] <= c(10, 50)) && all(result[3:4] >= c(10, 50)))
})
