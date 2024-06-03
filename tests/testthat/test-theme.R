test_that("theme_infomap returns a ggplot2 theme object and handles invalid fonts", {
  expect_s3_class(theme_infomap(), "theme")
  expect_s3_class(theme_infomap(font = "Anton"), "theme")
  expect_s3_class(theme_infomap_anton(), "theme")
})

test_that("theme_infomap returns a ggplot2 theme object and handles invalid fonts", {
  expect_s3_class(theme_poster(), "theme")
  expect_s3_class(theme_poster(font = "Anton"), "theme")
  expect_s3_class(theme_poster_anton(), "theme")
})
