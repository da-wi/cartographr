test_that("theme_infomap returns a ggplot2 theme object and handles invalid fonts", {
  expect_s3_class(theme_infomap(), "theme")
  expect_s3_class(theme_infomap(font = "Anton"), "theme")
  expect_s3_class(theme_infomap_anton(), "theme")
  expect_s3_class(theme_infomap_poppins(), "theme")
  expect_s3_class(theme_infomap_barlow(), "theme")
})

test_that("theme_poster returns a ggplot2 theme object and handles invalid fonts", {
  expect_s3_class(theme_poster(), "theme")
  expect_s3_class(theme_poster(font = "Anton"), "theme")
  expect_s3_class(theme_poster_anton(), "theme")
  expect_s3_class(theme_poster_poppins(), "theme")
})
