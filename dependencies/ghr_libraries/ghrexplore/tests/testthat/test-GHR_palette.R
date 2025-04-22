
test_that("GHR_palette returns a function", {
  expect_true(is.function(GHR_palette("IDExtremes")))
})

test_that("GHR_palette rejects invalid palettes", {
  expect_error(GHR_palette("InvalidPalette"),
               "The selected palette is invalid. Please select a GHR, RColorBrewer or viridisLite palette.")
})

test_that("GHR_colors generates correct number of colors", {
  colors <- GHR_colors(3, palette = "Purp")
  expect_equal(length(colors), 3)
})

test_that("GHR_colors handles GHR palettes", {
  expect_silent(GHR_colors(5, palette = "Colorblind"))
})

test_that("GHR_colors with RColorBrewer", {
  expect_silent(GHR_colors(5, palette = "Reds"))
})

test_that("GHR_colors with viridisLite", {
  expect_silent(GHR_colors(5, palette = "viridis"))
})

test_that("GHR_colors with single R color", {
  expect_silent(GHR_colors(5, palette = "skyblue"))
})

test_that("GHR_colors with single hex color", {
  expect_silent(GHR_colors(5, palette = "#03fcca"))
})
