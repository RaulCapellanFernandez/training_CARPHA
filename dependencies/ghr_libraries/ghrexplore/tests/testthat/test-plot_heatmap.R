# Sample data for testing
data <- data.frame(time = as.Date(c('2025-01-01', '2025-01-02',
                                    '2025-02-01', '2025-02-02', 
                                    '2025-01-01', '2025-01-02',
                                    '2025-02-01', '2025-02-02')),
                   cases = c(10, 15, 20, 25, 30, 35, 40, 45),
                   var = c(1,2,3,1,1,2,3,1), 
                   area = rep(letters[1:2], each = 4), 
                   pop= rep(1000, 8))

test_that("plot_heatmap handles missing arguments correctly", {
  expect_error(plot_heatmap(),
               "Missing required argument 'data'")
  expect_error(plot_heatmap(data),
               "Missing required argument 'var'")
})

test_that("plot_heatmap handles incorrect argument types", {
  expect_error(plot_heatmap(data = list(), time = "time", var = "cases"),
               "'data' should be a data.frame")
  expect_error(plot_heatmap(data, time = "invalid_column", var = "cases"),
               "'time' not found in the data")
  expect_error(plot_heatmap(data, time = "time", var = "var",
                            aggregate_time = "invalid"),
               "'aggregate_time' can be 'week','month',year'")
})

test_that("plot_heatmap correctly processes valid input", {
  expect_silent(plot_heatmap(data, time = "time", var = "cases", type= "counts", 
                             area= "area"))
  expect_silent(plot_heatmap(data, time = "time", var = "var", 
                             area= "area"))
})

test_that("incorrect type", {
  expect_error(plot_seasonality(data = data, var = "cases", 
                                time = "time", type = "invalid"), 
               "type must be either 'cov', 'counts' or 'inc'")
})

test_that("plot_heatmap respects aggregation options", {
  expect_silent(plot_heatmap(data, time = "time", var = "cases",
                             area= "area", pop = "pop", aggregate_time = "week"))
  expect_silent(plot_heatmap(data, time = "time", var = "cases",
                             area= "area", pop = "pop", aggregate_time = "month"))
})

test_that("plot_heatmap produces a ggplot object (cov)", {
  p <- plot_heatmap(data, time = "time", var = "cases",
                    area= "area", pop = "pop")
  expect_true(ggplot2::is.ggplot(p))
})

test_that("plot_heatmap produces a ggplot object (counts)", {
  p <- plot_heatmap(data, time = "time", var = "cases", type="counts",
                    area= "area", pop = "pop")
  expect_true(ggplot2::is.ggplot(p))
})

test_that("plot_heatmap produces a ggplot object (inc)", {
  p <- plot_heatmap(data, time = "time", var = "cases", type="inc",
                    area= "area", pop = "pop")
  expect_true(ggplot2::is.ggplot(p))
})