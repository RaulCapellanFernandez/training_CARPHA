
test_that("plot_seasonality handles missing or invalid data", {
  data <- data.frame(time = as.Date('2025-01-01'),
                     cases = 10, pop = 1000, area = 'A')
  
  # Test missing 'data'
  expect_error(plot_seasonality(), "Error: Missing required argument 'data'")
  
  # Test invalid 'data' type (e.g., not a data.frame)
  expect_error(plot_seasonality(data = list(time = as.Date('2025-01-01'), cases = 10)),
               "'data' should be a data.frame")
  
  # Test missing 'time'
  expect_error(plot_seasonality(data = data), "Error: Missing required argument 'var'")
  
  # Test missing 'time'
  expect_error(plot_seasonality(data = data, var = "cases", 
                                time = "time", type = "invalid"), 
               "type must be either 'cov', 'counts' or 'inc'")
})


test_that("Apropriate panels per facets for area", {
  data <- data.frame(time = as.Date(c('2025-01-01', '2025-01-02',
                                      '2025-02-01', '2025-02-02', 
                                      '2025-01-01', '2025-01-02',
                                      '2025-02-01', '2025-02-02')), 
                     var = c(1,2,3,1,1,2,3,1), 
                     area = rep(letters[1:2], each = 4))
  
  plot_result <- plot_seasonality(data = data,
                                  var = "var",
                                  time = "time",
                                  area = "area")
  
  # Extract the plot's build components using ggplot_build
  plot_build <- ggplot2::ggplot_build(plot_result)
  
  # Test if facets are used correctly for multiple areas
  facet_info <- plot_build$layout$facet$params$facets$area
  # 
  expect_true(!is.null(facet_info), "Area facets should be present in the plot")
  
})


test_that("plot_seasonality aggregates data correctly", {
  # Simulate monthly data
  data <- data.frame(time = as.Date(c('2025-01-01', '2025-01-02',
                                      '2025-02-01', '2025-02-02', 
                                      '2025-01-01', '2025-01-02',
                                      '2025-02-01', '2025-02-02')), 
                     var = c(1,2,3,1,1,2,3,1), 
                     area = rep(letters[1:2], each = 4))
  
  # Test aggregation by month
  plot_result <- plot_seasonality(data = data, var = "var", time = "time",
                                  area = "area", aggregate_time = "month")
  
  expect_equal(length(unique(plot_result$data$time)), 2)
  
})


test_that("log transformation is applied correctly when type == counts", {
  # Simulate data
  data <- data.frame(time = as.Date(c('2025-01-01', '2025-01-02',
                                      '2025-02-01', '2025-02-02', 
                                      '2025-01-01', '2025-01-02',
                                      '2025-02-01', '2025-02-02')), 
                     cases = c(1,2,3,1,1,2,3,1), 
                     pop= rep(1000, 8),
                     area = rep(letters[1:2], each = 4))
  
  data.agg <- aggregate_cases(data = data,
                          cases = "cases",
                          pop = "pop",
                          time = "time",
                          area = "area",
                          aggregate_time = "month",
                          pt = 100000)
  
  expected_log  <- log(data.agg$cases + 1)
  
  # Test log transformation for counts
  plot_result <- plot_seasonality(data = data, var = "cases", time = "time",
                                  area = "area", pop= "pop", type="counts",
                                  log = TRUE)
  
  # Check that the y-axis values are log-transformed
  y_values <- plot_result$data$inc
  expect_equal(y_values, expected_log) # Expecting log-transformed values to be < 0 for counts > 1
})

test_that("log transformation is applied correctly when type == incidence", {
  # Simulate data
  data <- data.frame(time = as.Date(c('2025-01-01', '2025-01-02',
                                      '2025-02-01', '2025-02-02', 
                                      '2025-01-01', '2025-01-02',
                                      '2025-02-01', '2025-02-02')), 
                     cases = c(1,2,3,1,1,2,3,1), 
                     pop= rep(1000, 8),
                     area = rep(letters[1:2], each = 4))
  
  data.agg <- aggregate_cases(data = data,
                              cases = "cases",
                              pop = "pop",
                              time = "time",
                              area = "area",
                              aggregate_time = "month",
                              pt = 100000)
  
  expected_log  <- log(data.agg$cases + 1) - log(data.agg$pop / 100000)
  
  # Test log transformation for inc
  plot_result <- plot_seasonality(data = data, var = "cases", time = "time",
                                  area = "area", pop= "pop", type="inc",
                                  log = TRUE)
  
  # Check that the y-axis values are log-transformed
  y_values <- plot_result$data$inc
  expect_equal(y_values, expected_log) # Expecting log-transformed values to be < 0 for counts > 1
})


test_that("Error when population is missing for incidence", {
  data <- data.frame(time = as.Date('2025-01-01'),
                     cases = 10, pop = NA, area = 'A')
  
  # Test for missing population with incidence type
  expect_error(plot_seasonality(data = data, var = "cases", time = "time", type = "inc"),
               "'pop' required if type = 'inc'")
})


test_that("Person-time subtitle is correctly displayed for incidence", {
  data <- data.frame(time = as.Date(c('2025-01-01', '2025-01-02',
                                      '2025-02-01', '2025-02-02', 
                                      '2025-01-01', '2025-01-02',
                                      '2025-02-01', '2025-02-02')), 
                     cases = c(1,2,3,1,1,2,3,1), 
                     pop= rep(1000, 8),
                     area = rep(letters[1:2], each = 4))
  
  plot_result <- plot_seasonality(data = data, var = "cases", time = "time",
                                  area = "area", pop= "pop", type="inc",
                                  pt = 1000)
  plot_build <- ggplot2::ggplot_build(plot_result)
  
  # Check if subtitle contains person-time
  subtitle <- plot_build$plot$labels$subtitle
  expect_true(grepl("1,000 person - month", subtitle))
})


