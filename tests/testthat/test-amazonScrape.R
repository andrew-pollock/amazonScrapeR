
# Get test data
uk_test_data   <- amazonScrape("1800560796", country = "UK")
us_test_data   <- amazonScrape("1800560796", country = "US")
both_test_data <- amazonScrape("1800560796", country = "Both")


test_that("6 columns are returned for UK", {
  expect_length(uk_test_data, 6)
})


test_that("6 columns are returned for US", {
  expect_length(us_test_data, 6)
})


test_that("no columns are completely NULL for UK", {
  expect_lt(sum(is.na(uk_test_data$Reviewer)), nrow(uk_test_data))

  expect_lt(sum(is.na(uk_test_data$Date)), nrow(uk_test_data))

  expect_lt(sum(is.na(uk_test_data$Title)), nrow(uk_test_data))

  expect_lt(sum(is.na(uk_test_data$Review_Text)), nrow(uk_test_data))

  expect_lt(sum(is.na(uk_test_data$Review_Score)), nrow(uk_test_data))

  expect_lt(sum(is.na(uk_test_data$Country)), nrow(uk_test_data))
})


test_that("no columns are completely NULL for US", {
  expect_lt(sum(is.na(us_test_data$Reviewer)), nrow(us_test_data))

  expect_lt(sum(is.na(us_test_data$Date)), nrow(us_test_data))

  expect_lt(sum(is.na(us_test_data$Title)), nrow(us_test_data))

  expect_lt(sum(is.na(us_test_data$Review_Text)), nrow(us_test_data))

  expect_lt(sum(is.na(us_test_data$Review_Score)), nrow(us_test_data))

  expect_lt(sum(is.na(us_test_data$Country)), nrow(us_test_data))
})


test_that("setting country = 'Both' returns UK and US reviews", {
  expect_gt(nrow(both_test_data[both_test_data$Country == "UK", ]), 0)
  expect_gt(nrow(both_test_data[both_test_data$Country == "US", ]), 0)
})


test_that("scraping a non-existant product ID returns an error", {
  expect_error(amazonScrape("not_a_product_id"))
})
