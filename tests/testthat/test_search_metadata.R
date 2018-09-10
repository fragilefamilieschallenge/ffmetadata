library(testthat)

context("Search Metadata")

test_that("search_metadata throws an error when not enough comparison operators specified", {
  expect_that(search_metadata(wave = "Year 1", respondent = "Mother", scope = "20 Cities",
                              operation = c("like", "eq")), throws_error())
})

test_that("search_metadata returns the correct number of variables for wave 1 search", {
  wave_1_search <- search_metadata(wave = "Year 1")
  expect_that(length(wave_1_search), equals(1886))
})

test_that("search_metadata returns the correct number of variables for AND search with wave 1 and respondent mother", {
  and_search <- search_metadata(wave = "Year 1", respondent = "Mother")
  expect_that(length(and_search), equals(910))
})

test_that("search_metadata returns the correct number of variables for f1% wild card name search", {
  wild_card_search <- search_metadata(name = "f1%", operation = "like")
  expect_that(length(wild_card_search), equals(407))
})
