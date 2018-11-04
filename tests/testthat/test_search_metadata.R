library(testthat)

context("Search Metadata")

test_that("search_metadata throws an error when not enough comparison operators specified", {
  expect_that(search_metadata(wave = "Year 1", respondent = "Mother", n_cities_asked = "20",
                              operation = c("like", "eq")), throws_error())
})

test_that("search_metadata returns the correct number of variables for wave 1 search", {
  wave_1_search <- search_metadata(wave = "Year 1")
  expect_that(length(wave_1_search), equals(1886))
})

test_that("search_metadata returns the correct number of variables for AND search with wave 1 and respondent mother", {
  and_search <- search_metadata(wave = "Year 1", respondent = "Mother")
  expect_that(length(and_search), equals(907))
})

test_that("search_metadata returns the correct number of variables for f1% wild card name search", {
  wild_card_search <- search_metadata(name = "f1%", operation = "like")
  expect_that(length(wild_card_search), equals(407))
})

test_that("search_metadata returns the correct number of variables when multiple operators used", {
  multiple_operators <- search_metadata(wave = "Year 1", name = "f%", operation = c("eq", "like"))
  expect_that(length(multiple_operators), equals(858))
})

test_that("search_metadata returns the correct number of variables when using in operator", {
  in_operator <- search_metadata(respondent = list("Interviewer", "Child Care Provider"), operation = "in")
  expect_that(length(in_operator), equals(1479))
})

test_that("search_metadata returns the correct number of variables when using is_null operator", {
  is_null_operator <- search_metadata(qText = "is_null")
  expect_that(length(is_null_operator), equals(4473))
})

test_that("search_metadata returns the correct number of variables when using is_not_null operator", {
  is_not_null_operator <- search_metadata(measures = "is_not_null")
  expect_that(length(is_not_null_operator), equals(1830))
})
