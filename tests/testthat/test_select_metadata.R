library(testthat)

context("Select Metadata")

test_that("select_metadata throws an error message when no variable name specified", {
  expect_that(select_metadata(fields = "data_type"), throws_error())
})

test_that("select_metadata returns a data frame when default parameters used", {
  default_call <- select_metadata(variable_name = "ce3agefc")
  expect_that(default_call, is_a("matrix"))
})

test_that("select_metadata returns a list when returnDataFrame set to False", {
  list_return <- select_metadata(variable_name = "ce3agefc", returnDataFrame = FALSE)
  expect_that(list_return, is_a("list"))
})

test_that("select_metadata returns a character type when one field is specified", {
  one_field <- select_metadata(variable_name = "ce3agefc", fields = "data_type")
  expect_that(one_field, is_a("character"))
})


test_that("select_metadata returns a data frame when multiple fields specified", {
  multiple_fields <- select_metadata(variable_name = "ce3agefc",
                                     fields = c("data_type", "data_source"))
  expect_that(multiple_fields, is_a("matrix"))
})

test_that("select_metadata returns endpoint error when invalid variable name used", {
  expect_that(select_metadata("m1a2"), throws_error())
})

