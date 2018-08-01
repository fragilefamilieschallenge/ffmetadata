## ---- echo=FALSE---------------------------------------------------------
library(ffmetadata)

## ------------------------------------------------------------------------
select_type <- select_metadata(variable_name = "ce3datey", fields = "data_source")

## ---- echo=FALSE---------------------------------------------------------
select_type

## ---- results='asis'-----------------------------------------------------
select_multiple_fields <- select_metadata(variable_name = "ce3datey", fields = c("data_source", "data_type"))

## ---- echo=FALSE---------------------------------------------------------
select_multiple_fields

## ---- results='asis'-----------------------------------------------------
select_full <- select_metadata(variable_name = "ce3datey")

## ---- echo=FALSE---------------------------------------------------------
select_full

## ------------------------------------------------------------------------
select_return_list <- select_metadata(variable_name = "ce3datey", returnDataFrame = FALSE)

## ---- echo=FALSE---------------------------------------------------------
select_return_list

## ---- results = "asis"---------------------------------------------------
search_1 <- search_metadata(wave = "Year 1")

## ------------------------------------------------------------------------
search_1_and <- search_metadata(wave = "Year 1", respondent = "Mother")

