## ---- echo=FALSE---------------------------------------------------------
library(pander)
library(ffmetadata)

## ------------------------------------------------------------------------
select_type <- select_metadata(variable_name = "ce3datey", fields = "data_source")

## ---- echo=FALSE---------------------------------------------------------
pander(select_type)

## ---- results='asis'-----------------------------------------------------
select_multiple_fields <- select_metadata(variable_name = "ce3datey", fields = c("data_source", "data_type"))

## ---- echo=FALSE---------------------------------------------------------
pander(select_multiple_fields)

## ---- results='asis'-----------------------------------------------------
select_full <- select_metadata(variable_name = "ce3datey")

## ---- echo=FALSE---------------------------------------------------------
  pander(select_full)

## ------------------------------------------------------------------------
select_return_list <- select_metadata(variable_name = "ce3datey", returnDataFrame = FALSE)

## ---- echo=FALSE---------------------------------------------------------
pander(select_return_list)

## ---- results = "asis"---------------------------------------------------
search <- search_metadata(wave = 3)

## ------------------------------------------------------------------------
search <- search_metadata(wave = 3, data_type = "oc")

