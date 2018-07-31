## ---- echo=FALSE---------------------------------------------------------
library(ffmetadata)

## ------------------------------------------------------------------------
select_type <- select_metadata(variable_name = "ce3datey", fields = "data_source")
print(select_type)

## ------------------------------------------------------------------------
select_multiple_fields <- select_metadata(variable_name = "ce3datey", fields = c("data_source", "data_type"))
print(select_multiple_fields)

## ------------------------------------------------------------------------
select_full <- select_metadata(variable_name = "ce3datey")
print(select_full)

## ------------------------------------------------------------------------
select_return_list <- select_metadata(variable_name = "ce3datey", returnDataFrame = FALSE)
print(select_return_list)

## ---- results = "asis"---------------------------------------------------
search <- search_metadata(wave = 3)

## ------------------------------------------------------------------------
search <- search_metadata(wave = 3, data_type = "oc")

