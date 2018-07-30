## ---- echo=FALSE---------------------------------------------------------
library(ffmetadata)

## ------------------------------------------------------------------------
select_type <- select_metadata(variable_name = "ce3datey", fields = "data_source")
print(select_type)

## ------------------------------------------------------------------------
select <- select_metadata(variable_name = "ce3datey", fields = c("data_source", "data_type"))

## ------------------------------------------------------------------------
select_full <- select_metadata(variable_name = "ce3datey")

## ------------------------------------------------------------------------
select_return_list <- select_metadata(variable_name = "ce3datey", returnDataFrame = FALSE)

## ---- results = "asis"---------------------------------------------------
search <- search_metadata(wave = 3)

## ------------------------------------------------------------------------
search <- search_metadata(wave = 3, data_type = "oc")

