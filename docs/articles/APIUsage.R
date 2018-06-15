## ---- echo=FALSE---------------------------------------------------------
library(ffmetadata)

## ------------------------------------------------------------------------
select_type <- select_metadata(variable_name = "ce3datey", field_name = "source")
print(select_type)

## ---- results = "asis"---------------------------------------------------
select_entire <- select_metadata(variable_name = "ce3datey")
pander::pandoc.table(select_entire, split.tables=Inf)

## ---- results = "asis"---------------------------------------------------
search_topic1 <- search_metadata(query = "mental health", field_name = "topic1")
pander::pandoc.table(search_topic1[1:3,], split.tables=Inf)

## ---- eval=FALSE---------------------------------------------------------
#  # named-list approach
#  params_list <- list(type="bin", source="questionnaire", scope=20)
#  #names(params_list) <- c("type", "source", "scope")
#  # call to filter_metadata
#  filtered <- filter_metadata(params_list)

## ---- eval=FALSE---------------------------------------------------------
#  # explicit parameter approach
#  filtered <- filter_metadata(type = "bin", source = "questionnaire", scope = 20)

## ---- eval=FALSE---------------------------------------------------------
#  # mixed approach
#  params_list <- c("bin", "questionnaire")
#  names(params_list) <- c("type", "source")
#  filtered <- filter_metadata(params_list, scope = 20)

