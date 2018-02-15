#' Select Metadata
#'
#' Modified from PHP Web API definition: selectMetadata
#' returns a specific field of a given variable, or
#' the entire variable if no field is specified
#'
#' @param varName name of variable
#' @param fieldName specific field of variable
#'
#' @return returns string with value if field is specified,
#' returns list of fields for variable if field is unspecified
#' @export
#'
#' @examples
#' select1 <- select_metadata(variable_name = "ce3agefc")
#' select2 <- select_metadata(variable_name = "ce3agefc", field_name = "type")
select_metadata <- function(variable_name = NULL, field_name = NULL) {
  # validate field_name
  if (!is.null(field_name)) {
    if (!(field_name %in% .valid_names)) {
      stop(field_name, " is not a valid field name")
    }
  }
  params <- list(new_name=variable_name, field_name=field_name)
  call_api("selectMetadata.php", params)
}

#' Search Metadata
#'
#' Modified from PHP Web API: searchMetadata returns a
#' data frame of variables given a query string and a search category,
#' or the entire set of metadata as a data frame if query is empty
#' @param query a substring searched for in the given search category
#' @param field_name the search category
#'
#' @return returns data frame where each observation is a variable
#' @export
#'
#' @examples
#' search_test <- search_metadata(query = "policing", field_name = "topic1")
search_metadata <- function(query = NULL, field_name = NULL) {
  # validate field_name
  if (!(field_name %in% .valid_names)) {
    stop(field_name, " is not a valid field name")
  }
  params <- list(query=query, field_name=field_name)
  call_api("searchMetadata.php", params)
}

#' Filter Metadata
#'
#' Modified from PHP Web API definition: filterMetadata returns a data frame
#' of variables given a set of filter values for variable categories,
#' or the entire set of metadata as a data frame if no filters are provided
#' @param ...
#'
#' @return returns data frame where each observation is a
#' variable
#' @export
#'
#' @examples
#' filter_test <- filter_metadata(wave = 3, source = "constructed", type = "bin")
filter_metadata <- function(filter_list=list(), ...) {
  params <- c(filter_list, list(...))

  # validate parameters, ignoring the first entry (the function name)
  for (i in 1:length(params)) {
    if (!(names(params)[i] %in% .valid_names)) {
      stop(names(params)[i], " is not a valid field name")
    }
  }

  call_api(endpoint="filterMetadata.php", params)
}

