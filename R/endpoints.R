#' selectMetadata
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
#' selectNoFieldName <- selectMetadata(new_name = "ce3agefc")
#' selectWithFieldName <- selectMetadata(new_name = "ce3agefc", field_name = "type")
selectMetadata <- function(new_name = NULL, field_name = NULL) {
  # validate field_name
  if (!is.null(field_name)) {
    if (!(field_name %in% validNames)) {
      stop(field_name, " is not a valid field name")
    }
  }
  params <- params_from_call(match.call())
  do.call("call_api", params)
}

#' searchMetadata
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
#' search_test <- searchMetadata(query = "policing", field_name = "topic1")
searchMetadata <- function(query = NULL, field_name = NULL) {
  # validate field_name
  if (!(field_name %in% validNames)) {
    stop(field_name, " is not a valid field name")
  }
  params <- params_from_call(match.call())
  do.call("call_api", params)
}

#' filterMetadata
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
#' filter_test <- filterMetadata(wave = 3, source = "constructed", type = "bin")
filterMetadata <- function(...) {
  params <- params_from_call(match.call())
  # validate parameters, ignoring the first entry (the function name)
  for (i in 2:length(params)) {
    if (!(names(params)[i] %in% validNames)) {
      stop(names(params)[i], " is not a valid field name")
    }
  }
  do.call("call_api", params)
}

