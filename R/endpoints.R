#' Select Metadata
#'
#' select_metadata allows users to retrieve the value of
#' a specific field of a given variable, or
#' the entire variable metadata if no field is specified
#'
#' @param variable_name name of variable
#' @param field_name specific field of variable to be accessed. See
#' details for valid field names
#'
#' @return returns string with value of a given field if field is specified,
#' returns all metadata for variable as a named list if field is unspecified
#'
#' @details List of valid field names:
#' \itemize{
#'     \item{data_source}
#'     \item{data_type}
#'     \item{group_id}
#'     \item{group_subid}
#'     \item{id}
#'     \item{label}
#'     \item{leaf}
#'     \item{name}
#'     \item{old_name}
#'     \item{respondent}
#'     \item{responses}
#'     \item{scope}
#'     \item{section}
#'     \item{topic}
#'     \item{umbrella}
#'     \item{warning}
#'     \item{wave}
#' }
#'
#' @export
#'
#' @examples
#' select1 <- select_metadata(variable_name = "ce3agefc")
#' select2 <- select_metadata(variable_name = "ce3agefc", field_name = "data_type")
select_metadata <- function(variable_name = NULL, field_name = NULL) {
  # parse parameters
  params <- list(varName=variable_name, fieldName=field_name)
  # pass to api call
  result <- call_api("select", params)
  # format single value as character
  if (length(result) == 1) {
    result <- as.character(result)
  }
  return(result)
}

#' Search Metadata
#'
#' search_metadata allows users to retrieve a list of
#' variable names based on whether or not those variables contain a given query
#' within a given field
#'
#' @param query a substring searched for in the given variable field
#' @param field_name the field in which the query is searched for.
#' See details for valid field names.
#'
#' @return returns list of names of all variables that have a substring-matched value within
#' a given field
#'
#' @details List of valid field names:
#' @details List of valid field names:
#' \itemize{
#'     \item{data_source}
#'     \item{data_type}
#'     \item{group_id}
#'     \item{group_subid}
#'     \item{id}
#'     \item{label}
#'     \item{leaf}
#'     \item{name}
#'     \item{old_name}
#'     \item{respondent}
#'     \item{responses}
#'     \item{scope}
#'     \item{section}
#'     \item{topic}
#'     \item{umbrella}
#'     \item{warning}
#'     \item{wave}
#' }
#'
#' @export
#'
#' @examples
#' search_test <- search_metadata(query = "oc", field_name = "data_type")
search_metadata <- function(query, field_name) {
  # error message for if field_name is missing
  if (missing(field_name)) {
    stop("search_metadata requires a value for field_name")
  }
  # error message for if query is missing
  if (missing(query)) {
    stop("search_metadata requires a value for query")
  }
  params <- list(query=query, fieldName=field_name)
  searched <- call_api("search", params)
  return(searched$matches)
}

#' Filter Metadata
#'
#' filter_metadata allows users to retrieve a list of variable
#' names based on a set of filter values for variable categories
#'
#' @param filter_list a named list of variables to filter metadata on. See
#' details for valid field names.
#' @param ... additional value combinations to filter on. See details for
#' valid field names.
#'
#' @return returns list of all variable names that match a set of given
#' filter values
#' @export
#'
#' @details List of valid field names:
#' \itemize{
#'     \item{data_source}
#'     \item{data_type}
#'     \item{group_id}
#'     \item{group_subid}
#'     \item{id}
#'     \item{label}
#'     \item{leaf}
#'     \item{name}
#'     \item{old_name}
#'     \item{respondent}
#'     \item{responses}
#'     \item{scope}
#'     \item{section}
#'     \item{topic}
#'     \item{umbrella}
#'     \item{warning}
#'     \item{wave}
#' }
#'
#' @examples
#' filter_test <- filter_metadata(wave = 3, data_source = "constructed", data_type = "bin")
filter_metadata <- function(filter_list=list(), ...) {
  # parse and pass parameters to api call
  params <- c(filter_list, list(...))
  filtered <- call_api(endpoint="filter", params)
  return(filtered$matches)
}

