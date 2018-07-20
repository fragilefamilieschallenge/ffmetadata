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
#' select2 <- select_metadata(variable_name = "ce3agefc", fields = "data_type")
#' select3 <- select_metadata(variable_name = "ce3agefc", fields = c("data_type", "data_source"))
select_metadata <- function(variable_name = NULL, fields = NULL) {
  # error message for if variable_name is missing
  if (missing(variable_name)) {
    stop("select_metadata requires a value for variable_name")
  }
  # format endpoint
  endpoint <- paste("/", variable_name, sep = "")
  url <- paste(.base_url, endpoint, sep = "")
  # null check
  if (!is.null(fields)) {
    if (is.vector(fields)) {
      # format url by appending list items
      url <- paste(url, "?", fields[1], sep = "")
      for (i in 2:length(fields)) {
        url <- paste(url, "&", fields[i], sep = "")
      }
    } else {
      # single field
      url <- paste(url, "?", fields, sep = "")
    }
  }
  result <- call_api(url)
  # format single value as character, otherwise unlist to convert to data frame
  if (length(result) == 1) {
    result <- as.character(result)
  } else {
    # un list into a format that can be converted to a data frame
    result <- unlist(result)
    result <- as.data.frame(result)
    result <- t(result)
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

