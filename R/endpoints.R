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
#'   \item{name: the new, standardized variable name}
#'   \item{old_name: the original variable name from older files}
#'   \item{data_type: variable type. Options are binary (bin), continuous (cont),
#'    unordered categorical (uc), ordered categorical (oc), and string (string)}
#'   \item{warning: flags for issues}
#'   \item{group: group number (matches the same questions across surveys)}
#'   \item{q_group_N: number of variables that are in the group}
#'   \item{topic1: indicates specific content topic (e.g.
#'    medication or housing status), there is no hierarchy between topic1 and
#'    topic2}
#'   \item{topic2: indicates specific content topic (e.g.
#'    medication or housing status), there is no hierarchy between topic1 and
#'    topic2}
#'   \item{data_source: source of variable}
#'   \item{respondent: respondent, either the person interviewed or
#'   the place an interview took place}
#'   \item{wave: the wave of data collection associated with the variable}
#'   \item{scope: the reach of a variable's sample population (grouped by
#'    city where each person was originally interviewed). The max scope is
#'    20 cities.}
#'   \item{section: section of survey where question is asked}
#'   \item{leaf: the rest of the variable name}
#'   \item{q_group_list: a string that lists all of the new variable names
#'   for all the variables in the group}
#'   \item{value1-value78: all of the potential responses for a given variable}
#'   \item{label1-label68: all of the labels for the potential responses for
#'   a given variable}
#' }
#'
#' @export
#'
#' @examples
#' select1 <- select_metadata(variable_name = "ce3agefc")
#' select2 <- select_metadata(variable_name = "ce3agefc", field_name = "type")
select_metadata <- function(variable_name = NULL, field_name = NULL) {
  # parse parameters
  params <- list(variable_name=variable_name, field_name=field_name)
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
#' @return returns list of names of all variables that contain a query string within
#' a given field
#'
#' @details List of valid field names:
#' \itemize{
#'   \item{name: the new, standardized variable name}
#'   \item{old_name: the original variable name from older files}
#'   \item{data_type: variable type. Options are binary (bin), continuous (cont),
#'    unordered categorical (uc), ordered categorical (oc), and string (string)}
#'   \item{warning: flags for issues}
#'   \item{group: group number (matches the same questions across surveys)}
#'   \item{q_group_N: number of variables that are in the group}
#'   \item{topic1: indicates specific content topic (e.g.
#'    medication or housing status), there is no hierarchy between topic1 and
#'    topic2}
#'   \item{topic2: indicates specific content topic (e.g.
#'    medication or housing status), there is no hierarchy between topic1 and
#'    topic2}
#'   \item{data_source: source of variable}
#'   \item{respondent: respondent, either the person interviewed or
#'   the place an interview took place}
#'   \item{wave: the wave of data collection associated with the variable}
#'   \item{scope: the reach of a variable's sample population (grouped by
#'    city where each person was originally interviewed). The max scope is
#'    20 cities.}
#'   \item{section: section of survey where question is asked}
#'   \item{leaf: the rest of the variable name}
#'   \item{q_group_list: a string that lists all of the new variable names
#'   for all the variables in the group}
#'   \item{value1-value78: all of the potential responses for a given variable}
#'   \item{label1-label68: all of the labels for the potential responses for
#'   a given variable}
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
  params <- list(query=query, field_name=field_name)
  searched <- call_api("search", params)
  # error handling
  if (stringr::str_detect(names(test)[1], "error")) {
    stop("Error Code: ", test[["error code"]],
         " Description: ", test[["error_description"]])
  }
  return(searched)
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
#' @return returns data frame of variable metadata based on a set of given
#' filter values, or the entire set of metadata if no filters are provided
#' @export
#'
#' @details List of valid field names:
#' \itemize{
#'   \item{name: the new, standardized variable name}
#'   \item{old_name: the original variable name from older files}
#'   \item{data_type: variable type. Options are binary (bin), continuous (cont),
#'    unordered categorical (uc), ordered categorical (oc), and string (string)}
#'   \item{warning: flags for issues}
#'   \item{group: group number (matches the same questions across surveys)}
#'   \item{q_group_N: number of variables that are in the group}
#'   \item{topic1: indicates specific content topic (e.g.
#'    medication or housing status), there is no hierarchy between topic1 and
#'    topic2}
#'   \item{topic2: indicates specific content topic (e.g.
#'    medication or housing status), there is no hierarchy between topic1 and
#'    topic2}
#'   \item{data_source: source of variable}
#'   \item{respondent: respondent, either the person interviewed or
#'   the place an interview took place}
#'   \item{wave: the wave of data collection associated with the variable}
#'   \item{scope: the reach of a variable's sample population (grouped by
#'    city where each person was originally interviewed). The max scope is
#'    20 cities.}
#'   \item{section: section of survey where question is asked}
#'   \item{leaf: the rest of the variable name}
#'   \item{q_group_list: a string that lists all of the new variable names
#'   for all the variables in the group}
#'   \item{value1-value78: all of the potential responses for a given variable}
#'   \item{label1-label68: all of the labels for the potential responses for
#'   a given variable}
#' }
#'
#' @examples
#' filter_test <- filter_metadata(wave = 3, data_source = "constructed", data_type = "bin")
filter_metadata <- function(filter_list=list(), ...) {
  # parse and pass parameters to api call
  params <- c(filter_list, list(...))
  filtered <- call_api(endpoint="filter", params)
  # error handling
  if (stringr::str_detect(names(test)[1], "error")) {
    stop("Error Code: ", test[["error code"]],
         " Description: ", test[["error_description"]])
  }
  return(filtered)
}

