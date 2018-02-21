#' Select Metadata
#'
#' select_metadata allows users to retrieve the value of
#' a specific field of a given variable, or
#' the entire variable if no field is specified
#'
#' @param variable_name name of variable
#' @param field_name specific field of variable to be accessed. See
#' details for valid field names
#'
#' @return returns string with value of a given field if field is specified,
#' returns a data.frame with variables for every field if field is unspecified
#'
#' @details List of valid field names:
#' \itemize{
#'   \item{new_name: the new, standardized variable name}
#'   \item{old_name: the original variable name from older files}
#'   \item{type: variable type. Options are binary (bin), continuous (cont),
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
#'   \item{source: source of variable}
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
  # validate field_name
  if (!is.null(field_name)) {
    if (!(field_name %in% .valid_names)) {
      stop(field_name, " is not a valid field name")
    }
  }
  params <- list(new_name=variable_name, field_name=field_name)
  result <- call_api("selectMetadata.php", params)

  #format single return values
  if(!is.null(field_name)){
    result <- unlist(result)
    if(is.factor(result))
      result <- as.character(result)
    names(result) <- field_name
  }
  result
}

#' Search Metadata
#'
#' search_metadata allows users to retrieve a data frame of
#' variables based on whether or not those variables contain a given query
#' within a given field, or the entire set of metadata if the query
#' is empty
#'
#' @param query a substring searched for in the given variable field
#' @param field_name the field in which the query is searched for.
#' See details for valid field names.
#'
#' @return returns data frame of variables given a query string and a
#' search category, or the entire set of metadata as a data frame if the
#' query is empty
#'
#' @details List of valid field names:
#' \itemize{
#'   \item{new_name: the new, standardized variable name}
#'   \item{old_name: the original variable name from older files}
#'   \item{type: variable type. Options are binary (bin), continuous (cont),
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
#'   \item{source: source of variable}
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
#' filter_metadata allows users to retreive a data frame
#' of variables based on a set of filter values for variable categories,
#' or the entire set of metadata as a data frame if no filters are provided
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
#'   \item{new_name: the new, standardized variable name}
#'   \item{old_name: the original variable name from older files}
#'   \item{type: variable type. Options are binary (bin), continuous (cont),
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
#'   \item{source: source of variable}
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

