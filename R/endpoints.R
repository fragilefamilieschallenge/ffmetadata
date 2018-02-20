#' Select Metadata
#'
#' Modified from PHP Web API definition: select_metadata
#' returns a specific field of a given variable, or
#' the entire variable if no field is specified
#'
#' @param variable_name name of variable
#' @param field_name specific field of variable
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
#' Modified from PHP Web API: search_metadata returns a
#' data frame of variables given a query string and a search category,
#' or the entire set of metadata as a data frame if query is empty
#' @param query a substring searched for in the given search category
#' @param field_name the search category
#'
#' @return returns data frame of variables given a query string and a
#' search category, or the entire set of metadata as a data frame if the
#' query is empty
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
#' Modified from PHP Web API definition: filter_metadata returns a data frame
#' of variables given a set of filter values for variable categories,
#' or the entire set of metadata as a data frame if no filters are provided
#' @param new_name the new, standardized variable name
#' @param old_name the original variable name from older files
#' @param type variable type
#' @param warning flags for issues
#' @param group group number (matches the same questions across surveys)
#' @param q_group_N number of variables that are in the group
#' @param topic1 first concept tag
#' @param topic2 second concept tag
#' @param source source of variable
#' @param respondent respondent, either the person interviewed or the place an
#' interview took place
#' @param wave the wave of data collection associated with the variable
#' @param scope the reach of a variable's sample population
#' @param section section of survey where question is asked
#' @param leaf the rest of the variable name
#' @param q_group_list a string that lists all of the new variable names
#' for all the variables in the group
#' @param value1-value78 all of the potential responses for a given variable
#' @param label1-label68 all of the labels for the potential responses for
#' a given variable
#'
#' @return returns data frame of variable metadata based on a set of given
#' filter values, or the entire set of metadata if no filters are provided
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

