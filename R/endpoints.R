#' Select Metadata
#'
#' select_metadata allows users to retrieve the value of
#' a specific field or fields of a given variable, or
#' the entire variable metadata if no field is specified
#'
#' @param variable_name name of variable
#' @param fields specific field or list of fields of variable to be accessed. See
#' details for valid field names
#' @param returnDataFrame optional parameter set to TRUE by default, output is
#' named list if set to FALSE
#'
#' @return returns string with value of a given field if field is specified,
#' returns all metadata for variable as data frame if field is unspecified
#'
#' @details List of valid field names:
#' \itemize{
#'     \item{data_source}
#'     \item{data_type}
#'     \item{fp_PCG}
#'     \item{fp_father}
#'     \item{fp_fchild}
#'     \item{fp_mother}
#'     \item{fp_other}
#'     \item{fp_partner}
#'     \item{group_id}
#'     \item{group_subid}
#'     \item{id}
#'     \item{label}
#'     \item{leaf}
#'     \item{measures}
#'     \item{name}
#'     \item{old_name}
#'     \item{probe}
#'     \item{qText}
#'     \item{respondent}
#'     \item{scope}
#'     \item{section}
#'     \item{warning}
#'     \item{wave}
#' }
#'
#' @export
#'
#' @importFrom utils URLencode
#'
#' @examples
#' select1 <- select_metadata(variable_name = "ce3agefc")
#'
#' select2 <- select_metadata(variable_name = "ce3agefc", fields = "data_type")
#'
#' select3 <- select_metadata(variable_name = "ce3agefc", fields = c("data_type", "data_source"))
select_metadata <- function(variable_name = NULL, fields = NULL, returnDataFrame = TRUE) {
  # error message for if variable_name is missing
  if (missing(variable_name)) {
    stop("select_metadata requires a value for variable_name")
  }
  # format url
  url <- httr::modify_url(.base_url, path = paste0("variable/", variable_name))
  if (!is.null(fields)) {
    url <- httr::modify_url(url, query = ifelse(length(fields) == 1, fields, fields[1]))
  }
  # encode url after adding fields
  i <- 2
  while (i <= length(fields)) {
    url <- paste0(url, "&", fields[i])
    url <- URLencode(url)
    i <- i + 1
  }
  result <- call_api(url)
  # format single value as character, otherwise unlist to convert to data frame
  if (length(result) == 1) {
    result <- as.character(result)
  } else if (returnDataFrame == TRUE) {
    # return data frame
    result <- unlist(result)
    result <- as.data.frame(result)
    result <- t(result)
  }
  return(result)
}

#' Search Metadata
#'
#' search_metadata allows users to retrieve a list of
#' variable names based on whether or not those variables contain a given query or set of
#' queries within a given field or set of fields
#'
#' @param ... any valid field name
#' @param filter_list an optional named list that can be passed to search_metadata in addition
#' to explicitly named parameters
#' @param operation optional parameter that allows one to specify the given
#' the method of comparison for search, "equals" is the default
#'
#' @return returns list of names of all variables that match specified parameter values
#'
#' @details List of valid field names:
#' \itemize{
#'     \item{data_source}
#'     \item{data_type}
#'     \item{fp_PCG}
#'     \item{fp_father}
#'     \item{fp_fchild}
#'     \item{fp_mother}
#'     \item{fp_other}
#'     \item{fp_partner}
#'     \item{group_id}
#'     \item{group_subid}
#'     \item{id}
#'     \item{label}
#'     \item{leaf}
#'     \item{measures}
#'     \item{name}
#'     \item{old_name}
#'     \item{probe}
#'     \item{qText}
#'     \item{respondent}
#'     \item{scope}
#'     \item{section}
#'     \item{warning}
#'     \item{wave}
#' }
#' @export
#'
#' @examples
#' search_test1 <- search_metadata(wave = "Year 1")
#'
#' search_test2 <- search_metadata(wave = "Year 1", respondent = "Mother")
#'
#' search_test3 <- search_metadata(wave = "Year 1", name = "f%", operation = c("eq", "like"))
#'
#' search_test4 <- search_metadata(name = "f1%", operation = "like")
#'
#' # Note that when using operators checking for null values (or lack thereof), the operation
#' # parameter should not be included in the call. It should be formatted instead like so:
#' search_test5 <- search_metadata(qText = "is_null")
search_metadata <- function(filter_list=list(), ..., operation = "eq") {

  if (length(list(...)) > length(operation) & length(operation) > 1) {
    stop("number of comparison operators must equal number of field names being
         used for search if multiple comparison operators are specified")
  }

  # format parameters
  params <- c(filter_list, list(...))

  # create data frame to hold filters
  filters <-  as.data.frame(matrix(nrow = length(params), ncol = 3))
  colnames(filters) <- c("name", "val", "op")

  for (i in 1:length(params)) {
    # add name
    filters[i, ]$name <- names(params)[i]

    # check if parameter value is list
    if (length(params[[i]]) > 1) {
      # nest in list to format for JSON
      filters[i, ]$val <- list(params[[i]])
      # if multiple operators used
      if (length(operation) > 1) {
        filters[i, ]$op <- operation[[i]]
      } else {
        # only one comparison operator used
        filters[i, ]$op <- operation
      }
    }
    # check for null operators
    else if (params[[i]] == "is_null" | params[[i]] == "is_not_null") {
        filters[i, ]$op <- params[[i]]
    }
    # otherwise standard operators
    else {
      filters[i, ]$val <- params[[i]]
      # if multiple operators used
      if (length(operation) > 1) {
        filters[i, ]$op <- operation[[i]]
      } else {
        # only one comparison operator used
        filters[i, ]$op <- operation
      }
    }
  }

  # name data frame as "filters" in list
  filter_list <- list(filters = filters)
  # convert to JSON as per endpoint syntax
  formatted_params <- jsonlite::toJSON(filter_list)
  # create url
  url <- httr::modify_url(.base_url, query = list(q = formatted_params))
  # call api
  searched <- call_api(url)
  return(searched)
}

