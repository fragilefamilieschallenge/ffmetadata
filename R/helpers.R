
# endpoint : api endpoint. e.g. "selectMetadata.php"
# params : a named list of url parameters
call_api <- function(endpoint, params) {
  endpoint <- paste(endpoint, "?", sep = "")
  # create url based on specified endpoint
  url <- httr::modify_url(.base_url, path = endpoint)
  # retrieve and iterate through params
  for (i in 1:length(params)) {
    if(!is.null(params[[i]])) {
      # append parameters
      url <- paste(url, names(params)[i], "=", params[[i]], "&",
                        sep = "")
    }
  }
  # get HTTP response
  resp <- httr::GET(url)
  # ensure JSON is returned
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  # read JSON
  metadata <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),
                                 simplifyVector = FALSE, simplifyDataFrame = TRUE)

  # error checking
  if (!is.null(metadata$`error code`)) {
     stop(
       sprintf(
         "GitHub API request failed [%s]\n<%s>",
         metadata$`error code`,
         metadata$error_description
       ),
       call. = FALSE
     )
  }
  return(metadata)
}

convert_json <- function(data) {
  # initialize new data frame with number of columns matching json fields
  data_frame <- data.frame(matrix(ncol = length(data), nrow = 1))
  # name columns after json fields
  names(data_frame) <- names(data)
  # populate data frame
  for (name in names(data)) {
    # test if list object
    if (is.atomic(data[[name]])) {
      data_frame[1, name] <- ifelse(is.null(data[[name]]), NA, data[[name]])
    } else {
      # store list in row
      data_frame[1, name][[1]] <- list(data[[name]])
    }
  }
  return(data_frame)
}
