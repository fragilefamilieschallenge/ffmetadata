
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
