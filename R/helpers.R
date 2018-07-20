# params : a named list of url parameters
call_api <- function(url) {
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
