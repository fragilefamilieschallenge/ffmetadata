# params : a named list of url parameters
call_api <- function(url) {
  # # create url based on specified endpoint
  # url <- paste(.base_url, endpoint, sep = "")
  # # prepare url for queries
  # if (!is.null(params)) {
  #   url <- httr::modify_url(url, query = params[[1]])
  # }
  # print(length(params))
  # # retrieve and iterate through params
  # for (i in 2:length(params)) {
  #    if(!is.null(params[[i]])) {
  #      if (i == length(params)) {
  #        url <- paste(url, params[[i]], sep = "")
  #      } else {
  #        # append parameters
  #        url <- paste(url, params[[i]], "&",
  #                     sep = "")
  #      }
  #    }
  # }
  # print(url)
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
