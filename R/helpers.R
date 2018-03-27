
# endpoint : api endpoint. e.g. "selectMetadata.php"
# params : a named list of url parameters
call_api <- function(endpoint, params) {
  # create link based on specified endpoint
  base_url <- paste(.base_url, endpoint, "?", sep = "")
  # retrieve and iterate through params
  for (i in 1:length(params)) {
    if(!is.null(params[[i]])) {
      # rename for naming conventions
      if (names(params)[i] == "field_name") {
        names(params)[i] <- "fieldName"
      } else if (names(params)[i] == "variable_name") {
        names(params)[i] <- "varName"
      }
      base_url <- paste(base_url, names(params)[i], "=", params[[i]], "&",
                        sep = "")
    }
  }
  # read JSON
  metadata <- jsonlite::fromJSON(base_url)
  return(metadata)
}
