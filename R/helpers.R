
# endpoint : api endpoint. e.g. "selectMetadata.php"
# params : a named list of url parameters
call_api <- function(endpoint, params) {
  # create link based on specified endpoint
  base_url <- paste(.base_url, endpoint, "?", sep = "")
  # retrieve and iterate through params
  for (i in 1:length(params)) {
    if(!is.null(params[[i]]))
      base_url <- paste(base_url, names(params)[i], "=", params[[i]], "&",
                     sep = "")
  }
  # problem with API not returning JSON objects
  lines <- readLines(utils::URLencode(base_url), warn=FALSE)
  meta <- list()
  for(i in 1:length(lines)) {
    if(!is.na(lines[i]) && nchar(lines[i])>1)
      meta[[i]] <- as.data.frame(jsonlite::fromJSON(lines[i]))
  }
  metadata <- do.call(rbind,meta)

  return(metadata)
}
