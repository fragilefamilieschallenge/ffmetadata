call_api <- function(endpoint, ...) {
  # create link based on specified endpoint
  baseUrl <- paste(baseUrl, endpoint, ".php?", sep = "")
  # retrieve and iterate through params
  params <- list(...)
  for (i in 1:length(params)) {
    baseUrl <- paste(baseUrl, names(params)[i], "=", params[i], "&",
                     sep = "")
  }
  # problem with JSON objects not being separated by new line characters
  # inefficient workaround
  lines <- readLines(baseUrl)
  m <- gregexpr("}", lines, perl = TRUE)
  # add new line after each matched instance of "}"
  regmatches(lines, m) <- "}\n"
  # write to file and then read to file (inefficient)
  write(lines, "json_temp.txt")
  metadata <- jsonlite::stream_in(file("json_temp.txt"))
  return(metadata)
}

params_from_call <- function(match_call) {
  params <- as.list(match_call)
  # rename first element in order for call_api to recognize params
  names(params)[1] <- "endpoint"
  # convert first parameter to character type
  params[1] <- as.character(params[1])
  return(params)
}
