selectMetadata <- function(new_name = NULL, field_name = NULL) {
  # validate field_name
  if (!(field_name %in% validNames)) {
    stop(field_name, " is not a valid field name")
  }
  params <- params_from_call(match.call())
  do.call("call_api", params)
}

searchMetadata <- function(query = NULL, field_name = NULL) {
  # validate field_name
  if (!(field_name %in% validNames)) {
    stop(field_name, " is not a valid field name")
  }
  params <- params_from_call(match.call())
  do.call("call_api", params)
}

filterMetadata <- function(...) {
  params <- params_from_call(match.call())
  # validate parameters, ignoring the first entry (the function name)
  for (i in 2:length(params)) {
    if (!(names(params)[i] %in% validNames)) {
      stop(names(params)[i], " is not a valid field name")
    }
  }
  do.call("call_api", params)
}

