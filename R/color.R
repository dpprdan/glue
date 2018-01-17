crayon_transformer <- function(code, envir, data) {
  res <- tryCatch(parse(text = code, keep.source = FALSE), error = function(e) e)
  if (inherits(res, "error")) {
    if (grepl("^\\w+\\s+", code)) {
      fun <- gsub("^(\\w+)\\s+.*", "\\1", code)
      text <- gsub("^\\w+\\s+(.*)", "\\1", code)
      out <- glue_data(data, text, .envir = envir, .transformer = crayon_transformer)
      return((get(fun))(out))
    }
    stop(res)
  }
  eval2(res, envir = envir, data = data)
}
