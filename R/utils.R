#' Parse a date string to a CET-timezone POSIXct
#'
#' Accepts YYYY-MM-DD strings (and passes POSIXct through unchanged).
#' @noRd
parse_date <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  lubridate::ymd(x, tz = "CET")
}


#' Serialise a data frame to JSON, capping rows to avoid overwhelming the LLM
#'
#' @noRd
safe_to_json <- function(df, max_rows = 200L) {
  if (is.null(df) || (is.data.frame(df) && nrow(df) == 0L)) {
    return("[]")
  }
  if (is.data.frame(df) && nrow(df) > max_rows) {
    df <- df[seq_len(max_rows), ]
  }
  jsonlite::toJSON(df, auto_unbox = TRUE, date_format = "ISO8601", na = "null")
}
