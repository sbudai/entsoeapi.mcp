#' Parse a date string to a CET-timezone POSIXct
#'
#' Accepts YYYY-MM-DD strings (and passes POSIXct through unchanged).
#' @noRd
parse_date <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  lubridate::ymd(x, tz = "CET")
}


#' Drop constant metadata columns from a tidy time-series data frame.
#'
#' entsoeapi tidy output repeats domain names, currency codes, and unit labels
#' on every row. Removing them can cut JSON payload size by 50%+ with no loss
#' of information (the LLM already knows the query context).
#'
#' Columns are dropped only when: (a) every non-NA value is identical, AND
#' (b) the column name does not match any of the "always keep" patterns below.
#'
#' @noRd
slim_ts <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0L) return(df)

  # Patterns for columns that are always informative even when constant
  keep_pattern <- paste(
    "eic|mrid|code|name|start|end|position|price|quantity|amount|value|rate|type",
    sep = "|"
  )

  is_constant <- vapply(df, function(col) {
    vals <- col[!is.na(col)]
    length(vals) == 0L || length(unique(vals)) == 1L
  }, logical(1L))

  keep_always  <- grepl(keep_pattern, names(df), ignore.case = TRUE)
  df[, !(is_constant & !keep_always), drop = FALSE]
}


#' Serialise a data frame to JSON, slimming and capping rows to avoid
#' overwhelming the LLM context window.
#'
#' @noRd
safe_to_json <- function(df, max_rows = 100L) {
  if (is.null(df) || (is.data.frame(df) && nrow(df) == 0L)) {
    return("[]")
  }
  if (is.data.frame(df)) {
    df <- slim_ts(df)
    if (nrow(df) > max_rows) df <- df[seq_len(max_rows), ]
  }
  jsonlite::toJSON(df, auto_unbox = TRUE, date_format = "ISO8601", na = "null")
}
