#' Parse a date string to a CET-timezone POSIXct
#'
#' @param ts A date string in `"YYYY-MM-DD"` format, or a `POSIXct` object
#'   (passed through unchanged).
#' @param tz Time zone string passed to `lubridate::ymd()`; defaults to
#'   `"CET"`.
#'
#' @importFrom lubridate ymd
#'
#' @return a vector of class POSIXct
#'
#' @noRd
parse_date <- function(ts, tz = "CET") {
  if (inherits(x = ts, what = "POSIXct")) return(ts)
  ymd(ts, tz = tz)
}


#' Drop constant metadata columns from a tidy time-series data frame.
#'
#' entsoeapi tidy output repeats domain names, currency codes, and unit labels
#' on every row. Removing them can cut CSV payload size by 50%+ with no loss
#' of information (the LLM already knows the query context).
#'
#' Columns are dropped only when: (a) every non-NA value is identical, AND
#' (b) the column name does not match any of the "always keep" patterns below.
#'
#' @param df the dataframe for columnwise shrinkage
#' @param keep_pattern patterns for columns that are always informative
#'   even when constant
#'
#' @return the dataframe without uninformative constant columns
#'
#' @noRd
slim_ts <- function(
  df,
  keep_pattern = paste(
    "eic", "name", "start", "end", "position", "price",
    "quantity", "amount", "value", "rate", "type", sep = "|"
  )
) {
  if (!is.data.frame(df) || nrow(df) == 0L) return(df)

  is_constant <- vapply(
    X = df,
    FUN = \(col) {
      vals <- col[!is.na(col)]
      length(vals) == 0L || length(unique(vals)) == 1L
    },
    FUN.VALUE = logical(1L)
  )

  keep_always <- grepl(
    pattern = keep_pattern,
    x = names(df),
    ignore.case = TRUE
  )
  df[, !(is_constant & !keep_always), drop = FALSE]
}


#' Serialise a data frame to CSV, slimming and capping rows to avoid
#' overwhelming the LLM context window.
#'
#' Compared with JSON, CSV omits repeated column keys on every row, typically
#' reducing output size 60-70% for wide time-series data frames. A truncation
#' notice is appended when rows are dropped so the LLM knows data is partial.
#'
#' @param df a dataframe to convert to CSV
#' @param max_rows how many rows of the dataframe to include
#'
#' @importFrom utils capture.output write.csv
#' @importFrom jsonlite toJSON
#'
#' @return a CSV string; falls back to JSON for non-data-frame inputs
#'
#' @noRd
safe_to_csv <- function(df, max_rows = 100L) {
  if (is.null(df) || (is.data.frame(df) && nrow(df) == 0L)) return("(no data)")
  if (is.data.frame(df)) {
    total_rows <- nrow(df)
    df <- slim_ts(df)
    truncated <- nrow(df) > max_rows
    if (truncated) df <- df[seq_len(max_rows), ]
    out <- paste(
      capture.output(write.csv(x = df, file = stdout(), row.names = FALSE)),
      collapse = "\n"
    )
    if (truncated) {
      out <- paste0(
        out,
        "\n# truncated: showing ", max_rows, " of ", total_rows, " rows"
      )
    }
    return(out)
  }
  # Fallback for non-data-frame results (e.g. nested lists)
  toJSON(df, auto_unbox = TRUE, date_format = "ISO8601", na = "null")
}
