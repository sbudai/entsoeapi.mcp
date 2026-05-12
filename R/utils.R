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


#' Cache a data frame in the session DuckDB and return an envelope.
#'
#' Used by the 21 time-series tools to push the full result into an in-memory
#' DuckDB table; the LLM then aggregates / filters / joins via the `sql_query`
#' tool. Constant metadata columns are dropped first via [slim_ts()] — they
#' waste DuckDB space *and* preview tokens.
#'
#' Falls back to [safe_to_csv()] for NULL / empty frames and non-data-frame
#' results (occasional `entsoeapi` helpers return nested lists).
#'
#' @param df A data frame, or NULL / empty / list (handled gracefully).
#' @param prefix Short label for the cached table name, e.g. `"load_actual"`.
#' @param args_list Named list of identifying args (hashed into the suffix).
#'
#' @return The envelope text produced by [db_store()], or a fallback string.
#'
#' @noRd
safe_to_cache <- function(df, prefix, args_list) {
  if (is.null(df) || (is.data.frame(df) && nrow(df) == 0L)) return("(no data)")
  if (!is.data.frame(df)) return(safe_to_csv(df))
  df <- slim_ts(df)
  db_store(df = df, prefix = prefix, args_list = args_list)
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
