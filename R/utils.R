#' Parse a date string to a CET-timezone POSIXct
#'
#' An unparseable value raises an error naming the argument and the expected
#' format, so the LLM can correct the call. Without it, `ymd()` quietly
#' returns `NA`, and the error surfacing later from entsoeapi does not say
#' which value was wrong.
#'
#' @param ts A date string in `"YYYY-MM-DD"` format, or a `POSIXct` object
#'   (passed through unchanged).
#' @param tz Time zone string passed to `lubridate::ymd()`; defaults to
#'   `"CET"`.
#' @param arg Name used in the error message; defaults to the variable
#'   passed as `ts` (e.g. `period_start`).
#'
#' @importFrom lubridate ymd
#'
#' @return a vector of class POSIXct
#'
#' @noRd
parse_date <- function(
  ts,
  tz = "CET",
  arg = if (is.name(substitute(ts))) deparse(substitute(ts)) else "date"
) {
  if (inherits(x = ts, what = "POSIXct")) return(ts)
  out <- if (is.character(ts) && length(ts) == 1L && !is.na(ts)) {
    suppressWarnings(ymd(ts, tz = tz))  # nolint: object_usage_linter
  }
  if (length(out) != 1L || is.na(out)) {
    got <- paste(deparse(ts), collapse = " ")
    stop(
      arg, " must be a date in YYYY-MM-DD format (e.g. '2024-03-15'), got ",
      got,
      call. = FALSE
    )
  }
  out
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
#' tool. Constant metadata columns are dropped first via `slim_ts()` — they
#' waste DuckDB space *and* preview tokens.
#'
#' Falls back to `safe_to_csv()` for NULL / empty frames and non-data-frame
#' results (occasional `entsoeapi` helpers return nested lists).
#'
#' The tools pipe the `entsoeapi` call straight into this function, so `df`
#' arrives as an unevaluated promise. The cache is checked BEFORE `df` is
#' touched: when the same call was already made in this session, the API
#' request is never sent and the cached table is described instead. Do not
#' reference `df` above that check.
#'
#' @param df A data frame, or NULL / empty / list (handled gracefully).
#' @param prefix Short label for the cached table name, e.g. `"load_actual"`.
#' @param args_list Named list of identifying args (hashed into the suffix).
#'
#' @return The envelope text produced by `db_store()` / `db_describe()`, or a
#'   fallback string.
#'
#' @importFrom DBI dbExistsTable
#'
#' @noRd
safe_to_cache <- function(df, prefix, args_list) {
  table <- db_table_name(prefix = prefix, args_list = args_list)  # nolint: object_usage_linter
  if (dbExistsTable(conn = db_con(), name = table)) {  # nolint: object_usage_linter
    return(db_describe(name = table))  # nolint: object_usage_linter
  }
  if (is.null(df) || (is.data.frame(df) && nrow(df) == 0L)) return("(no data)")
  if (!is.data.frame(df)) return(safe_to_csv(df))
  df <- slim_ts(df = df)
  db_store(df = df, prefix = prefix, args_list = args_list)  # nolint: object_usage_linter
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
    out <- write.csv(x = df, file = stdout(), row.names = FALSE) |>
      capture.output() |>
      paste(collapse = "\n")
    if (truncated) {
      out <- paste0(
        out,
        "\n# truncated: showing ", max_rows, " of ", total_rows, " rows"
      )
    }
    return(out)
  }
  # Fallback for non-data-frame results (e.g. nested lists)
  toJSON(x = df, auto_unbox = TRUE, date_format = "ISO8601", na = "null")  # nolint: object_usage_linter
}
