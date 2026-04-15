# ============================================================
# Shared Description Constants (token optimization)
# ============================================================

#' Common argument descriptions - defined once, reused everywhere.
#'
#' This reduces token usage by avoiding repetition across 30+ tools.
#' These constants are used internally for tool descriptions.
#'
#' @keywords internal
#' @export
desc_eic <- "EIC code of the bidding zone."

#' @keywords internal
#' @export
desc_eic_in <- "EIC code of the importing bidding zone."

#' @keywords internal
#' @export
desc_eic_out <- "EIC code of the exporting bidding zone."

#' @keywords internal
#' @export
desc_period_start <- "Start date in YYYY-MM-DD format (CET)."

#' @keywords internal
#' @export
desc_period_end <- "End date in YYYY-MM-DD format (CET)."

#' @keywords internal
#' @export
desc_year <- "Year as integer, e.g. 2024."

#' @keywords internal
#' @export
desc_gen_type <- "Optional ENTSO-E production type code, e.g. 'B01' (Biomass), 'B16' (Solar)."

#' @keywords internal
#' @export
desc_psr_type <- "Optional PSR type code, e.g. 'B16' for Solar PV."

#' @keywords internal
#' @export
desc_contract_type_da <- "Contract type: 'A01' Day ahead (default), 'A07' Intraday."

#' @keywords internal
#' @export
desc_doc_status <- "Document status: 'A05' active, 'A09' cancelled, 'A13' withdrawn."

#' @keywords internal
#' @export
desc_event_nature <- "Event nature: 'A53' planned, 'A54' unplanned."

#' @keywords internal
#' @export
desc_n <- "Number of items to return."

#' Common tool description suffixes.
#'
#' These constants are used internally for building tool descriptions.
#'
#' @keywords internal
#' @export
suffix_max_1yr <- "Max 1-year range."

#' @keywords internal
#' @export
suffix_use_area_eic <- "Use area_eic() to find EIC codes."


# ============================================================
# Date Parsing
# ============================================================

#' Parse a date string to a CET-timezone POSIXct
#'
#' @param ts Accepts YYYY-MM-DD strings (and passes POSIXct through unchanged)
#' @param tz timezone
#'
#' @return a vector of class POSIXct
#'
#' @noRd
parse_date <- function(ts, tz = "CET") {
  if (inherits(x = ts, what = "POSIXct")) return(ts)
  lubridate::ymd(ts, tz = tz)
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
#' @param df the dataframe for columnwise shrinkage
#' @param keep_pattern patterns for columns that are always informative
#'   even when constant
#'
#' @return the dataframe without informative constant columns
#'
#' @noRd
slim_ts <- function(
  df,
  keep_pattern = paste(
    "eic", "mrid", "code", "name", "start", "end", "position", "price",
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


#' Serialise a data frame to JSON, slimming and capping rows to avoid
#' overwhelming the LLM context window.
#'
#' @param df a dataframe to convert to JSON
#' @param max_rows how many rows of the dataframe to convert to JSON
#'
#' @return JSON string
#'
#' @noRd
safe_to_json <- function(df, max_rows = 100L) {
  if (is.null(df) || (is.data.frame(df) && nrow(df) == 0L)) return("[]")
  if (is.data.frame(df)) {
    df <- slim_ts(df)
    if (nrow(df) > max_rows) df <- df[seq_len(max_rows), ]
  }
  jsonlite::toJSON(
    x = df,
    auto_unbox = TRUE,
    date_format = "ISO8601",
    na = "null"
  )
}
