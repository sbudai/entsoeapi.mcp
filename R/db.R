# ============================================================
# Session-scoped DuckDB cache
# ============================================================
#
# Every time-series tool inserts its full result into an in-memory DuckDB
# database held inside the running R process. The LLM gets back a compact
# envelope (table name + row count + schema + 5-row preview + SQL hint)
# instead of capped CSV; aggregation happens inside the container via the
# new sql_query / list_tables / describe_table tools.
#
# Connection lifetime = container lifetime. The Docker config uses
# `docker run --rm -i`, so the DB dies when the MCP session ends - exactly
# the cleanup story we want for session scratch space.

# Package-level environment that holds the singleton connection.
# Using emptyenv() as parent prevents accidental fall-through to globalenv()
# if a NULL-returning lookup happens during lazy load.
.pkg_env <- new.env(parent = emptyenv())


#' Lazy singleton in-memory DuckDB connection
#'
#' First call opens the connection; subsequent calls return the cached one.
#' If the connection has been invalidated (e.g. by a test that closed it),
#' it is re-opened transparently.
#'
#' @return A live `duckdb_connection` valid for the package session.
#'
#' @importFrom DBI dbIsValid dbConnect dbDisconnect
#' @importFrom duckdb duckdb
#'
#' @noRd
db_con <- function() {
  if (is.null(.pkg_env$con) || !dbIsValid(.pkg_env$con)) {
    .pkg_env$con <- dbConnect(drv = duckdb(), dbdir = ":memory:")
    # Best-effort clean shutdown when the R session exits. `try()` guards
    # against the (rare) case where the connection is already dead.
    reg.finalizer(
      e = .pkg_env,
      f = \(env) {
        try(expr = dbDisconnect(conn = env$con, shutdown = TRUE), silent = TRUE)
      },
      onexit = TRUE
    )
  }
  .pkg_env$con
}


#' Build a deterministic, content-addressed table name
#'
#' `db_table_name("load_actual", list(eic = "...", ps = ..., pe = ...))`
#' -> `"load_actual_a3f7d2"`. Identical args always hash to the same suffix,
#' which means repeat calls within a session reuse the existing cached
#' table - no second hit to the ENTSO-E API.
#'
#' Six hex chars give 16.7M-name space, plenty for a session-scoped cache.
#'
#' @param prefix Short, human-readable label, e.g. `"load_actual"`.
#' @param args_list Named list of arguments that uniquely identify the call.
#'
#' @return A character scalar table name.
#'
#' @importFrom digest digest
#'
#' @noRd
db_table_name <- function(prefix, args_list) {
  h <- substr(
    x = digest(object = args_list, algo = "md5"), start = 1L, stop = 6L
  )
  paste0(prefix, "_", h)
}


#' Render a data frame to the envelope text returned to the LLM
#'
#' Plain-text format optimised for LLM comprehension. The directive block
#' is placed BEFORE the schema sample so the model reads "do not compute
#' stats from these rows" before it sees rows that look statistic-able:
#'
#' ```
#' # table: <name>
#' # rows: <n>
#' # columns: <col> <TYPE>, ...
#' # ------------------------------------------------------------
#' # IMPORTANT - read before using this result: ...
#' # ------------------------------------------------------------
#' # WARNING: multiple series present in this table:   (only if detected)
#' #   sequence: 1, 2   (filter explicitly; ...)
#' # schema sample (k of n rows; do not use for analysis):
#' <csv header>
#' <csv sample rows>
#' # suggested query: <sql aggregation>
#' ```
#'
#' @param table Character scalar; the DuckDB table name.
#' @param n_rows Integer; total row count of the cached table.
#' @param df_preview Data frame; small sample used for the schema preview.
#' @param columns Named character vector mapping column names to DuckDB type
#'   labels (as produced by `.r_type_label()`).
#' @param hint Character scalar; a suggested SQL aggregation query to display.
#' @param multi_series_lines  Character vector of pre-formatted WARNING body
#'   lines (each already prefixed with `"#   "`). Pass `character(0L)` to
#'   omit the WARNING block entirely.
#'
#' @return a data.frame and its related metadata in text format
#'
#' @importFrom utils capture.output write.csv
#'
#' @noRd
.format_envelope <- function(
  table,
  n_rows,
  df_preview,
  columns,
  hint,
  multi_series_lines = character(0L)
) {
  col_descr <- paste(
    paste(names(columns), columns), collapse = ", "
  )
  preview_csv <- write.csv(
    x = df_preview, file = stdout(), row.names = FALSE
  ) |>
    capture.output() |>
    paste(collapse = "\n")
  divider <- paste0("# ", strrep(x = "-", times = 60))
  directive <- paste(
    divider,
    "# IMPORTANT - read before using this result:",
    "#   This is a CACHED TABLE, not a result set. The rows below",
    "#   are a SCHEMA SAMPLE only - they are NOT representative of",
    "#   the data, may omit other series, and MUST NOT be used for",
    "#   any analysis. NEVER compute averages, totals, min/max,",
    "#   counts, or any statistic from this sample.",
    "#   ALWAYS call sql_query(...) against the table named above",
    "#   for ANY aggregation, filter, statistic, or join.",
    divider,
    sep = "\n"
  )
  warning_block <- if (length(multi_series_lines) > 0L) {
    paste0(
      "# WARNING: multiple series present in this table:\n",
      paste(multi_series_lines, collapse = "\n"),
      "\n"
    )
  } else {
    ""
  }
  paste0(
    "# table: ", table, "\n",
    "# rows: ", n_rows, "\n",
    "# columns: ", col_descr, "\n",
    directive, "\n",
    warning_block,
    "# schema sample (", nrow(df_preview), " of ", n_rows,
    " rows; do not use for analysis):\n",
    preview_csv, "\n",
    "# suggested query: ", hint
  )
}


#' Detect ENTSO-E "multiple series" columns in a cached table
#'
#' ENTSO-E auction documents commonly ship more than one `TimeSeries` per
#' response (e.g. day-ahead prices: Sequence 1 = binding, Sequence 2 =
#' shadow/SDAC rerun). After `slim_ts()` constant-column pruning, columns
#' that survive *and* have > 1 distinct value are a strong signal that the
#' cached table contains several series stacked together. The LLM needs to
#' know this so it filters explicitly before aggregating.
#'
#' We probe a small, hand-picked candidate list (cheap `intersect()` against
#' actual column names) and `LIMIT 10` the DISTINCT scan so the check stays
#' O(1) regardless of table size.
#'
#' @param con A live DuckDB connection, as returned by `db_con()`.
#' @param table Character scalar; the DuckDB table name to inspect.
#'
#' @return Character vector of pre-formatted `"#   col: v1, v2, ..."` lines;
#'   `character(0L)` when nothing multi-valued is found.
#'
#' @importFrom DBI dbListFields dbQuoteIdentifier dbGetQuery
#'
#' @noRd
.detect_multi_series <- function(con, table) {
  cols <- dbListFields(conn = con, name = table)
  candidates <- c(
    "sequence", "time_series_mrid", "auction_type",
    "business_type", "process_type", "ts_business_type",
    "ts_curve_type", "constraint_ts_business_type"
  )
  cols_to_check <- intersect(x = candidates, y = cols)
  if (length(cols_to_check) == 0L) return(character(0L))

  out <- character(0L)
  for (col in cols_to_check) {
    qcol <- dbQuoteIdentifier(conn = con, x = col)
    qtab <- dbQuoteIdentifier(conn = con, x = table)
    vals <- dbGetQuery(
      conn = con,
      statement = sprintf(
        fmt = "SELECT DISTINCT %s AS v FROM %s WHERE %s IS NOT NULL LIMIT 10",
        qcol, qtab, qcol
      )
    )$v
    if (length(vals) > 1L) {
      out <- c(
        out,
        sprintf(
          fmt = "#   %s: %s     (filter explicitly; do not assume one)",
          col, paste(vals, collapse = ", ")
        )
      )
    }
  }
  out
}


#' Map an R column to a DuckDB type label (best-effort, for the envelope only)
#'
#' DuckDB does the real type inference on INSERT; this is purely a human
#' label for the envelope.
#'
#' @param col the column name to map
#'
#' @return a DuckDB type label (character)
#'
#' @noRd
.r_type_label <- function(col) {
  if (inherits(x = col, what = "POSIXct")) return("TIMESTAMP")
  if (inherits(x = col, what = "Date")) return("DATE")
  if (is.integer(col)) return("INTEGER")
  if (is.numeric(col)) return("DOUBLE")
  if (is.logical(col)) return("BOOLEAN")
  "VARCHAR"
}


#' Build a SQL aggregation hint suitable for the data frame's schema
#'
#' If there's a timestamp column and at least one numeric column, suggest a
#' weekly aggregation. Otherwise fall back to `SELECT *`.
#'
#' @param table Character scalar; the DuckDB table name used in the generated
#'   SQL.
#' @param df Data frame; used to detect timestamp and numeric columns.
#'
#' @return a SQL aggregation hint (character)
#'
#' @noRd
.build_hint <- function(table, df) {
  ts_col <- names(df)[vapply(
    X = df,
    FUN = \(c) {
      inherits(x = c, what = "POSIXct") || inherits(x = c, what = "Date")
    },
    FUN.VALUE = logical(1L)
  )]
  num_col <- names(df)[vapply(
    X = df,
    FUN = \(c) {
      is.numeric(c) && !inherits(x = c, what = "POSIXct") &&
        !inherits(x = c, what = "Date")
    },
    FUN.VALUE = logical(1L)
  )]
  if (length(ts_col) >= 1L && length(num_col) >= 1L) {
    return(
      sprintf(
        fmt = paste(
          "SELECT date_trunc('week', %s) AS wk,",
          "AVG(%s) FROM %s GROUP BY 1 ORDER BY 1"
        ),
        ts_col[1L], num_col[1L], table
      )
    )
  }
  sprintf(fmt = "SELECT * FROM %s LIMIT 20", table)
}


#' Insert a data frame into the cache; return the envelope
#'
#' If a table with the same content-addressed name already exists, the
#' insert is skipped (cache hit) and the existing row count is reported.
#'
#' @param df A data frame.
#' @param prefix Short, human-readable label for the table name.
#' @param args_list Named list of identifying args (hashed into the suffix).
#' @param preview_rows  How many sample rows to include in the envelope.
#'
#' @return The envelope text described in `.format_envelope()`.
#'
#' @importFrom DBI dbExistsTable dbWriteTable dbGetQuery
#'
#' @noRd
db_store <- function(df, prefix, args_list, preview_rows = 5L) {
  con <- db_con()
  table <- db_table_name(prefix = prefix, args_list = args_list)

  # Cache hit: same args -> same table -> skip the INSERT.
  if (!dbExistsTable(conn = con, name = table)) {
    dbWriteTable(
      conn = con,
      name = table,
      value = df,
      overwrite = FALSE,
      append = FALSE
    )
  }

  n_rows <- dbGetQuery(
    conn = con,
    statement = sprintf(fmt = "SELECT COUNT(*) AS n FROM %s", table)
  )$n
  columns <- vapply(X = df, FUN = .r_type_label, FUN.VALUE = character(1L))
  preview <- df[seq_len(min(preview_rows, nrow(df))), , drop = FALSE]
  hint <- .build_hint(table = table, df = df)
  ms <- .detect_multi_series(con = con, table = table)

  .format_envelope(
    table = table,
    n_rows = n_rows,
    df_preview = preview,
    columns = columns,
    hint = hint,
    multi_series_lines = ms
  )
}


#' Execute a SQL query against the session DuckDB
#'
#' Returns CSV (consistent with the rest of the package). Caps the row
#' count at `max_rows` and appends a truncation notice so the LLM knows
#' when its result is partial. DuckDB errors are caught and returned as
#' `"# error: <msg>"` text so the LLM can read them and retry without
#' the JSON-RPC layer collapsing.
#'
#' @param sql DuckDB-dialect SQL. Any statement DuckDB accepts is fine.
#' @param max_rows Row cap for the returned CSV (default 100).
#'
#' @return CSV text, possibly with a `# truncated:` footer or `# error:` line.
#'
#' @importFrom DBI dbGetQuery
#' @importFrom utils write.csv capture.output
#'
#' @noRd
db_query <- function(sql, max_rows = 100L) {
  con <- db_con()
  result <- tryCatch(
    expr  = dbGetQuery(conn = con, statement = sql),
    error = \(e) e
  )
  if (inherits(x = result, what = "error")) {
    return(paste0("# error: ", conditionMessage(c = result)))
  }
  if (!is.data.frame(result) || nrow(result) == 0L) return("(no rows)")
  total_rows <- nrow(result)
  truncated  <- total_rows > max_rows
  if (truncated) result <- result[seq_len(max_rows), , drop = FALSE]
  out <- write.csv(x = result, file = stdout(), row.names = FALSE) |>
    capture.output() |>
    paste(collapse = "\n")
  if (truncated) {
    out <- paste0(
      out,
      "\n# truncated: showing ", max_rows, " of ", total_rows, " rows"
    )
  }
  out
}


#' Enumerate user tables in the session DuckDB
#'
#' Returns one row per cached table with row count and a compact schema
#' string. Internal DuckDB system tables are excluded.
#'
#' @return CSV with columns `table_name`, `rows`, `columns`.
#'
#' @importFrom DBI dbListTables dbGetQuery dbListFields
#' @importFrom utils capture.output write.csv
#'
#' @noRd
db_list <- function() {
  con  <- db_con()
  tabs <- dbListTables(conn = con)
  if (length(tabs) == 0L) return("(no cached tables)")
  rows_per <- vapply(
    X = tabs,
    FUN = \(t) {
      res <- dbGetQuery(
        conn = con,
        statement = sprintf(fmt = "SELECT COUNT(*) AS n FROM %s", t)
      )
      res$n
    },
    FUN.VALUE = numeric(1L)
  )
  cols_per <- vapply(
    X = tabs,
    FUN = \(t) {
      fields <- dbListFields(conn = con, name = t)
      paste(fields, collapse = ";")
    },
    FUN.VALUE = character(1L)
  )
  df <- data.frame(
    table_name = tabs,
    rows = as.integer(rows_per),
    columns = cols_per
  )
  write.csv(x = df, file = stdout(), row.names = FALSE) |>
    capture.output() |>
    paste(collapse = "\n")
}


#' Describe one cached table: schema + row count + sample rows
#'
#' @param name Table name (as returned by `db_store()` or `db_list()`).
#' @param n_preview Number of sample rows to include.
#'
#' @return The same envelope text shape as `db_store()`.
#'
#' @importFrom DBI dbExistsTable dbGetQuery
#'
#' @noRd
db_describe <- function(name, n_preview = 5L) {
  con <- db_con()
  if (!dbExistsTable(conn = con, name = name)) {
    return(paste0("# error: table '", name, "' not found"))
  }
  preview <- dbGetQuery(
    conn = con,
    statement = sprintf(fmt = "SELECT * FROM %s LIMIT %d", name, n_preview)
  )
  n_rows <- dbGetQuery(
    conn = con,
    statement = sprintf(fmt = "SELECT COUNT(*) AS n FROM %s", name)
  )$n
  columns <- vapply(X = preview, FUN = .r_type_label, FUN.VALUE = character(1L))
  hint <- .build_hint(table = name, df = preview)
  ms <- .detect_multi_series(con = con, table = name)

  .format_envelope(
    table = name,
    n_rows = n_rows,
    df_preview = preview,
    columns = columns,
    hint = hint,
    multi_series_lines = ms
  )
}
