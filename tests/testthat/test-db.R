# Tests for R/db.R — the in-memory DuckDB session cache.
#
# All tests share the package-level singleton connection. To keep them
# isolated, each test that touches table state clears the DB on entry via
# .clear_db() (defined below). The connection itself is reused — tearing it
# down per test would be slow and would not match how the MCP server uses it
# in production (one connection, lifetime of the R session).

# Helper: drop every user table in the singleton DuckDB
.clear_db <- function() {
  con  <- entsoeapi.mcp:::db_con()
  tabs <- DBI::dbListTables(conn = con)
  for (t in tabs) {
    DBI::dbExecute(conn = con, statement = sprintf("DROP TABLE %s", t))
  }
}


# ── db_con() ──────────────────────────────────────────────────────────────────

testthat::test_that(
  desc = "db_con() returns a valid DuckDB connection",
  code = {
    con <- entsoeapi.mcp:::db_con()
    testthat::expect_true(object = DBI::dbIsValid(con))
    testthat::expect_s4_class(object = con, class = "duckdb_connection")
  }
)

testthat::test_that(
  desc = "db_con() is a singleton — two calls return the same connection",
  code = {
    con1 <- entsoeapi.mcp:::db_con()
    con2 <- entsoeapi.mcp:::db_con()
    testthat::expect_identical(object = con1, expected = con2)
  }
)


# ── db_table_name() ──────────────────────────────────────────────────────────

testthat::test_that(
  desc = "db_table_name() is deterministic for identical args",
  code = {
    a <- entsoeapi.mcp:::db_table_name(
      prefix    = "load_actual",
      args_list = list(eic = "10Y1001A1001A83F", ps = "2024-01-01")
    )
    b <- entsoeapi.mcp:::db_table_name(
      prefix    = "load_actual",
      args_list = list(eic = "10Y1001A1001A83F", ps = "2024-01-01")
    )
    testthat::expect_identical(object = a, expected = b)
  }
)

testthat::test_that(
  desc = "db_table_name() differs when any arg differs",
  code = {
    a <- entsoeapi.mcp:::db_table_name(
      prefix    = "load",
      args_list = list(eic = "DE", ps = "2024-01-01")
    )
    b <- entsoeapi.mcp:::db_table_name(
      prefix    = "load",
      args_list = list(eic = "FR", ps = "2024-01-01")
    )
    testthat::expect_false(object = identical(x = a, y = b))
  }
)

testthat::test_that(
  desc = "db_table_name() uses the human-readable prefix",
  code = {
    n <- entsoeapi.mcp:::db_table_name(
      prefix    = "my_prefix",
      args_list = list(x = 1)
    )
    testthat::expect_true(object = startsWith(x = n, prefix = "my_prefix_"))
  }
)


# ── db_store() ──────────────────────────────────────────────────────────────

testthat::test_that(
  desc = "db_store() inserts every row and reports the correct row count",
  code = {
    .clear_db()
    df <- data.frame(
      ts    = as.POSIXct(c("2024-01-01", "2024-01-02"), tz = "CET"),
      value = c(10.0, 20.0)
    )
    env <- entsoeapi.mcp:::db_store(
      df        = df,
      prefix    = "stored_test",
      args_list = list(seed = "1")
    )
    testthat::expect_true(object = grepl(pattern = "rows: 2", x = env))
    testthat::expect_true(object = grepl(pattern = "stored_test_", x = env))
  }
)

testthat::test_that(
  desc = "db_store() envelope contains preview rows from the data",
  code = {
    .clear_db()
    df <- data.frame(
      ts    = as.POSIXct(c("2024-01-01", "2024-01-02"), tz = "CET"),
      value = c(42L, 84L)
    )
    env <- entsoeapi.mcp:::db_store(
      df        = df,
      prefix    = "preview_test",
      args_list = list(seed = "2")
    )
    testthat::expect_true(object = grepl(pattern = "42", x = env))
    testthat::expect_true(object = grepl(pattern = "84", x = env))
    testthat::expect_true(object = grepl(
      pattern = "schema sample \\(",
      x       = env
    ))
  }
)

testthat::test_that(
  desc = "db_store() round-trips POSIXct as TIMESTAMP in the envelope label",
  code = {
    .clear_db()
    df <- data.frame(
      ts    = as.POSIXct(c("2024-01-01 00:00:00"), tz = "CET"),
      value = 1.0
    )
    env <- entsoeapi.mcp:::db_store(
      df        = df,
      prefix    = "ts_test",
      args_list = list(seed = "3")
    )
    testthat::expect_true(
      object = grepl(pattern = "ts TIMESTAMP", x = env)
    )
  }
)

testthat::test_that(
  desc = paste(
    "db_store() suggests a date_trunc SQL hint when a timestamp column exists"
  ),
  code = {
    .clear_db()
    df <- data.frame(
      dt_start = as.POSIXct(c("2024-01-01"), tz = "CET"),
      value    = 1.0
    )
    env <- entsoeapi.mcp:::db_store(
      df        = df,
      prefix    = "hint_test",
      args_list = list(seed = "4")
    )
    testthat::expect_true(object = grepl(pattern = "date_trunc", x = env))
    testthat::expect_true(object = grepl(pattern = "GROUP BY", x = env))
  }
)

testthat::test_that(
  desc = "db_store() with identical args reuses the existing table (cache hit)",
  code = {
    .clear_db()
    df <- data.frame(value = 1:5)
    args <- list(seed = "cache-hit")

    env1 <- entsoeapi.mcp:::db_store(
      df = df, prefix = "cache_test", args_list = args
    )
    env2 <- entsoeapi.mcp:::db_store(
      df = df, prefix = "cache_test", args_list = args
    )

    # Same args → same hash → same table name embedded in both envelopes
    name1 <- regmatches(
      x = env1,
      m = regexpr(pattern = "cache_test_[0-9a-f]+", text = env1)
    )
    name2 <- regmatches(
      x = env2,
      m = regexpr(pattern = "cache_test_[0-9a-f]+", text = env2)
    )
    testthat::expect_identical(object = name1, expected = name2)

    # And we only ever created one table
    con  <- entsoeapi.mcp:::db_con()
    tabs <- DBI::dbListTables(conn = con)
    testthat::expect_length(
      object = grep(pattern = "cache_test_", x = tabs), n = 1L
    )
  }
)


# ── db_query() ──────────────────────────────────────────────────────────────

testthat::test_that(
  desc = "db_query() returns CSV rows for a valid SELECT",
  code = {
    .clear_db()
    entsoeapi.mcp:::db_store(
      df        = data.frame(x = 1:3),
      prefix    = "q",
      args_list = list(seed = "select")
    )
    con <- entsoeapi.mcp:::db_con()
    tab <- grep(
      pattern = "^q_",
      x       = DBI::dbListTables(conn = con),
      value   = TRUE
    )[1L]
    out <- entsoeapi.mcp:::db_query(
      sql = sprintf("SELECT x FROM %s ORDER BY x", tab)
    )
    # write.csv quotes the header, so the row layout is:
    #   "x"\n1\n2\n3
    testthat::expect_true(object = grepl(pattern = "1\n2\n3", x = out))
    testthat::expect_true(object = grepl(pattern = "x",       x = out))
  }
)

testthat::test_that(
  desc = "db_query() caps rows at max_rows and appends a truncation notice",
  code = {
    .clear_db()
    entsoeapi.mcp:::db_store(
      df        = data.frame(x = 1:150),
      prefix    = "qt",
      args_list = list(seed = "trunc")
    )
    con <- entsoeapi.mcp:::db_con()
    tab <- grep(
      pattern = "^qt_",
      x       = DBI::dbListTables(conn = con),
      value   = TRUE
    )[1L]
    out <- entsoeapi.mcp:::db_query(
      sql      = sprintf("SELECT x FROM %s ORDER BY x", tab),
      max_rows = 100L
    )
    testthat::expect_true(object = grepl(pattern = "truncated", x = out))
    testthat::expect_true(object = grepl(pattern = "100 of 150", x = out))
  }
)

testthat::test_that(
  desc = "db_query() returns '# error:' string on invalid SQL (no exception)",
  code = {
    out <- entsoeapi.mcp:::db_query(sql = "SELECT * FROM no_such_table_xyz")
    testthat::expect_true(object = grepl(pattern = "^# error:", x = out))
  }
)

testthat::test_that(
  desc = "db_query() returns '(no rows)' for a SELECT with no results",
  code = {
    .clear_db()
    entsoeapi.mcp:::db_store(
      df        = data.frame(x = 1:3),
      prefix    = "qe",
      args_list = list(seed = "empty")
    )
    con <- entsoeapi.mcp:::db_con()
    tab <- grep(
      pattern = "^qe_",
      x       = DBI::dbListTables(conn = con),
      value   = TRUE
    )[1L]
    out <- entsoeapi.mcp:::db_query(
      sql = sprintf("SELECT x FROM %s WHERE x > 999", tab)
    )
    testthat::expect_identical(object = out, expected = "(no rows)")
  }
)


# ── db_list() ──────────────────────────────────────────────────────────────

testthat::test_that(
  desc = "db_list() returns '(no cached tables)' on an empty DB",
  code = {
    .clear_db()
    testthat::expect_identical(
      object   = entsoeapi.mcp:::db_list(),
      expected = "(no cached tables)"
    )
  }
)

testthat::test_that(
  desc = "db_list() enumerates user tables with row counts and schemas",
  code = {
    .clear_db()
    entsoeapi.mcp:::db_store(
      df        = data.frame(a = 1:3, b = 4:6),
      prefix    = "listed",
      args_list = list(seed = "list-1")
    )
    out <- entsoeapi.mcp:::db_list()
    testthat::expect_true(object = grepl(pattern = "listed_", x = out))
    testthat::expect_true(object = grepl(pattern = "a;b",     x = out))
    testthat::expect_true(object = grepl(pattern = "3",       x = out))
  }
)


# ── db_describe() ──────────────────────────────────────────────────────────

testthat::test_that(
  desc = "db_describe() returns an envelope for an existing table",
  code = {
    .clear_db()
    entsoeapi.mcp:::db_store(
      df        = data.frame(x = 1:4, y = letters[1:4]),
      prefix    = "desc",
      args_list = list(seed = "describe")
    )
    con <- entsoeapi.mcp:::db_con()
    tab <- grep(
      pattern = "^desc_",
      x       = DBI::dbListTables(conn = con),
      value   = TRUE
    )[1L]
    out <- entsoeapi.mcp:::db_describe(name = tab, n_preview = 2L)
    testthat::expect_true(object = grepl(pattern = "rows: 4",    x = out))
    testthat::expect_true(
      object = grepl(pattern = "schema sample \\(2 of 4", x = out)
    )
  }
)

testthat::test_that(
  desc = "db_describe() returns '# error:' for an unknown table",
  code = {
    out <- entsoeapi.mcp:::db_describe(name = "nope_xyz_123")
    testthat::expect_true(object = grepl(pattern = "^# error:", x = out))
  }
)


# ── safe_to_cache() end-to-end ─────────────────────────────────────────────

testthat::test_that(
  desc = "safe_to_cache() returns '(no data)' for NULL and empty data frames",
  code = {
    testthat::expect_identical(
      object   = entsoeapi.mcp:::safe_to_cache(
        df = NULL, prefix = "x", args_list = list()
      ),
      expected = "(no data)"
    )
    testthat::expect_identical(
      object   = entsoeapi.mcp:::safe_to_cache(
        df = data.frame(a = integer(0)), prefix = "x", args_list = list()
      ),
      expected = "(no data)"
    )
  }
)

testthat::test_that(
  desc = paste(
    "safe_to_cache() falls back to safe_to_csv() (JSON) for non-data-frame",
    "input"
  ),
  code = {
    out <- entsoeapi.mcp:::safe_to_cache(
      df        = list(a = 1, b = "two"),
      prefix    = "ignored",
      args_list = list()
    )
    parsed <- jsonlite::fromJSON(txt = out)
    testthat::expect_equal(object = parsed$a, expected = 1)
    testthat::expect_equal(object = parsed$b, expected = "two")
  }
)

# ── envelope v0.3.0: directive + WARNING block ─────────────────────────────

testthat::test_that(
  desc = "db_store() envelope leads with the 'NEVER compute' directive",
  code = {
    .clear_db()
    env <- entsoeapi.mcp:::db_store(
      df        = data.frame(value = 1:3),
      prefix    = "directive_test",
      args_list = list(seed = "directive")
    )
    testthat::expect_true(object = grepl(pattern = "NEVER compute", x = env))
    testthat::expect_true(object = grepl(
      pattern = "ALWAYS call sql_query",
      x       = env
    ))
  }
)

testthat::test_that(
  desc = "db_store() envelope uses 'schema sample' wording (not 'preview')",
  code = {
    .clear_db()
    env <- entsoeapi.mcp:::db_store(
      df        = data.frame(value = 1:3),
      prefix    = "rename_test",
      args_list = list(seed = "rename")
    )
    testthat::expect_true(object = grepl(pattern = "schema sample", x = env))
    # The literal old wording must not appear anywhere in the envelope.
    testthat::expect_false(object = grepl(
      pattern = "# preview \\(first",
      x       = env
    ))
  }
)

testthat::test_that(
  desc = paste(
    "db_store() emits multi-series WARNING when a 'sequence' column has > 1",
    "distinct value"
  ),
  code = {
    .clear_db()
    df <- data.frame(
      ts = as.POSIXct(c("2024-01-01", "2024-01-02", "2024-01-03"), tz = "CET"),
      value = c(10.0, 20.0, 30.0),
      sequence = c(1L, 2L, 1L)
    )
    env <- entsoeapi.mcp:::db_store(
      df        = df,
      prefix    = "ms_test",
      args_list = list(seed = "multi-series")
    )
    testthat::expect_true(object = grepl(
      pattern = "WARNING: multiple series present",
      x       = env
    ))
    # Both distinct sequence values should be listed.
    testthat::expect_true(object = grepl(pattern = "sequence:", x = env))
  }
)

testthat::test_that(
  desc = paste(
    "db_store() omits the WARNING block when no series-discriminator",
    "column exists"
  ),
  code = {
    .clear_db()
    env <- entsoeapi.mcp:::db_store(
      df        = data.frame(x = 1:3),
      prefix    = "no_ms_test",
      args_list = list(seed = "no-multi-series")
    )
    testthat::expect_false(object = grepl(pattern = "WARNING:", x = env))
  }
)


testthat::test_that(
  desc = "safe_to_cache() drops constant columns via slim_ts() before caching",
  code = {
    .clear_db()
    df <- data.frame(
      ts       = as.POSIXct(c("2024-01-01", "2024-01-02"), tz = "CET"),
      value    = c(1.0, 2.0),
      junk_col = c("X", "X") # constant; not in keep_pattern → dropped
    )
    env <- entsoeapi.mcp:::safe_to_cache(
      df        = df,
      prefix    = "slim_test",
      args_list = list(seed = "slim")
    )
    testthat::expect_false(object = grepl(pattern = "junk_col", x = env))
    testthat::expect_true(object  = grepl(pattern = "value",    x = env))
  }
)
