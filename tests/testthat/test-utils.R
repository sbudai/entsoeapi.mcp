testthat::test_that(
  desc = "parse_date() converts YYYY-MM-DD strings to CET POSIXct",
  code = {
    result <- entsoeapi.mcp:::parse_date(ts = "2024-03-15")
    expect_s3_class(result, "POSIXct")
    testthat::expect_equal(
      object = format(result, "%Y-%m-%d"),
      expected = "2024-03-15"
    )
    testthat::expect_equal(
      object = attr(x = result, which = "tzone"),
      expected = "CET"
    )
  }
)

testthat::test_that(
  desc = "parse_date() passes POSIXct through unchanged",
  code = {
    ts <- as.POSIXct(x = "2024-03-15 12:00:00", tz = "UTC")
    expect_identical(
      object = entsoeapi.mcp:::parse_date(ts = ts),
      expected = ts
    )
  }
)

testthat::test_that(
  desc = "parse_date() handles start-of-year boundary",
  code = {
    result <- entsoeapi.mcp:::parse_date(ts = "2024-01-01")
    testthat::expect_equal(
      object = format(x = result, format = "%Y-%m-%d"),
      expected = "2024-01-01"
    )
  }
)


# ── slim_ts() ────────────────────────────────────────────────────────────────

testthat::test_that(
  desc = "slim_ts() drops columns where all non-NA values are identical",
  code = {
    df <- data.frame(
      datetime  = as.POSIXct(x = c("2024-01-01", "2024-01-02"), tz = "CET"),
      value     = c(100, 200),
      unit      = c("MW", "MW"), # constant — will be dropped
      # constant, but 'name' matches keep_pattern
      area_name = c("Germany", "Germany")
    )
    result <- entsoeapi.mcp:::slim_ts(df = df)
    testthat::expect_false(object = "unit" %in% names(result))
    testthat::expect_true(object = "area_name" %in% names(result))
    testthat::expect_true(object = "value" %in% names(result))
    testthat::expect_true(object = "datetime" %in% names(result))
  }
)

testthat::test_that(
  desc = "slim_ts() keeps columns whose name matches the keep_pattern",
  code = {
    df <- data.frame(
      eic_code = rep("10Y1001A1001A83F", 3),  # constant, but 'eic' matches
      price    = rep(50.0, 3),                # constant, but 'price' matches
      junk     = rep("X", 3)                  # constant, no match → dropped
    )
    result <- entsoeapi.mcp:::slim_ts(df = df)
    testthat::expect_true(object = "eic_code" %in% names(result))
    testthat::expect_true(object = "price" %in% names(result))
    testthat::expect_false(object = "junk" %in% names(result))
  }
)

testthat::test_that(
  desc = "slim_ts() returns empty data frame unchanged",
  code = {
    df <- data.frame(a = integer(0), b = character(0))
    testthat::expect_identical(
      object = entsoeapi.mcp:::slim_ts(df),
      expected = df
    )
  }
)

testthat::test_that(
  desc = "slim_ts() returns non-data-frame input unchanged",
  code = {
    x <- list(a = 1)
    testthat::expect_identical(
      object = entsoeapi.mcp:::slim_ts(x),
      expected = x
    )
  }
)

testthat::test_that(
  desc = "slim_ts() keeps varying columns regardless of name",
  code = {
    df <- data.frame(
      x = c(1, 2, 3),
      y = c("a", "a", "b")
    )
    result <- entsoeapi.mcp:::slim_ts(df = df)
    testthat::expect_true(object = all(c("x", "y") %in% names(result)))
  }
)


# ── safe_to_csv() ────────────────────────────────────────────────────────────

testthat::test_that(
  desc = "safe_to_csv() returns '(no data)' for NULL",
  code = {
    testthat::expect_equal(
      object = entsoeapi.mcp:::safe_to_csv(df = NULL),
      expected = "(no data)"
    )
  }
)

testthat::test_that(
  desc = "safe_to_csv() returns '(no data)' for zero-row data frame",
  code = {
    testthat::expect_equal(
      object = data.frame(a = integer(0)) |>
        entsoeapi.mcp:::safe_to_csv(),
      expected = "(no data)"
    )
  }
)

testthat::test_that(
  desc = "safe_to_csv() returns valid CSV with header row",
  code = {
    df <- data.frame(
      time = c("2024-01-01", "2024-01-02"),
      value = c(10L, 20L)
    )
    out <- entsoeapi.mcp:::safe_to_csv(df = df)
    lines <- strsplit(x = out, split = "\n")[[1]]
    testthat::expect_true(object = grepl(pattern = "time", x = lines[1]))
    testthat::expect_true(object = grepl(pattern = "value", x = lines[1]))
    # header + 2 data rows
    testthat::expect_equal(object = length(lines), expected = 3L)
  }
)

testthat::test_that(
  desc = "safe_to_csv() truncates to max_rows and appends notice",
  code = {
    df <- data.frame(x = seq_len(150L))
    out <- entsoeapi.mcp:::safe_to_csv(df = df, max_rows = 100L)
    lines <- strsplit(x = out, split = "\n")[[1]]
    # header + 100 data rows + 1 truncation comment = 102
    testthat::expect_equal(object = length(lines), expected = 102L)
    testthat::expect_true(object = grepl(pattern = "truncated", x = out))
    testthat::expect_true(object = grepl(pattern = "100 of 150", x = out))
  }
)

testthat::test_that(
  desc = paste(
    "safe_to_csv() does NOT append truncation notice",
    "when rows <= max_rows"
  ),
  code = {
    out <- data.frame(x = seq_len(50L)) |>
      entsoeapi.mcp:::safe_to_csv(max_rows = 100L)
    testthat::expect_false(object = grepl(pattern = "truncated", x = out))
  }
)

testthat::test_that(
  desc = "safe_to_csv() falls back to JSON for list input",
  code = {
    x <- list(a = 1, b = "two")
    out <- entsoeapi.mcp:::safe_to_csv(df = x)
    # JSON output should be parseable
    parsed <- jsonlite::fromJSON(txt = out)
    testthat::expect_equal(object = parsed$a, expected = 1)
    testthat::expect_equal(object = parsed$b, expected = "two")
  }
)
