---
editor_options: 
  markdown: 
    wrap: 80
---

# entsoeapi.mcp 0.3.1

## Bug fix

- **Tool error resilience**: API errors (invalid EIC code, missing token, HTTP
  failures) previously propagated as unhandled R exceptions, causing Claude
  Desktop to show "Failed to call tool". The `mute_stdout_tool()` wrapper now
  catches these errors via `tryCatch` and returns a `# error: <message>` text
  string so the LLM can read the message and self-correct (e.g. by calling
  `area_eic` first to obtain a valid EIC code).

## LLM guidance improvements

- **`sql_querymax_rows` guidance**: the tool description now explains when to
  raise `max_rows` beyond the 100-row default — e.g. `max_rows = 800` for one
  month of hourly data, `max_rows = 3000` for 15-min data — while recommending
  that values above 5000 be avoided to prevent filling the context window. The
  `max_rows` argument description notes that the 100-row default is intentional:
  it encourages `GROUP BY` aggregation inside DuckDB rather than bulk dumps
  through the JSON-RPC pipe.

## Internal

- Completed roxygen2 `@param` descriptions for all internal helper functions
  that previously had bare (empty) `@param` tags: `.eic_filter()`,
  `.format_envelope()`, `.detect_multi_series()`, `.build_hint()`.

## Compatibility

- No tool signatures, return types, tool count, or runtime dependencies change.

# entsoeapi.mcp 0.3.0

## LLM-usability fixes for the DuckDB cache

- **Envelope rewrite**: every cached-table envelope now leads with an explicit
  directive — never compute statistics from the schema sample; always use
  `sql_query` against the named table. The block previously labelled
  `# preview (first N rows)` is now
  `# schema sample (N of M rows; do not use for analysis)` so the LLM does not
  mistake it for the result.

- **Multi-series detection**: when a cached table contains more than one
  distinct value in any of the ENTSO-E series-discriminator columns (`sequence`,
  `business_type`, `process_type`, `time_series_mrid`, `auction_type`, etc.),
  the envelope now carries a `# WARNING: multiple series present` line listing
  the values. The LLM is instructed to filter explicitly before aggregating.

- **EIC disambiguation**: `area_eic` description now requires the LLM to present
  every match with its `area_type_code` and ask the user when a country resolves
  to multiple EICs (e.g. Germany: DE-LU bidding zone vs. DE control area).
  Prices → bidding zone, load → control area, flows → border.

- **Binding-sequence guidance**: `energy_prices` and `intraday_prices`
  descriptions now explain that Sequence 1 is normally the binding auction price
  and Sequence 2 is secondary/shadow, and require explicit filtering when both
  are present.

## Compatibility

- No tool signatures, return types, or arguments change. Tool count stays at 29.
  No new runtime dependencies.

# entsoeapi.mcp 0.2.0

## DuckDB-backed result cache

- New: in-process DuckDB (in-memory, per session) caches every time-series
  result. The 21 data tools now return a compact envelope
  `{table, rows, schema, 5-row preview, SQL hint}` instead of capped CSV.

- New tools: `sql_query`, `list_tables`, `describe_table`. Tool count: 26 → 29.

- Repeat calls with identical arguments reuse the existing cached table
  (content-addressed via a short md5 hash) — no second hit to the Transparency
  Platform within a session.

- `slim_ts()` still drops constant metadata columns before insertion to save
  memory and preview tokens.

## Compatibility

- **Behaviour change**: the 21 time-series tools no longer return a raw CSV
  string. Anything parsing the old format would break. EIC lookup tools
  (`area_eic`, `party_eic`, `all_approved_eic`, `resource_object_eic`) and
  `get_news` are unchanged — they still return CSV directly.

- New runtime dependencies: `duckdb`, `DBI`, `digest`.

# entsoeapi.mcp 0.1.0

## Initial release

- Provides a Model Context Protocol (MCP) server that exposes the `entsoeapi`
  package as 26 structured tools for LLM-enabled applications such as Claude
  Desktop.

- Tools cover all major ENTSO-E Transparency Platform domains:

  - **EIC lookup** – area, party, resource-object and all-approved EIC codes,
    and ENTSO-E news.
  - **Load** – actual, day-ahead, week-ahead, month-ahead, year-ahead, and
    margin load (unified `tool_load` with a `type` parameter).
  - **Generation** – time-series (`tool_gen_time_series`) and installed capacity
    (`tool_gen_capacity`), each merging several underlying API document types.
  - **Market** – day-ahead energy prices, intraday prices, net transfer
    capacities, day-ahead commercial schedules, explicit offered transfer
    capacities, flow-based allocations, congestion income, and allocated
    transfer capacities for third countries.
  - **Transmission** – cross-border physical flows, total commercial schedules,
    net positions, and forecasted transfer capacities.
  - **Outages** – generation units, production units, and transmission grid.
  - **Balancing** – imbalance prices, imbalance volumes, and contracted
    reserves.

## Key implementation details

- MCP server runs over stdin/stdout via `mcptools::mcp_server()`; the
  `ENTSOE_PAT` security token is read automatically from the environment.

- All tool results are serialised as CSV (instead of JSON), reducing
  context-window token usage by 60–70 % for wide time-series data frames.
  Results are capped at 100 rows with a truncation notice appended when data is
  partial.

- Uninformative constant columns (repeated domain names, currency codes, unit
  labels) are dropped from every result via an internal `slim_ts()` helper,
  further reducing payload size.

- HTTP response headers printed by `httr2::req_verbose()` are suppressed before
  they can corrupt the JSON-RPC stream on the MCP stdio transport.

- Tool argument descriptions steer the LLM toward short date ranges to avoid
  unnecessarily large API responses.
