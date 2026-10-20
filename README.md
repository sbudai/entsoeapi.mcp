
<!-- README.md is generated from README.Rmd. Please edit that file -->

# entsoeapi.mcp

<!-- badges: start -->

![Tests](https://img.shields.io/badge/tests-121%20passing-brightgreen)
![Coverage](https://img.shields.io/badge/coverage-99%25-brightgreen)
![Tools](https://img.shields.io/badge/tools-29-blue)
![R](https://img.shields.io/badge/R-%E2%89%A5%204.2.0-blue)
![License](https://img.shields.io/badge/license-MIT-blue)

<!-- badges: end -->

An [MCP](https://modelcontextprotocol.io) server that exposes the
[ENTSO-E Transparency Platform](https://transparency.entsoe.eu) as tools
for LLM-enabled applications such as Claude Desktop. Query European
electricity market, load, generation, transmission, outages, and
balancing data through natural language.

Built in pure R on top of
[`entsoeapi`](https://github.com/krose/entsoeapi) and
[`mcptools`](https://github.com/posit-dev/mcptools). Distributed as a
Docker image — no R installation required for end users.

Large time-series results are cached in an in-process **DuckDB** inside
the container, so the LLM can aggregate, filter, and join via SQL
without flooding its context window.

------------------------------------------------------------------------

## What you can ask

> *“What was the actual electricity load in Germany on 3 January
> 2025?”*  
> *“Show me day-ahead prices for France and Germany last Monday.”*  
> *“Which generation units in Belgium had unplanned outages this
> week?”*  
> *“What is the installed solar capacity in the Netherlands in 2024?”*  
> *“Get the net transfer capacity from Germany to Poland for today.”*

------------------------------------------------------------------------

## Prerequisites

| Requirement | Purpose | Notes |
|----|----|----|
| [Docker](https://docs.docker.com/get-docker/) | Runs the server container | Any version that supports `docker run`. |
| [Claude Desktop](https://claude.com/download) | MCP client | or any MCP-compatible client |
| [ENTSO-E API token](https://transparency.entsoe.eu/usrm/user/createPublicUser) | Access to the Transparency Platform | Free registration; UUID v4 format. |

------------------------------------------------------------------------

## Quick start

### Step 1 — Get an ENTSO-E API token

1.  Register at
    [transparency.entsoe.eu](https://transparency.entsoe.eu/usrm/user/createPublicUser)
2.  After logging in, go to **My Account Settings → Web API Security
    Token**
3.  Generate a token — it looks like
    [`xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx`](https://en.wikipedia.org/wiki/Universally_unique_identifier)

### Step 2 — Pull the Docker image using CLI

``` bash
docker pull sbudai/entsoeapi.mcp:latest
```

Or build from source (see [Building from source](#building-from-source)
below).

### Step 3 — Configure MCP server settings of Claude Desktop

Open (or create) your Claude Desktop config file:

- **macOS / Linux**: `~/.config/Claude/claude_desktop_config.json`
- **Windows**: `%APPDATA%\Claude\claude_desktop_config.json`

Add the `entsoe` entry under `"mcpServers"`:

``` json
{
  "mcpServers": {
    "entsoe": {
      "command": "docker",
      "args": ["run", "--rm", "-i", "-e", "ENTSOE_PAT", "-v", "entsoeapi-cache:/home/mcp/.cache", "sbudai/entsoeapi.mcp:latest"],
      "env": {
        "ENTSOE_PAT": "your-entsoe-api-token-here"
      }
    }
  }
}
```

Key flags:

- `--rm` — removes the container when the session ends (no container
  accumulation)
- `-i` — keeps stdin open, required for MCP stdio transport
- `-e ENTSOE_PAT` — forwards the token from Claude Desktop into the
  container
- `-v entsoeapi-cache:/home/mcp/.cache` — optional; keeps the downloaded
  EIC code tables in a named volume for a day, so the EIC lookup tools
  answer instantly in later sessions instead of re-downloading them

### Step 4 — Restart Claude Desktop and verify

Restart Claude Desktop to load the new MCP server configuration. You
should see **entsoe** listed under the tools panel.

Test with a simple query:

> *“What is the EIC code for Germany?”*

Claude should call the `area_eic` tool and return a list of German
bidding zones and control areas.

------------------------------------------------------------------------

## Available tools (29)

The 21 time-series tools return a compact envelope pointing at a cached
DuckDB table (`{table, rows, schema, preview, SQL hint}`). Use
`sql_query` to aggregate or join. EIC lookup tools and `get_news` return
CSV directly. All date parameters use **CET timezone** and **YYYY-MM-DD
format**.

### EIC lookup

| Tool | Description |
|----|----|
| `area_eic` | Bidding zones, control areas, member states — look up EIC codes by country name |
| `party_eic` | Market participants (TSOs, traders, etc.) |
| `all_approved_eic` | All approved EIC codes across all types |
| `resource_object_eic` | Generation units and other resource objects |
| `get_news` | Latest news and maintenance notices from ENTSO-E |

### Load

One tool with a `type` parameter:

| `type` value         | ENTSO-E dataset | Description                           |
|----------------------|-----------------|---------------------------------------|
| `actual` *(default)* | 6.1.A           | Actual total load                     |
| `day_ahead`          | 6.1.B           | Day-ahead total load forecast         |
| `week_ahead`         | 6.1.C           | Week-ahead total load forecast        |
| `month_ahead`        | 6.1.D           | Month-ahead total load forecast       |
| `year_ahead`         | 6.1.E           | Year-ahead total load forecast        |
| `margin`             | 8.1             | Year-ahead generation forecast margin |

### Generation

One tool (`gen_time_series`) with a `type` parameter:

| `type` value | ENTSO-E dataset | Description |
|----|----|----|
| `actual` *(default)* | 16.1.B&C | Actual generation per production type |
| `wind_solar` | 14.1.D | Day-ahead wind and solar forecast |
| `day_ahead` | 14.1.C | Day-ahead aggregated generation forecast |
| `per_unit` | 16.1.A | Actual generation per generation unit *(slow)* |
| `storage` | 16.1.D | Hydro reservoir / water storage filling rate |

### Generation — installed capacity

One tool (`gen_capacity`) with a `per_unit` parameter:

| `per_unit` value    | ENTSO-E dataset | Description                      |
|---------------------|-----------------|----------------------------------|
| `FALSE` *(default)* | 14.1.A          | aggregate by production type     |
| `TRUE`              | 14.1.C          | individual production unit level |

### Market

| Tool | ENTSO-E dataset | Description |
|----|----|----|
| `energy_prices` | 12.1.D | Day-ahead / intraday market clearing prices |
| `intraday_prices` | 12.1.D | intraday market prices |
| `net_transfer_capacities` | 11.1 | NTC between two bidding zones |
| `day_ahead_commercial_sched` | 12.1.F | Day-ahead commercial schedules |
| `explicit_offered_transfer_capacities` | 12.1.A | Explicit offered transfer capacities |
| `flow_based_allocations` | 12.1.B | Flow-based allocations for CWE/Core region |
| `congestion_income` | 12.1.G | Congestion income |
| `allocated_transfer_capacities_3rd_countries` | 12.1.C | Allocated transfer capacities on third-country borders |

### Transmission

| Tool | ENTSO-E dataset | Description |
|----|----|----|
| `cross_border_physical_flows` | 12.1.G | Physical cross-border flows |
| `total_commercial_sched` | 12.1.F | Total commercial schedules |
| `net_positions` | 12.1.H | Net positions |
| `forecasted_transfer_capacities` | 11.1 | Forecasted transfer capacities |

### Outages

| Tool | ENTSO-E dataset | Description |
|----|----|----|
| `outages_gen_units` | 15.1.A&B | Unavailability of generation units |
| `outages_prod_units` | 15.1.C&D | Unavailability of production units |
| `outages_transmission_grid` | 15.1.A&B | Unavailability in the transmission grid |

### Balancing

| Tool                  | ENTSO-E dataset | Description                   |
|-----------------------|-----------------|-------------------------------|
| `imbalance_prices`    | 17.1.D          | Imbalance prices              |
| `imbalance_volumes`   | 17.1.C          | Imbalance volumes             |
| `contracted_reserves` | 17.1.B          | Contracted balancing reserves |

### Session DuckDB cache

| Tool | Description |
|----|----|
| `sql_query` | Run a SQL query (DuckDB dialect) against cached tables. Returns CSV, capped at `max_rows` (default 100). Raise the cap for plotting (e.g. 800 for monthly hourly data); avoid values above 5000. |
| `list_tables` | List every cached table in this session with row counts and column schemas. |
| `describe_table` | Schema + row count + N sample rows for one named table. |

Every time-series tool inserts its full result into an in-memory DuckDB
table and returns an envelope. The envelope tells Claude — in every call
— that statistics must be computed via `sql_query`, never from the
schema sample, and warns when several `TimeSeries` are stacked together
in one cached table. This is the real envelope for Austrian day-ahead
prices on 12 October 2026: one day of 15-minute prices, but 288 rows,
because the document holds classification sequence 2, sequence 1, and
sequence 1 again under a third `ts_mrid`:

    # table: energy_prices_2c603b
    # rows: 288
    # columns: ts_in_domain_name VARCHAR, ts_out_domain_name VARCHAR, type VARCHAR, type_def VARCHAR, market_agreement_type VARCHAR, market_agreement_type_def VARCHAR, ts_auction_type VARCHAR, ts_auction_type_def VARCHAR, ts_business_type VARCHAR, ts_business_type_def VARCHAR, ts_time_interval_start TIMESTAMP, ts_time_interval_end TIMESTAMP, ts_mrid DOUBLE, ts_point_dt_start TIMESTAMP, ts_point_dt_start_cet TIMESTAMP, ts_point_price_amount DOUBLE, ts_currency_unit_name VARCHAR, ts_price_measure_unit_name VARCHAR, ts_classification_sequence_position DOUBLE
    # ------------------------------------------------------------
    # IMPORTANT - read before using this result:
    #   This is a CACHED TABLE, not a result set. The rows below
    #   are a SCHEMA SAMPLE only - they are NOT representative of
    #   the data, may omit other series, and MUST NOT be used for
    #   any analysis. NEVER compute averages, totals, min/max,
    #   counts, or any statistic from this sample.
    #   ALWAYS call sql_query(...) against the table named above
    #   for ANY aggregation, filter, statistic, or join.
    # ------------------------------------------------------------
    # time: ts_point_dt_start is UTC; ts_point_dt_start_cet is the same
    #   instant in Central European wall-clock time (CET/CEST, DST
    #   handled). Group by it for local days and hours; do not shift
    #   by a fixed INTERVAL.
    # WARNING: multiple series present in this table:
    #   ts_classification_sequence_position: 1, 2     (filter explicitly; do not assume one)
    #   ts_mrid: 1, 2, 3     (filter explicitly; do not assume one)
    # schema sample (5 of 288 rows; do not use for analysis):
    "ts_in_domain_name","ts_out_domain_name","type","type_def","market_agreement_type","market_agreement_type_def","ts_auction_type","ts_auction_type_def","ts_business_type","ts_business_type_def","ts_time_interval_start","ts_time_interval_end","ts_mrid","ts_point_dt_start","ts_point_dt_start_cet","ts_point_price_amount","ts_currency_unit_name","ts_price_measure_unit_name","ts_classification_sequence_position"
    "Austria","Austria","A44","Price Document","A01","Daily contract","A01","Implicit","A62","Spot price",2026-10-11 22:00:00,2026-10-12 22:00:00,1,2026-10-11 22:00:00,2026-10-12 00:00:00,195.36,"EUR","MWH",2
    "Austria","Austria","A44","Price Document","A01","Daily contract","A01","Implicit","A62","Spot price",2026-10-11 22:00:00,2026-10-12 22:00:00,1,2026-10-11 22:15:00,2026-10-12 00:15:00,190.12,"EUR","MWH",2
    "Austria","Austria","A44","Price Document","A01","Daily contract","A01","Implicit","A62","Spot price",2026-10-11 22:00:00,2026-10-12 22:00:00,1,2026-10-11 22:30:00,2026-10-12 00:30:00,178.49,"EUR","MWH",2
    "Austria","Austria","A44","Price Document","A01","Daily contract","A01","Implicit","A62","Spot price",2026-10-11 22:00:00,2026-10-12 22:00:00,1,2026-10-11 22:45:00,2026-10-12 00:45:00,178.24,"EUR","MWH",2
    "Austria","Austria","A44","Price Document","A01","Daily contract","A01","Implicit","A62","Spot price",2026-10-11 22:00:00,2026-10-12 22:00:00,1,2026-10-11 23:00:00,2026-10-12 01:00:00,190,"EUR","MWH",2
    # suggested query: SELECT date_trunc('week', ts_point_dt_start_cet) AS wk, AVG(ts_point_price_amount) FROM energy_prices_2c603b GROUP BY 1 ORDER BY 1

Note that `ts_point_dt_start` is UTC (22:00 UTC is midnight in Vienna in
summer), while `ts_point_dt_start_cet` holds the same instant in Central
European wall-clock time, with daylight saving applied. The server
computes it in R because the sandboxed DuckDB cannot load its time-zone
extension, and a fixed `+ INTERVAL 2 HOUR` shift is wrong on changeover
days. The sample rows show only sequence 2, so Claude would filter
first,
e.g. `WHERE ts_classification_sequence_position = 1 AND ts_mrid = 2`,
then group by `ts_point_dt_start_cet`.

The cache lives for the lifetime of one Claude Desktop session (the
`docker run --rm -i` container). Identical queries within a session
reuse the same table — no second API hit.

The database is sandboxed: SQL cannot read or write files, reach the
network, install extensions, or change settings, and its memory is
capped at 1 GB (override with `-e ENTSOEAPI_MCP_DB_MEMORY=2GB`).

**Example workflow** Claude would run for *“Weekly average German load
for January 2024”*:

1.  `area_eic(query = "Germany")` — resolve EIC code
2.  `load(eic = "10Y1001A1001A83F", period_start = "2024-01-01", period_end = "2024-02-01")`
    — envelope with `table = load_actual_<hash>`
3.  `sql_query(sql = "SELECT date_trunc('week', ts_point_dt_start_cet) AS wk, AVG(ts_point_quantity)::INT AS avg_mw FROM load_actual_<hash> GROUP BY 1 ORDER BY 1")`
    — 5 weekly rows instead of 2,976 quarter-hourly rows

------------------------------------------------------------------------

## Tips for good results

- **Always call `area_eic` first** to resolve a country name to its EIC
  code before querying data. Many countries have multiple bidding zones
  (e.g. Norway has NO1–NO5).
- **Fetch generously, aggregate via SQL.** The 100-row cap only applies
  to `sql_query` output — the cached DuckDB table holds the full result.
  Fetch a month or year of data once, then aggregate down to weekly /
  daily values via `sql_query(...)`.
- **For generation time-series**, omit `gen_type` to get all production
  types, or filter with codes like `B16` (Solar) or `B18` (Wind
  offshore).
- **Never accept synthetic values.** Every data-fetching tool explicitly
  instructs the LLM not to generate synthetic data when a live ENTSO-E
  tool is available. If Claude produces values without calling a tool,
  ask it to retry using the appropriate tool.

The server also sends these rules to the client as its MCP
`instructions` when a session starts, so the model sees the workflow
(EIC first, fetch once, aggregate with `sql_query`) before its first
tool call.

## Agent skill

[`inst/skills/entsoe-market-data/SKILL.md`](inst/skills/entsoe-market-data/SKILL.md)
is an [Agent Skill](https://agentskills.io) that teaches an agent when
and how to use this server: picking the right EIC per dataset, reading
the cache envelope, writing the `sql_query` step, and the common
pitfalls (several series in one table, UTC versus local timestamps). It
needs the MCP server above to be connected. To use it with Claude Code,
copy the folder into `~/.claude/skills/` (or a project’s
`.claude/skills/`); in Claude Desktop, upload it as a zip under
**Settings → Customize → Skills**. In an R installation it is at
`system.file("skills", "entsoe-market-data", package = "entsoeapi.mcp")`.

## Data notes

- **Timezone**: All dates are interpreted as CET (Central European Time;
  UTC+1 / UTC+2 in summer) regardless of your local timezone. Cached
  tables keep point timestamps in UTC (`ts_point_dt_start`) and add a
  Central European wall-clock copy (`ts_point_dt_start_cet`) for
  grouping by local day or hour.
- **Resolution**: Load and generation data is typically at 15-minute or
  1-hour intervals
- **Row cap**: The 21 time-series tools store their **full result** in
  DuckDB — no row cap at the fetch stage. The cap applies to `sql_query`
  output (default 100 rows) to keep aggregated results compact; raise
  `max_rows` explicitly when you need raw rows for plotting (e.g. 800
  for a month of hourly data). A truncation notice is appended when
  output is cut.
- **Max range**: Most time-series tools accept up to 1 year per query
- **EIC codes**: Use `area_eic` to resolve country or zone names to EIC
  codes before querying data

------------------------------------------------------------------------

## Alternative: native R (no Docker)

If you already have R ≥ 4.2.0 installed:

``` bash
# Install dependencies
Rscript -e "install.packages('pak'); pak::pkg_install(c('krose/entsoeapi', 'posit-dev/mcptools', 'ellmer', 'lubridate', 'jsonlite'))"

# Install this package
R CMD INSTALL /path/to/entsoeapi.mcp
```

Then in `claude_desktop_config.json`:

``` json
{
  "mcpServers": {
    "entsoe": {
      "command": "Rscript",
      "args": ["-e", "entsoeapi.mcp::run()"],
      "env": {
        "ENTSOE_PAT": "your-entsoe-api-token-here"
      }
    }
  }
}
```

------------------------------------------------------------------------

## Building from source

``` bash
git clone https://github.com/sbudai/entsoeapi.mcp.git
cd entsoeapi.mcp

# Build the Docker image
docker build -t sbudai/entsoeapi.mcp:latest .

# Verify the server starts and responds to MCP initialize
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test","version":"0"}}}' \
  | docker run --rm -i -e ENTSOE_PAT=test sbudai/entsoeapi.mcp:latest
```

The image is built in two stages:
[`rocker/r-ver`](https://rocker-project.org) installs the pre-built
Linux binaries from [Posit Public Package
Manager](https://packagemanager.posit.co), then R and the packages are
copied onto a plain `ubuntu:noble` together with only the shared
libraries they link against (~230 MB instead of ~1.8 GB). It runs as a
non-root `mcp` user. Override the R version with
`--build-arg R_VERSION=4.6.1`.

------------------------------------------------------------------------

## Testing

The package ships with **121 unit tests** that exercise every tool
wrapper, every dispatch branch, and every utility — yielding **99% line
coverage** across `R/run.R`, `R/tools.R`, and `R/utils.R`.

Run the suite locally:

``` r
# Plain run (fast)
pkgload::load_all()
testthat::test_dir("tests/testthat")

# With coverage report
covr::package_coverage() |> covr::report()
```

Tests are network-free: every `entsoeapi::*` call is mocked at the
package’s *imports environment* via a small `.with_imp_bindings()`
helper, and `safe_to_csv()` is mocked in the namespace via
`testthat::with_mocked_bindings(.package = "entsoeapi.mcp")`. This lets
the suite run in CI without an `ENTSOE_PAT` token and without hitting
the Transparency Platform.

------------------------------------------------------------------------

## Repository structure

    entsoeapi.mcp/
    ├── Dockerfile            # Self-contained runtime image
    ├── DESCRIPTION           # Package metadata and dependencies
    ├── NAMESPACE             # Exported functions (generated by roxygen2)
    ├── LICENSE               # MIT
    ├── NEWS.md               # What has changed in each version
    ├── README.md             # GitHub standard version of README.Rmd
    ├── README.Rmd            # Comprehensive introduction of the project
    ├── RELEASE_CHECKLIST     # What to check during launching a new version
    ├── R/
    |   ├── run.R             # Entry point: run() → mcp_server(tools = all_tools()), server instructions
    |   ├── tools.R           # All 29 ellmer::tool() definitions
    |   ├── utils.R           # parse_date(), slim_ts(), safe_to_csv(), safe_to_cache()
    |   └── db.R              # In-memory DuckDB singleton + sql_query / list / describe helpers
    ├── tests
    |   ├── testthat.R        # Test framework helper
    |   └── testthat
    |       ├── test-run.R    # Unit tests of run.R (16 tests)
    |       ├── test-tools.R  # Unit tests of tools.R (51 tests)
    |       ├── test-utils.R  # Unit tests of utils.R (20 tests)
    |       └── test-db.R     # Unit tests of db.R (34 tests)
    ├── inst
    |   └── skills
    |       └── entsoe-market-data
    |           └── SKILL.md  # Agent Skill: how an agent should use this server
    ├── man
    |   └── run.Rd            # Documentation of functions in R/run.R
    └── .github/
        ├── pull_request_template.md
        └── ISSUE_TEMPLATE
           ├── bug_report.md
           └── feature_request.md

### Token budget design

Tool results from the 21 time-series tools are inserted into an
in-process **DuckDB** (in-memory, per session) and the LLM gets a
compact envelope back — table name, row count, column schema, a 5-row
preview, and an SQL hint. The LLM then issues SQL via `sql_query` to
aggregate, filter, or join inside the container, so only the
*aggregated* result ever reaches the prompt context. `slim_ts()` still
drops constant metadata columns before insertion to save memory and
preview tokens. The 5 EIC lookup tools and `get_news` keep their
direct-CSV path because they’re small and interactive.

------------------------------------------------------------------------

## Requirements

- R ≥ 4.2.0
- [`entsoeapi`](https://github.com/krose/entsoeapi) (GitHub)
- [`mcptools`](https://github.com/posit-dev/mcptools) (GitHub)
- `ellmer`, `lubridate`, `jsonlite`, `duckdb`, `DBI`, `rlang` (CRAN)

------------------------------------------------------------------------

## License

MIT © Sándor Budai. See [LICENSE](LICENSE).

The underlying data is provided by [ENTSO-E](https://www.entsoe.eu)
under their [terms of
use](https://transparencyplatform.zendesk.com/hc/en-us/articles/40921911218961-Legal-Terms-and-Conditions).
