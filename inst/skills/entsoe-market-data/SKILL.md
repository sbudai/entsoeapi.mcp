---
name: entsoe-market-data
description: Answer questions about European electricity markets with real ENTSO-E Transparency Platform data through the entsoeapi.mcp server - day-ahead and intraday prices, load, generation mix, installed capacity, cross-border flows, transfer capacities, outages and balancing. Use whenever the user asks about power prices, demand, generation or grid data for a European country or bidding zone, instead of web search or invented numbers. Requires the entsoeapi.mcp MCP server (tools such as area_eic, energy_prices, load, sql_query).
---

# ENTSO-E market data

The `entsoeapi.mcp` server exposes the ENTSO-E Transparency Platform as MCP
tools. This skill tells you how to use them: resolve the area, fetch once,
then answer from the session cache with SQL.

If none of its tools (`area_eic`, `energy_prices`, `sql_query`, ...) are
available, the server is not connected. Point the user to
<https://github.com/sbudai/entsoeapi.mcp#quick-start> instead of guessing
numbers. It needs Docker and a free ENTSO-E API token in `ENTSOE_PAT`.

## Workflow

### 1. Resolve the area to an EIC code

Every data tool takes an EIC code, not a country name. Call
`area_eic(query = "<country, ISO code or zone>")` first.

- Many countries have several EICs. Germany alone has the DE-LU bidding
  zone, a German control area and the historical DE-AT-LU zone.
- Pick the EIC whose area type matches the data:
  - **prices** -> bidding zone (BZN)
  - **load** -> control area (CTA)
  - **flows, transfer capacities, commercial schedules** -> a border, given
    as an importing `eic_in` and an exporting `eic_out`
- When more than one EIC fits, show the matches and let the user choose.
  Do not pick silently.

`party_eic`, `resource_object_eic` and `all_approved_eic` find market
participants, power plants and other objects the same way.

### 2. Fetch the data once

Dates are `YYYY-MM-DD` strings in CET. One request covers at most one year;
short ranges (a few days) are much faster, especially for 15-minute data and
for the `per_unit` generation and outage tools, which send one request per
day.

A data tool does not return the full data. It stores the result in an
in-memory DuckDB table for this session and returns an envelope:

```
# table: energy_prices_70171e
# rows: 96
# columns: ..., ts_point_dt_start TIMESTAMP, ts_point_dt_start_cet TIMESTAMP,
#   ts_point_price_amount DOUBLE, ...
...
# time: ts_point_dt_start is UTC; ts_point_dt_start_cet is Central European
#   wall-clock time (CET/CEST, DST handled). ...
...
# suggested query: SELECT ... FROM energy_prices_70171e ...
```

The few rows printed under the schema are a sample to show the column
shapes. **Never compute an answer from the sample rows.** Take column names
from the `# columns:` line; they differ between tools (prices use
`ts_point_price_amount`, other tools `ts_point_quantity` and the like).

### 3. Answer with SQL

Query the table named in the envelope with `sql_query` (DuckDB dialect):

```sql
SELECT date_trunc('day', ts_point_dt_start_cet) AS day,
       avg(ts_point_price_amount) AS avg_price,
       min(ts_point_price_amount) AS min_price,
       max(ts_point_price_amount) AS max_price
FROM energy_prices_70171e
GROUP BY 1 ORDER BY 1
```

- Results are capped at `max_rows` (default 100). Aggregate with `GROUP BY`;
  raise `max_rows` (up to about 5000) only when raw points are needed, for
  example to draw a chart.
- To combine areas or datasets, fetch each one, then `JOIN` the tables on
  the timestamp column.
- Calling a data tool again with the same arguments reuses the cached table.
  Use `list_tables` and `describe_table` to see what is already cached
  before fetching again.
- The database is sandboxed: no file or network access, no `SET`.

## Which tool for which question

| Question | Tool | Notes |
|---|---|---|
| Day-ahead / spot price | `energy_prices` | `contract_type = 'A01'` (default) |
| Intraday price | `intraday_prices` | |
| Demand, consumption, load forecast | `load` | `type`: `actual`, `day_ahead`, `week_ahead`, `month_ahead`, `year_ahead`, `margin` |
| Generation mix, renewables output | `gen_time_series` | `type`: `actual`, `wind_solar`, `day_ahead`, `per_unit`, `storage`; `gen_type` e.g. `B16` solar, `B19` wind onshore |
| Wind / solar forecast | `gen_time_series` | `type = 'wind_solar'`; `process_type`: `A01` day-ahead (default), `A40` intraday, `A18` current |
| Installed capacity | `gen_capacity` | takes a `year`, not a date range |
| Physical cross-border flow | `cross_border_physical_flows` | `eic_in`, `eic_out` |
| Transfer capacity (NTC, offered, forecast) | `net_transfer_capacities`, `explicit_offered_transfer_capacities`, `forecasted_transfer_capacities` | per border |
| Commercial schedules, net position | `day_ahead_commercial_sched`, `total_commercial_sched`, `net_positions` | |
| Flow-based market coupling | `flow_based_allocations` | Core / CWE region |
| Congestion income, third-country capacity | `congestion_income`, `allocated_transfer_capacities_3rd_countries` | |
| Redispatching, countertrading, congestion management costs | `congestion_management` | `type`: `redispatching_internal` (control-area `eic`), `redispatching_cross_border` and `countertrading` (`eic_in`, `eic_out`; try both directions), `costs` (monthly, currency in `ts_currency_unit_name`) |
| Plant or grid outages | `outages_gen_units`, `outages_prod_units`, `outages_transmission_grid` | |
| Imbalance price / volume, reserves | `imbalance_prices`, `imbalance_volumes`, `contracted_reserves` | |
| Platform announcements | `get_news` | |

Each tool's own description lists its remaining codes and options.

## Pitfalls

- **Several series in one table.** Price documents often hold more than one
  series. `ts_classification_sequence_position` 1 is the SDAC day-ahead
  price; 2 is a second auction, often at another resolution
  (`ts_resolution`). The same series can also repeat under a different
  `ts_mrid`, which doubles counts and sums. When the envelope warns about
  multiple series, filter on the columns it names (e.g.
  `WHERE ts_classification_sequence_position = 1 AND ts_mrid = 2`) or ask
  the user which series they mean before computing statistics.
- **Wrong area type.** Prices requested with a control-area EIC, or load with
  a bidding-zone EIC, come back empty or misleading. Go back to step 1.
- **`(no data)`** means ENTSO-E published nothing for that area and period;
  the API's reason follows it when there is one. Say so; do not fill the gap
  with estimates.
- **Errors** come back as text starting with `# error:`. Read the message,
  fix the arguments (date format, EIC, code value) and retry.
- **Time zone.** Input dates are CET, and `ts_point_dt_start` is UTC: a
  Budapest delivery day starts at 22:00 UTC in summer (CEST) and 23:00 UTC in
  winter. Group by `ts_point_dt_start_cet` instead, the same instant in
  Central European wall-clock time with daylight saving already applied.
  Do not add a fixed `INTERVAL`: it is wrong for part of every changeover day
  (last Sunday of March and of October). On the October changeover day the
  02:00-02:59 hour occurs twice (ENTSO-E calls them 3A and 3B), so that local
  day has 25 hours (100 quarter-hours) and `hour(ts_point_dt_start_cet) = 2`
  covers both. For EU areas outside CET the offset from the `_cet` column is
  constant all year: subtract 1 hour for GB, IE and PT; add 1 hour for FI,
  the Baltics, GR, RO and BG.

## Example

User: "What was the average day-ahead price in Hungary last week?"

1. `area_eic(query = "Hungary")` -> pick the HU bidding zone
   (`10YHU-MAVIR----U`).
2. `energy_prices(eic = "10YHU-MAVIR----U", period_start = "<Mon>",
   period_end = "<next Mon>")` -> envelope with table `energy_prices_<hash>`.
3. `sql_query("SELECT avg(ts_point_price_amount) FROM energy_prices_<hash>")`,
   filtering by series first if the envelope warned about several.
4. Report the value in EUR/MWh, with the date range and the data source
   (ENTSO-E Transparency Platform).
