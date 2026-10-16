# MCP tools

The server exposes **29 tools**. The time-series tools store their full
result in a session DuckDB table and return a short summary that points
at it. The LLM then uses `sql_query` to aggregate, filter or join. The
EIC lookup tools and `get_news` return CSV directly. All dates are
`YYYY-MM-DD`, interpreted in CET.

The descriptions below are exactly what the LLM reads when it decides
which tool to call and how.

## EIC lookup and news

### `area_eic`

Return Area Y EIC codes (bidding zones, control areas, member states,
etc.). Pass a query string to filter by country name, ISO code, or zone
name (case-insensitive). IMPORTANT: Always display the full table of
matches to the user and ask them to confirm which EIC code to use before
querying data - many countries have multiple bidding zones. When the
query resolves to MULTIPLE EICs (e.g. Germany has at least three: DE-LU
bidding zone ‘10Y1001A1001A82H’, DE control area, and the historical
DE-AT-LU zone), you MUST present every match with its area_type_code and
ask the user which to use before calling any downstream tool. Day-ahead
and intraday PRICES are published per BIDDING ZONE (BZN); LOAD is
published per CONTROL AREA (CTA); CROSS-BORDER flows and transfer
capacities are published per BORDER. Pick the EIC whose area_type_code
matches the domain you need. Trigger phrases: ‘EIC code for Germany’,
‘what zone is Austria’, ‘find the bidding zone for France’, ‘look up EIC
for HU’.

| Argument | Required | Description |
|----|----|----|
| `query` | no | Optional filter string, e.g. ‘Germany’, ‘DE’, ‘France’, ‘Bidding Zone’. Case-insensitive match against display name, long name, and country code. |

### `party_eic`

Return Party X EIC codes for market participants (TSOs, traders, etc.).
Pass a query string to filter by name or country code.

| Argument | Required | Description |
|----|----|----|
| `query` | no | Optional filter string, e.g. ‘Germany’, ‘TSO’, ‘DE’. Case-insensitive. |

### `all_approved_eic`

Return all approved EIC codes across all types (Area, Party, Tie-line,
Resource Object, Substation, etc.) in one table. Slower than area_eic()
alone. Pass a query string to filter results.

| Argument | Required | Description |
|----|----|----|
| `query` | no | Optional filter string, e.g. ‘Germany’, ‘DE’, ‘Tie-line’. Case-insensitive. |

### `resource_object_eic`

Return Resource Object W EIC codes - generation units and other
resources. Pass a query string to filter by plant name, country code, or
fuel type.

| Argument | Required | Description |
|----|----|----|
| `query` | no | Optional filter string, e.g. ‘wind’, ‘DE’, ‘nuclear’. Case-insensitive. |

### `get_news`

Fetch the latest news items from the ENTSO-E Transparency Platform RSS
feed. Useful for checking maintenance windows or data availability
issues.

| Argument | Required | Description                                     |
|----------|----------|-------------------------------------------------|
| `n`      | no       | Number of news items to return. Defaults to 10. |

## Load

### `load`

Get real electricity consumption / demand data for a European country or
bidding zone (e.g. Germany, Austria, Hungary, France). Use for any
request about total load, power demand, electricity consumption, MW load
profile, or actual/forecast demand. Returns actual ENTSO-E data - do NOT
generate synthetic load values. Prefer 1-3 day date ranges - 15-min data
fills the 100-row cap in ~1 day; hourly data allows ~4 days. Max 1-year
API limit. type: ‘actual’ (6.1.A, default), ‘day_ahead’ (6.1.B),
‘week_ahead’ (6.1.C), ‘month_ahead’ (6.1.D), ‘year_ahead’ (6.1.E),
‘margin’ (8.1 year-ahead margin). Use area_eic() to find the EIC code.

| Argument | Required | Description |
|----|----|----|
| `eic` | yes | EIC code of the bidding zone, e.g. ‘10Y1001A1001A83F’ for Germany. |
| `period_start` | yes | Start date YYYY-MM-DD (CET). Keep to 1-7 days - data is at 15-min to 1-hour resolution, capped at 100 rows. |
| `period_end` | yes | End date YYYY-MM-DD (CET). |
| `type` | no | Load series: ‘actual’ (default), ‘day_ahead’, ‘week_ahead’, ‘month_ahead’, ‘year_ahead’, ‘margin’. |

## Generation

### `gen_time_series`

Get real electricity generation data by fuel type or technology for a
European bidding zone. Use for any request about actual generation,
power output by fuel type, fuel mix, renewable generation, solar output,
wind power output, generation breakdown, or electricity production by
technology (solar, wind, nuclear, hydro, gas, coal, biomass, etc.).
Returns actual ENTSO-E data - do NOT generate synthetic generation
values. Prefer 1-3 day date ranges - 15-min data fills the 100-row cap
in ~1 day; hourly data allows ~4 days. Max 1-year API limit. type:
‘actual’ (16.1.B&C per production type, default), ‘wind_solar’ (14.1.D
forecast), ‘day_ahead’ (14.1.C aggregate forecast), ‘per_unit’ (16.1.A
per unit; one API request per day, sent in parallel, so keep ranges
short), ‘storage’ (16.1.D hydro reservoir filling rate). gen_type:
optional ENTSO-E production type code used with ‘actual’/‘per_unit’,
e.g. ‘B01’ Biomass, ‘B16’ Solar. ‘actual’ and ‘per_unit’ are paginated
daily.

| Argument | Required | Description |
|----|----|----|
| `eic` | yes | EIC code of the bidding zone or control area. |
| `period_start` | yes | Start date YYYY-MM-DD (CET). Keep to 1-7 days - data is at 15-min to 1-hour resolution, capped at 100 rows. |
| `period_end` | yes | End date YYYY-MM-DD (CET). |
| `type` | no | Generation series: ‘actual’ (default), ‘wind_solar’, ‘day_ahead’, ‘per_unit’, ‘storage’. |
| `gen_type` | no | Optional ENTSO-E production type code, e.g. ‘B01’ Biomass, ‘B16’ Solar. Used with ‘actual’ and ‘per_unit’. |

### `gen_capacity`

Get real installed generation capacity (MW) for a European bidding zone.
Use for any request about installed capacity, nameplate capacity,
generation fleet, renewable capacity, nuclear capacity, or power plant
capacity by technology type. Returns actual ENTSO-E data - do NOT
generate synthetic capacity values. per_unit=FALSE (default): aggregate
by production type (14.1.A). per_unit=TRUE: individual production unit
level (14.1.C, more granular).

| Argument | Required | Description |
|----|----|----|
| `eic` | yes | EIC code of the bidding zone. |
| `year` | yes | Year as integer, e.g. 2024. |
| `per_unit` | no | FALSE (default) = per production type (14.1.A). TRUE = per production unit (14.1.C). |
| `psr_type` | no | Optional PSR type code, e.g. ‘B16’ for Solar PV. |

## Market

### `energy_prices`

Get real electricity market prices for European bidding zones such as
Germany (DE-LU), Austria (AT), Hungary (HU), France (FR), Italy (IT),
Czech Republic (CZ), Romania (RO), etc. Use for any request about power
prices, spot prices, day-ahead prices, SDAC prices, EPEX prices, EUR/MWh
electricity prices, or ENTSO-E market price data. Returns actual ENTSO-E
data - do NOT generate synthetic prices when this tool is available.
Technical: day-ahead or intraday market clearing prices (12.1.D). Max
1-year API limit. contract_type: ‘A01’ = Day ahead (default), ‘A07’ =
Intraday. Use area_eic() to find the EIC code. NOTE: ENTSO-E publishes
multiple TimeSeries (sequences) per auction document - typically
Sequence 1 = the BINDING auction price, Sequence 2 = secondary / SDAC
rerun / shadow series. The cached table may contain all sequences; if
the envelope WARNING reports multiple series, filter explicitly
(e.g. WHERE sequence = 1) before computing statistics, or ask the user
which series they meant.

| Argument | Required | Description |
|----|----|----|
| `eic` | yes | EIC code of the bidding zone. |
| `period_start` | yes | Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible - data is at 15-min to 1-hour resolution and results are capped at 100 rows. |
| `period_end` | yes | End date YYYY-MM-DD (CET). |
| `contract_type` | no | Contract type: ‘A01’ for Day ahead (default), ‘A07’ for Intraday. |

### `intraday_prices`

Get real continuous intraday electricity prices (XBID, ID prices) for a
European bidding zone. Use for any request about intraday market prices,
continuous trading prices, or ID auction results. Returns actual ENTSO-E
data - do NOT generate synthetic prices. Specialised wrapper around the
intraday contract type. Prefer 1-3 day date ranges (results capped at
100 rows). Max 1-year API limit. NOTE: ENTSO-E publishes multiple
TimeSeries (sequences) per auction document - typically Sequence 1 = the
BINDING auction price, Sequence 2 = secondary / shadow series. If the
envelope WARNING reports multiple series, filter explicitly (e.g. WHERE
sequence = 1) before computing statistics, or ask the user which series
they meant.

| Argument | Required | Description |
|----|----|----|
| `eic` | yes | EIC code of the bidding zone. |
| `period_start` | yes | Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible - data is at 15-min to 1-hour resolution and results are capped at 100 rows. |
| `period_end` | yes | End date YYYY-MM-DD (CET). |

### `net_transfer_capacities`

Get real Net Transfer Capacities (NTC) between two European bidding
zones. Use for any request about interconnection capacity, cross-border
transfer capacity, or import/export capacity between countries. Returns
actual ENTSO-E data - do NOT generate synthetic capacity values.
Technical: NTC between two bidding zones (11.1). Prefer 1-3 day date
ranges (results capped at 100 rows). contract_type: ‘A02’ = Day ahead
(default), ‘A03’ = Intraday, ‘A04’ = Week ahead. Use area_eic() to find
EIC codes for both zones.

| Argument | Required | Description |
|----|----|----|
| `eic_in` | yes | EIC code of the importing bidding zone. |
| `eic_out` | yes | EIC code of the exporting bidding zone. |
| `period_start` | yes | Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible - data is at 15-min to 1-hour resolution and results are capped at 100 rows. |
| `period_end` | yes | End date YYYY-MM-DD (CET). |
| `contract_type` | no | Contract type: ‘A02’ Day ahead (default), ‘A03’ Intraday, ‘A04’ Week ahead. |

### `day_ahead_commercial_sched`

Get real day-ahead commercial schedules (nominations) between two
European bidding zones. Use for requests about DACF schedules,
commercial nominations, or scheduled cross-border exchanges. Returns
actual ENTSO-E data - do NOT generate synthetic schedule values.
Technical: day-ahead commercial schedules (12.1.F). Prefer 1-3 day date
ranges (results capped at 100 rows). Max 1-year API limit. Use
area_eic() to find EIC codes.

| Argument | Required | Description |
|----|----|----|
| `eic_in` | yes | EIC code of the importing bidding zone. |
| `eic_out` | yes | EIC code of the exporting bidding zone. |
| `period_start` | yes | Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible - data is at 15-min to 1-hour resolution and results are capped at 100 rows. |
| `period_end` | yes | End date YYYY-MM-DD (CET). |

### `explicit_offered_transfer_capacities`

Get real explicit offered transfer capacities from long-term capacity
auctions (annual, monthly) between two European bidding zones. Use for
requests about explicit capacity auctions, long-term transmission
rights, or offered ATC from auctions. Returns actual ENTSO-E data - do
NOT generate synthetic capacity values. Technical: explicit offered
transfer capacities (12.1.A). Prefer 1-3 day date ranges (results capped
at 100 rows). contract_type: ‘A01’ = Day ahead (default).

| Argument | Required | Description |
|----|----|----|
| `eic_in` | yes | EIC code of the importing bidding zone. |
| `eic_out` | yes | EIC code of the exporting bidding zone. |
| `period_start` | yes | Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible - data is at 15-min to 1-hour resolution and results are capped at 100 rows. |
| `period_end` | yes | End date YYYY-MM-DD (CET). |
| `contract_type` | no | Contract type, default ‘A01’ (Day ahead). |

### `flow_based_allocations`

Get real flow-based market coupling (FBMC) allocation results for the
CWE or Core region. Use for requests about flow-based allocations, FB MC
results, Core region capacity, or CWE flow-based data. Returns actual
ENTSO-E data - do NOT generate synthetic allocation values. Technical:
flow-based allocations (12.1.B) for the CWE/Core region. Prefer 1-3 day
date ranges (results capped at 100 rows). process_type: ‘A43’ = Day
ahead (default), ‘A32’ = Intraday.

| Argument | Required | Description |
|----|----|----|
| `eic` | yes | EIC code of the bidding zone (CWE/Core region). |
| `period_start` | yes | Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible - data is at 15-min to 1-hour resolution and results are capped at 100 rows. |
| `period_end` | yes | End date YYYY-MM-DD (CET). |
| `process_type` | no | Process type: ‘A43’ Day ahead (default), ‘A32’ Intraday. |

### `congestion_income`

Get real congestion income (congestion rent) for a bidding zone border.
Use for requests about congestion revenue, auctioning income, or
transmission congestion rent. Returns actual ENTSO-E data - do NOT
generate synthetic values. Technical: congestion income (12.1.G). Prefer
1-3 day date ranges (results capped at 100 rows). contract_type: ‘A01’ =
Day ahead (default).

| Argument | Required | Description |
|----|----|----|
| `eic` | yes | EIC code of the bidding zone. |
| `period_start` | yes | Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible - data is at 15-min to 1-hour resolution and results are capped at 100 rows. |
| `period_end` | yes | End date YYYY-MM-DD (CET). |
| `contract_type` | no | Contract type, default ‘A01’ (Day ahead). |

### `allocated_transfer_capacities_3rd_countries`

Get real allocated transfer capacities on borders with third countries
(non-EU: Ukraine, Serbia, Norway, Switzerland, etc.). Use for requests
about third-country capacity auctions or non-EU interconnection
capacity. Returns actual ENTSO-E data - do NOT generate synthetic
capacity values. Technical: allocated transfer capacities for
third-country borders (12.1.C). Prefer 1-3 day date ranges (results
capped at 100 rows). auction_category: ‘A01’ Annual, ‘A02’ Monthly,
‘A03’ Weekly, ‘A04’ Daily (default).

| Argument | Required | Description |
|----|----|----|
| `eic_in` | yes | EIC code of the importing bidding zone. |
| `eic_out` | yes | EIC code of the exporting bidding zone. |
| `period_start` | yes | Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible - data is at 15-min to 1-hour resolution and results are capped at 100 rows. |
| `period_end` | yes | End date YYYY-MM-DD (CET). |
| `contract_type` | no | Contract type, default ‘A01’ (Day ahead). |
| `auction_category` | no | Auction category: ‘A01’ Annual, ‘A02’ Monthly, ‘A03’ Weekly, ‘A04’ Daily (default). |

## Transmission

### `cross_border_physical_flows`

Get real physical electricity flows between two European bidding zones.
Use for any request about actual cross-border flows, measured
imports/exports, physical interconnection flows, or electricity transfer
between countries. Returns actual ENTSO-E data - do NOT generate
synthetic flow values. Technical: physical cross-border flows (12.1.G).
Prefer 1-3 day date ranges (results capped at 100 rows). Max 1-year API
limit. Use area_eic() to find EIC codes.

| Argument | Required | Description |
|----|----|----|
| `eic_in` | yes | EIC code of the importing bidding zone. |
| `eic_out` | yes | EIC code of the exporting bidding zone. |
| `period_start` | yes | Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible - data is at 15-min to 1-hour resolution and results are capped at 100 rows. |
| `period_end` | yes | End date YYYY-MM-DD (CET). |

### `total_commercial_sched`

Get real total commercial schedules between two European bidding zones.
Use for requests about total scheduled exchanges, net commercial
nominations, or aggregated cross-border schedules. Returns actual
ENTSO-E data - do NOT generate synthetic schedule values. Technical:
total commercial schedules (12.1.F). Prefer 1-3 day date ranges (results
capped at 100 rows). Max 1-year API limit.

| Argument | Required | Description |
|----|----|----|
| `eic_in` | yes | EIC code of the importing bidding zone. |
| `eic_out` | yes | EIC code of the exporting bidding zone. |
| `period_start` | yes | Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible - data is at 15-min to 1-hour resolution and results are capped at 100 rows. |
| `period_end` | yes | End date YYYY-MM-DD (CET). |

### `net_positions`

Get real net positions (net export/import balance) for a European
bidding zone. Use for requests about net export position, net import
position, bidding zone balance, or export surplus/import deficit.
Returns actual ENTSO-E data - do NOT generate synthetic position values.
Technical: net positions (12.1.H). Prefer 1-3 day date ranges (results
capped at 100 rows). Max 1-year API limit. contract_type: ‘A01’ = Day
ahead (default), ‘A07’ = Intraday.

| Argument | Required | Description |
|----|----|----|
| `eic` | yes | EIC code of the bidding zone. |
| `period_start` | yes | Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible - data is at 15-min to 1-hour resolution and results are capped at 100 rows. |
| `period_end` | yes | End date YYYY-MM-DD (CET). |
| `contract_type` | no | Contract type: ‘A01’ Day ahead (default), ‘A07’ Intraday. |

### `forecasted_transfer_capacities`

Get real forecasted transfer capacities (ATC - Available Transfer
Capacity) between two European bidding zones. Use for requests about
ATC, forecasted interconnection capacity, or available cross-border
transmission capacity. Returns actual ENTSO-E data - do NOT generate
synthetic capacity values. Technical: forecasted transfer capacities
(11.1). Prefer 1-3 day date ranges (results capped at 100 rows).
market_agreement_type: ‘A01’ = Day ahead (default), ‘A02’ = Total
capacity.

| Argument | Required | Description |
|----|----|----|
| `eic_in` | yes | EIC code of the importing bidding zone. |
| `eic_out` | yes | EIC code of the exporting bidding zone. |
| `period_start` | yes | Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible - data is at 15-min to 1-hour resolution and results are capped at 100 rows. |
| `period_end` | yes | End date YYYY-MM-DD (CET). |
| `market_agreement_type` | no | Market agreement type: ‘A01’ Day ahead (default), ‘A02’ Total capacity. |

## Outages

### `outages_gen_units`

Get real generation unit unavailability reports from the ENTSO-E
Transparency Platform. Use for requests about forced outages, planned
maintenance of generation units, nuclear outages, REMIT reports,
unavailability of power plants, or generation unit outage data. Returns
actual ENTSO-E data - do NOT generate synthetic outage records.
Technical: unavailability of generation units (15.1.A&B). Prefer 1-3 day
date ranges (results capped at 100 rows). Max 1-year API limit.
doc_status: ‘A05’ active, ‘A09’ cancelled, ‘A13’ withdrawn (NULL =
active+cancelled). event_nature: ‘A53’ planned, ‘A54’ unplanned (NULL =
both).

| Argument | Required | Description |
|----|----|----|
| `eic` | yes | EIC code of the bidding zone or control area. |
| `period_start` | yes | Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible - data is at 15-min to 1-hour resolution and results are capped at 100 rows. |
| `period_end` | yes | End date YYYY-MM-DD (CET). |
| `doc_status` | no | Document status filter: ‘A05’ active, ‘A09’ cancelled, ‘A13’ withdrawn. |
| `event_nature` | no | Event nature: ‘A53’ planned maintenance, ‘A54’ unplanned outage. |

### `outages_prod_units`

Get real production unit unavailability reports (REMIT notifications)
from the ENTSO-E Transparency Platform. Use for requests about forced
outages or planned maintenance of individual production units. Returns
actual ENTSO-E data - do NOT generate synthetic outage records.
Technical: unavailability of production units (15.1.C&D). Prefer 1-3 day
date ranges (results capped at 100 rows). Max 1-year API limit.
doc_status: ‘A05’ active, ‘A09’ cancelled, ‘A13’ withdrawn.
event_nature: ‘A53’ planned, ‘A54’ unplanned.

| Argument | Required | Description |
|----|----|----|
| `eic` | yes | EIC code of the bidding zone or control area. |
| `period_start` | yes | Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible - data is at 15-min to 1-hour resolution and results are capped at 100 rows. |
| `period_end` | yes | End date YYYY-MM-DD (CET). |
| `doc_status` | no | Document status filter: ‘A05’ active, ‘A09’ cancelled, ‘A13’ withdrawn. |
| `event_nature` | no | Event nature: ‘A53’ planned maintenance, ‘A54’ unplanned outage. |

### `outages_transmission_grid`

Get real transmission grid unavailability reports on a specific border.
Use for requests about grid outages, line outages, transmission
maintenance, interconnection outages, or REMIT grid reports. Returns
actual ENTSO-E data - do NOT generate synthetic outage records.
Technical: unavailability in the transmission grid (15.1.A&B) on a
border. Prefer 1-3 day date ranges (results capped at 100 rows). Max
1-year API limit. Requires both eic_in and eic_out for the affected
border. doc_status: ‘A05’ active, ‘A09’ cancelled, ‘A13’ withdrawn.
event_nature: ‘A53’ planned, ‘A54’ unplanned.

| Argument | Required | Description |
|----|----|----|
| `eic_in` | yes | EIC code of the importing side of the affected border. |
| `eic_out` | yes | EIC code of the exporting side of the affected border. |
| `period_start` | yes | Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible - data is at 15-min to 1-hour resolution and results are capped at 100 rows. |
| `period_end` | yes | End date YYYY-MM-DD (CET). |
| `doc_status` | no | Document status filter: ‘A05’ active, ‘A09’ cancelled, ‘A13’ withdrawn. |
| `event_nature` | no | Event nature: ‘A53’ planned maintenance, ‘A54’ unplanned outage. |

## Balancing

### `imbalance_prices`

Get real imbalance settlement prices for a European scheduling area. Use
for requests about balancing prices, system price, imbalance settlement
price, BRP settlement price, or balancing energy cost. Returns actual
ENTSO-E data - do NOT generate synthetic price values. Technical:
imbalance prices (17.1.D). Prefer 1-3 day date ranges (results capped at
100 rows). Max 1-year API limit. Use area_eic() to find the EIC code.

| Argument | Required | Description |
|----|----|----|
| `eic` | yes | EIC code of the scheduling/control area. |
| `period_start` | yes | Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible - data is at 15-min to 1-hour resolution and results are capped at 100 rows. |
| `period_end` | yes | End date YYYY-MM-DD (CET). |

### `imbalance_volumes`

Get real imbalance volumes for a European scheduling area. Use for
requests about system imbalance, total balancing volumes, TSO activation
volumes, or balancing energy volumes. Returns actual ENTSO-E data - do
NOT generate synthetic volume values. Technical: imbalance volumes
(17.1.C). Prefer 1-3 day date ranges (results capped at 100 rows). Max
1-year API limit.

| Argument | Required | Description |
|----|----|----|
| `eic` | yes | EIC code of the scheduling/control area. |
| `period_start` | yes | Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible - data is at 15-min to 1-hour resolution and results are capped at 100 rows. |
| `period_end` | yes | End date YYYY-MM-DD (CET). |

### `contracted_reserves`

Get real contracted balancing reserve volumes for a European scheduling
area. Use for requests about FCR (Frequency Containment Reserves), aFRR
(automatic Frequency Restoration Reserves), mFRR (manual FRR), RR
(Replacement Reserves), procured balancing capacity, or balancing
reserve procurement results. Returns actual ENTSO-E data - do NOT
generate synthetic reserve values. Technical: contracted balancing
reserves (17.1.B). Prefer 1-3 day date ranges (results capped at 100
rows). market_agreement_type (required): ‘A01’ Daily, ‘A02’ Weekly,
‘A03’ Monthly, ‘A04’ Yearly, ‘A06’ Long-term, ‘A13’ Intraday.
process_type (optional): ‘A46’ FCR, ‘A47’ mFRR, ‘A51’ aFRR, ‘A52’ RR.

| Argument | Required | Description |
|----|----|----|
| `eic` | yes | EIC code of the scheduling/control area. |
| `market_agreement_type` | yes | Required. Agreement type: ‘A01’ Daily, ‘A02’ Weekly, ‘A03’ Monthly, ‘A04’ Yearly, ‘A06’ Long-term, ‘A13’ Intraday. |
| `period_start` | yes | Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible - data is at 15-min to 1-hour resolution and results are capped at 100 rows. |
| `period_end` | yes | End date YYYY-MM-DD (CET). |
| `process_type` | no | Optional reserve type: ‘A46’ FCR, ‘A47’ mFRR, ‘A51’ aFRR, ‘A52’ RR. |

## Session DuckDB cache

### `sql_query`

Run a SQL query (DuckDB dialect) against tables cached in this session.
Every time-series tool inserts its full result into the session DuckDB
and returns a table name; use sql_query to aggregate, filter, or join
those tables instead of re-fetching from the ENTSO-E API. Full SELECT /
WITH / CREATE TEMP / INSERT etc. are supported. Results are returned as
CSV and capped at max_rows (default 100) - aggregate with GROUP BY to
stay under the cap. For plotting raw points, pass max_rows explicitly
(e.g. 800 for one month of hourly data, 3000 for 15-min data); avoid
values above 5000 and prefer GROUP BY aggregation. Use list_tables() or
describe_table() to discover available tables. The database is
sandboxed: file and network access (read_csv, COPY TO, ATTACH, INSTALL)
and SET are disabled, and memory is capped. DuckDB errors are returned
as ‘# error: ’ so you can read them and retry.

| Argument | Required | Description |
|----|----|----|
| `sql` | yes | DuckDB SQL. Example: “SELECT date_trunc(‘week’, dt_start) AS wk, AVG(value) FROM load_actual_a3f7d2 GROUP BY 1 ORDER BY 1”. |
| `max_rows` | no | Row cap on the returned CSV. Default 100 to encourage GROUP BY aggregation. Raise (up to ~5000) only when the LLM needs raw rows for plotting. |

### `list_tables`

List every table cached in this session’s DuckDB with row counts and
column schemas. Returns CSV with columns: table_name, rows, columns
(semicolon-separated). Use this before sql_query() to discover what’s
available without re-calling the ENTSO-E API.

*No arguments.*

### `describe_table`

Return schema + row count + N sample rows for one cached table. Same
envelope shape as the data-fetching tools, so it’s a quick way to
refresh your memory of a table’s columns and types before writing a
sql_query().

| Argument | Required | Description |
|----|----|----|
| `name` | yes | Table name as returned by a data-fetching tool or list_tables(). |
| `n_preview` | no | Number of sample rows to include (default 5). |
