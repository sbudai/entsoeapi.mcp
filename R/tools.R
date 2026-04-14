# ============================================================
# EIC Lookup Tools (no date params)
# ============================================================

tool_area_eic <- ellmer::tool(
  name = "area_eic",
  fun = function() {
    entsoeapi::area_eic() |> safe_to_json()
  },
  description = "Return all Area Y EIC codes with display names and country codes.
Call this first to resolve a country or bidding zone name to its EIC code before
calling any data query tool.",
  arguments = list()
)

tool_party_eic <- ellmer::tool(
  name = "party_eic",
  fun = function() {
    entsoeapi::party_eic() |> safe_to_json()
  },
  description = "Return all Party X EIC codes for market participants (TSOs, traders, etc.).",
  arguments = list()
)

tool_all_approved_eic <- ellmer::tool(
  name = "all_approved_eic",
  fun = function() {
    entsoeapi::all_approved_eic() |> safe_to_json()
  },
  description = "Return all approved EIC codes across all types (Area, Party, Tie-line,
Resource Object, Substation, etc.) in one table. Slower than area_eic() alone.",
  arguments = list()
)

tool_resource_object_eic <- ellmer::tool(
  name = "resource_object_eic",
  fun = function() {
    entsoeapi::resource_object_eic() |> safe_to_json()
  },
  description = "Return all Resource Object W EIC codes — generation units and other
resources that can produce or consume energy.",
  arguments = list()
)

tool_get_news <- ellmer::tool(
  name = "get_news",
  fun = function(n = 10L) {
    entsoeapi::get_news(n = as.integer(n)) |> safe_to_json()
  },
  description = "Fetch the latest news items from the ENTSO-E Transparency Platform RSS feed.
Useful for checking maintenance windows or data availability issues.",
  arguments = list(
    n = ellmer::type_integer(
      "Number of news items to return. Defaults to 10.",
      required = FALSE
    )
  )
)


# ============================================================
# Load Tools
# ============================================================

tool_load_actual_total <- ellmer::tool(
  name = "load_actual_total",
  fun = function(eic, period_start, period_end) {
    entsoeapi::load_actual_total(
      eic          = eic,
      period_start = parse_date(period_start),
      period_end   = parse_date(period_end),
      tidy_output  = TRUE
    ) |> safe_to_json()
  },
  description = "Get actual total load (6.1.A) for a bidding zone. Max 1-year range.
Use area_eic() to find the EIC code for a country/zone.",
  arguments = list(
    eic          = ellmer::type_string("EIC code of the bidding zone, e.g. '10Y1001A1001A83F' for Germany."),
    period_start = ellmer::type_string("Start date in YYYY-MM-DD format (CET timezone)."),
    period_end   = ellmer::type_string("End date in YYYY-MM-DD format (CET timezone).")
  )
)

tool_load_day_ahead_total_forecast <- ellmer::tool(
  name = "load_day_ahead_total_forecast",
  fun = function(eic, period_start, period_end) {
    entsoeapi::load_day_ahead_total_forecast(
      eic          = eic,
      period_start = parse_date(period_start),
      period_end   = parse_date(period_end),
      tidy_output  = TRUE
    ) |> safe_to_json()
  },
  description = "Get day-ahead total load forecast (6.1.B) for a bidding zone. Max 1-year range.",
  arguments = list(
    eic          = ellmer::type_string("EIC code of the bidding zone."),
    period_start = ellmer::type_string("Start date in YYYY-MM-DD format (CET timezone)."),
    period_end   = ellmer::type_string("End date in YYYY-MM-DD format (CET timezone).")
  )
)

tool_load_week_ahead_total_forecast <- ellmer::tool(
  name = "load_week_ahead_total_forecast",
  fun = function(eic, period_start, period_end) {
    entsoeapi::load_week_ahead_total_forecast(
      eic          = eic,
      period_start = parse_date(period_start),
      period_end   = parse_date(period_end),
      tidy_output  = TRUE
    ) |> safe_to_json()
  },
  description = "Get week-ahead total load forecast (6.1.C) for a bidding zone. Max 1-year range.",
  arguments = list(
    eic          = ellmer::type_string("EIC code of the bidding zone."),
    period_start = ellmer::type_string("Start date in YYYY-MM-DD format (CET timezone)."),
    period_end   = ellmer::type_string("End date in YYYY-MM-DD format (CET timezone).")
  )
)

tool_load_month_ahead_total_forecast <- ellmer::tool(
  name = "load_month_ahead_total_forecast",
  fun = function(eic, period_start, period_end) {
    entsoeapi::load_month_ahead_total_forecast(
      eic          = eic,
      period_start = parse_date(period_start),
      period_end   = parse_date(period_end),
      tidy_output  = TRUE
    ) |> safe_to_json()
  },
  description = "Get month-ahead total load forecast (6.1.D) for a bidding zone. Max 1-year range.",
  arguments = list(
    eic          = ellmer::type_string("EIC code of the bidding zone."),
    period_start = ellmer::type_string("Start date in YYYY-MM-DD format (CET timezone)."),
    period_end   = ellmer::type_string("End date in YYYY-MM-DD format (CET timezone).")
  )
)

tool_load_year_ahead_total_forecast <- ellmer::tool(
  name = "load_year_ahead_total_forecast",
  fun = function(eic, period_start, period_end) {
    entsoeapi::load_year_ahead_total_forecast(
      eic          = eic,
      period_start = parse_date(period_start),
      period_end   = parse_date(period_end),
      tidy_output  = TRUE
    ) |> safe_to_json()
  },
  description = "Get year-ahead total load forecast (6.1.E) for a bidding zone. Max 1-year range.",
  arguments = list(
    eic          = ellmer::type_string("EIC code of the bidding zone."),
    period_start = ellmer::type_string("Start date in YYYY-MM-DD format (CET timezone)."),
    period_end   = ellmer::type_string("End date in YYYY-MM-DD format (CET timezone).")
  )
)

tool_load_year_ahead_forecast_margin <- ellmer::tool(
  name = "load_year_ahead_forecast_margin",
  fun = function(eic, period_start, period_end) {
    entsoeapi::load_year_ahead_forecast_margin(
      eic          = eic,
      period_start = parse_date(period_start),
      period_end   = parse_date(period_end),
      tidy_output  = TRUE
    ) |> safe_to_json()
  },
  description = "Get year-ahead generation forecast margin (8.1) for a bidding zone. Max 1-year range.",
  arguments = list(
    eic          = ellmer::type_string("EIC code of the bidding zone."),
    period_start = ellmer::type_string("Start date in YYYY-MM-DD format (CET timezone)."),
    period_end   = ellmer::type_string("End date in YYYY-MM-DD format (CET timezone).")
  )
)


# ============================================================
# Generation Tools
# ============================================================

tool_gen_per_prod_type <- ellmer::tool(
  name = "gen_per_prod_type",
  fun = function(eic, period_start, period_end, gen_type = NULL) {
    entsoeapi::gen_per_prod_type(
      eic          = eic,
      period_start = parse_date(period_start),
      period_end   = parse_date(period_end),
      gen_type     = gen_type,
      tidy_output  = TRUE
    ) |> safe_to_json()
  },
  description = "Get actual generation per production type (16.1.B&C) for a bidding zone.
Max 1-day range per call (internally paginated). Optionally filter by generation type code.",
  arguments = list(
    eic          = ellmer::type_string("EIC code of the bidding zone."),
    period_start = ellmer::type_string("Start date in YYYY-MM-DD format (CET timezone)."),
    period_end   = ellmer::type_string("End date in YYYY-MM-DD format (CET timezone)."),
    gen_type     = ellmer::type_string(
      "Optional ENTSO-E production type code, e.g. 'B01' for Biomass, 'B16' for Solar.",
      required = FALSE
    )
  )
)

tool_gen_installed_capacity_per_pt <- ellmer::tool(
  name = "gen_installed_capacity_per_pt",
  fun = function(eic, year, psr_type = NULL) {
    entsoeapi::gen_installed_capacity_per_pt(
      eic      = eic,
      year     = as.integer(year),
      psr_type = psr_type,
      tidy_output = TRUE
    ) |> safe_to_json()
  },
  description = "Get installed generation capacity per production type (14.1.A) for a bidding zone
and a given year. Optionally filter by PSR type code.",
  arguments = list(
    eic      = ellmer::type_string("EIC code of the bidding zone."),
    year     = ellmer::type_integer("Year as an integer, e.g. 2024."),
    psr_type = ellmer::type_string(
      "Optional PSR type code, e.g. 'B16' for Solar PV.",
      required = FALSE
    )
  )
)

tool_gen_installed_capacity_per_pu <- ellmer::tool(
  name = "gen_installed_capacity_per_pu",
  fun = function(eic, year, psr_type = NULL) {
    entsoeapi::gen_installed_capacity_per_pu(
      eic      = eic,
      year     = as.integer(year),
      psr_type = psr_type,
      tidy_output = TRUE
    ) |> safe_to_json()
  },
  description = "Get installed generation capacity per production unit (14.1.C) for a bidding zone
and a given year. More granular than per_pt (individual unit level).",
  arguments = list(
    eic      = ellmer::type_string("EIC code of the bidding zone."),
    year     = ellmer::type_integer("Year as an integer, e.g. 2024."),
    psr_type = ellmer::type_string(
      "Optional PSR type code, e.g. 'B16' for Solar PV.",
      required = FALSE
    )
  )
)

tool_gen_wind_solar_forecasts <- ellmer::tool(
  name = "gen_wind_solar_forecasts",
  fun = function(eic, period_start, period_end) {
    entsoeapi::gen_wind_solar_forecasts(
      eic          = eic,
      period_start = parse_date(period_start),
      period_end   = parse_date(period_end),
      tidy_output  = TRUE
    ) |> safe_to_json()
  },
  description = "Get day-ahead wind and solar generation forecasts (14.1.D) for a bidding zone.
Max 1-year range.",
  arguments = list(
    eic          = ellmer::type_string("EIC code of the bidding zone."),
    period_start = ellmer::type_string("Start date in YYYY-MM-DD format (CET timezone)."),
    period_end   = ellmer::type_string("End date in YYYY-MM-DD format (CET timezone).")
  )
)

tool_gen_day_ahead_forecast <- ellmer::tool(
  name = "gen_day_ahead_forecast",
  fun = function(eic, period_start, period_end) {
    entsoeapi::gen_day_ahead_forecast(
      eic          = eic,
      period_start = parse_date(period_start),
      period_end   = parse_date(period_end),
      tidy_output  = TRUE
    ) |> safe_to_json()
  },
  description = "Get day-ahead aggregated generation forecast (14.1.C) for a bidding zone.
Max 1-year range.",
  arguments = list(
    eic          = ellmer::type_string("EIC code of the bidding zone."),
    period_start = ellmer::type_string("Start date in YYYY-MM-DD format (CET timezone)."),
    period_end   = ellmer::type_string("End date in YYYY-MM-DD format (CET timezone).")
  )
)

tool_gen_per_gen_unit <- ellmer::tool(
  name = "gen_per_gen_unit",
  fun = function(eic, period_start, period_end, gen_type = NULL) {
    entsoeapi::gen_per_gen_unit(
      eic          = eic,
      period_start = parse_date(period_start),
      period_end   = parse_date(period_end),
      gen_type     = gen_type,
      tidy_output  = TRUE
    ) |> safe_to_json()
  },
  description = "Get actual generation per generation unit (16.1.A) for a control area.
Paginated internally to 1-day chunks. Optionally filter by generation type code.
Note: This can be slow for long date ranges.",
  arguments = list(
    eic          = ellmer::type_string("EIC code of the control area."),
    period_start = ellmer::type_string("Start date in YYYY-MM-DD format (CET timezone)."),
    period_end   = ellmer::type_string("End date in YYYY-MM-DD format (CET timezone)."),
    gen_type     = ellmer::type_string(
      "Optional ENTSO-E production type code, e.g. 'B02' for Brown coal.",
      required = FALSE
    )
  )
)

tool_gen_storage_mean_filling_rate <- ellmer::tool(
  name = "gen_storage_mean_filling_rate",
  fun = function(eic, period_start, period_end) {
    entsoeapi::gen_storage_mean_filling_rate(
      eic          = eic,
      period_start = parse_date(period_start),
      period_end   = parse_date(period_end),
      tidy_output  = TRUE
    ) |> safe_to_json()
  },
  description = "Get aggregated filling rate of water reservoirs and hydro storage plants (16.1.D)
for a bidding zone. Max 1-year range.",
  arguments = list(
    eic          = ellmer::type_string("EIC code of the bidding zone."),
    period_start = ellmer::type_string("Start date in YYYY-MM-DD format (CET timezone)."),
    period_end   = ellmer::type_string("End date in YYYY-MM-DD format (CET timezone).")
  )
)


# ============================================================
# Market Tools
# ============================================================

tool_energy_prices <- ellmer::tool(
  name = "energy_prices",
  fun = function(eic, period_start, period_end, contract_type = "A01") {
    entsoeapi::energy_prices(
      eic           = eic,
      period_start  = parse_date(period_start),
      period_end    = parse_date(period_end),
      contract_type = contract_type,
      tidy_output   = TRUE
    ) |> safe_to_json()
  },
  description = "Get day-ahead or intraday market clearing prices (12.1.D) for a bidding zone.
Max 1-year range. contract_type: 'A01' = Day ahead (default), 'A07' = Intraday.
Use area_eic() to find the EIC code.",
  arguments = list(
    eic           = ellmer::type_string("EIC code of the bidding zone."),
    period_start  = ellmer::type_string("Start date in YYYY-MM-DD format (CET timezone)."),
    period_end    = ellmer::type_string("End date in YYYY-MM-DD format (CET timezone)."),
    contract_type = ellmer::type_string(
      "Contract type: 'A01' for Day ahead (default), 'A07' for Intraday.",
      required = FALSE
    )
  )
)

tool_intraday_prices <- ellmer::tool(
  name = "intraday_prices",
  fun = function(eic, period_start, period_end) {
    entsoeapi::intraday_prices(
      eic          = eic,
      period_start = parse_date(period_start),
      period_end   = parse_date(period_end),
      tidy_output  = TRUE
    ) |> safe_to_json()
  },
  description = "Get intraday market prices for a bidding zone. Specialised wrapper around
the intraday contract type. Max 1-year range.",
  arguments = list(
    eic          = ellmer::type_string("EIC code of the bidding zone."),
    period_start = ellmer::type_string("Start date in YYYY-MM-DD format (CET timezone)."),
    period_end   = ellmer::type_string("End date in YYYY-MM-DD format (CET timezone).")
  )
)

tool_net_transfer_capacities <- ellmer::tool(
  name = "net_transfer_capacities",
  fun = function(eic_in, eic_out, period_start, period_end, contract_type = "A02") {
    entsoeapi::net_transfer_capacities(
      eic_in        = eic_in,
      eic_out       = eic_out,
      period_start  = parse_date(period_start),
      period_end    = parse_date(period_end),
      contract_type = contract_type,
      tidy_output   = TRUE
    ) |> safe_to_json()
  },
  description = "Get Net Transfer Capacities (NTC) between two bidding zones (11.1).
contract_type: 'A02' = Day ahead (default), 'A03' = Intraday, 'A04' = Week ahead.
Use area_eic() to find EIC codes for both zones.",
  arguments = list(
    eic_in        = ellmer::type_string("EIC code of the importing bidding zone."),
    eic_out       = ellmer::type_string("EIC code of the exporting bidding zone."),
    period_start  = ellmer::type_string("Start date in YYYY-MM-DD format (CET timezone)."),
    period_end    = ellmer::type_string("End date in YYYY-MM-DD format (CET timezone)."),
    contract_type = ellmer::type_string(
      "Contract type: 'A02' Day ahead (default), 'A03' Intraday, 'A04' Week ahead.",
      required = FALSE
    )
  )
)

tool_day_ahead_commercial_sched <- ellmer::tool(
  name = "day_ahead_commercial_sched",
  fun = function(eic_in, eic_out, period_start, period_end) {
    entsoeapi::day_ahead_commercial_sched(
      eic_in       = eic_in,
      eic_out      = eic_out,
      period_start = parse_date(period_start),
      period_end   = parse_date(period_end),
      tidy_output  = TRUE
    ) |> safe_to_json()
  },
  description = "Get day-ahead commercial schedules (12.1.F) between two bidding zones.
Max 1-year range. Use area_eic() to find EIC codes.",
  arguments = list(
    eic_in       = ellmer::type_string("EIC code of the importing bidding zone."),
    eic_out      = ellmer::type_string("EIC code of the exporting bidding zone."),
    period_start = ellmer::type_string("Start date in YYYY-MM-DD format (CET timezone)."),
    period_end   = ellmer::type_string("End date in YYYY-MM-DD format (CET timezone).")
  )
)

tool_explicit_offered_transfer_capacities <- ellmer::tool(
  name = "explicit_offered_transfer_capacities",
  fun = function(eic_in, eic_out, period_start, period_end, contract_type = "A01") {
    entsoeapi::explicit_offered_transfer_capacities(
      eic_in        = eic_in,
      eic_out       = eic_out,
      period_start  = parse_date(period_start),
      period_end    = parse_date(period_end),
      contract_type = contract_type,
      tidy_output   = TRUE
    ) |> safe_to_json()
  },
  description = "Get explicit offered transfer capacities (12.1.A) between two bidding zones.
contract_type: 'A01' = Day ahead (default).",
  arguments = list(
    eic_in        = ellmer::type_string("EIC code of the importing bidding zone."),
    eic_out       = ellmer::type_string("EIC code of the exporting bidding zone."),
    period_start  = ellmer::type_string("Start date in YYYY-MM-DD format (CET timezone)."),
    period_end    = ellmer::type_string("End date in YYYY-MM-DD format (CET timezone)."),
    contract_type = ellmer::type_string(
      "Contract type, default 'A01' (Day ahead).",
      required = FALSE
    )
  )
)

tool_flow_based_allocations <- ellmer::tool(
  name = "flow_based_allocations",
  fun = function(eic, period_start, period_end, process_type = "A43") {
    entsoeapi::flow_based_allocations(
      eic          = eic,
      period_start = parse_date(period_start),
      period_end   = parse_date(period_end),
      process_type = process_type,
      tidy_output  = TRUE
    ) |> safe_to_json()
  },
  description = "Get flow-based allocations (12.1.B) for the CWE/Core region.
process_type: 'A43' = Day ahead (default), 'A32' = Intraday.",
  arguments = list(
    eic          = ellmer::type_string("EIC code of the bidding zone (CWE/Core region)."),
    period_start = ellmer::type_string("Start date in YYYY-MM-DD format (CET timezone)."),
    period_end   = ellmer::type_string("End date in YYYY-MM-DD format (CET timezone)."),
    process_type = ellmer::type_string(
      "Process type: 'A43' Day ahead (default), 'A32' Intraday.",
      required = FALSE
    )
  )
)

tool_congestion_income <- ellmer::tool(
  name = "congestion_income",
  fun = function(eic, period_start, period_end, contract_type = "A01") {
    entsoeapi::congestion_income(
      eic           = eic,
      period_start  = parse_date(period_start),
      period_end    = parse_date(period_end),
      contract_type = contract_type,
      tidy_output   = TRUE
    ) |> safe_to_json()
  },
  description = "Get congestion income (12.1.G) for a bidding zone or border.
contract_type: 'A01' = Day ahead (default).",
  arguments = list(
    eic           = ellmer::type_string("EIC code of the bidding zone."),
    period_start  = ellmer::type_string("Start date in YYYY-MM-DD format (CET timezone)."),
    period_end    = ellmer::type_string("End date in YYYY-MM-DD format (CET timezone)."),
    contract_type = ellmer::type_string(
      "Contract type, default 'A01' (Day ahead).",
      required = FALSE
    )
  )
)

tool_allocated_transfer_capacities_3rd_countries <- ellmer::tool(
  name = "allocated_transfer_capacities_3rd_countries",
  fun = function(eic_in, eic_out, period_start, period_end,
                 contract_type = "A01", auction_category = "A04") {
    entsoeapi::allocated_transfer_capacities_3rd_countries(
      eic_in            = eic_in,
      eic_out           = eic_out,
      period_start      = parse_date(period_start),
      period_end        = parse_date(period_end),
      contract_type     = contract_type,
      auction_category  = auction_category,
      tidy_output       = TRUE
    ) |> safe_to_json()
  },
  description = "Get allocated transfer capacities for third-country borders (12.1.C).
auction_category: 'A01' Annual, 'A02' Monthly, 'A03' Weekly, 'A04' Daily (default).",
  arguments = list(
    eic_in           = ellmer::type_string("EIC code of the importing bidding zone."),
    eic_out          = ellmer::type_string("EIC code of the exporting bidding zone."),
    period_start     = ellmer::type_string("Start date in YYYY-MM-DD format (CET timezone)."),
    period_end       = ellmer::type_string("End date in YYYY-MM-DD format (CET timezone)."),
    contract_type    = ellmer::type_string(
      "Contract type, default 'A01' (Day ahead).",
      required = FALSE
    ),
    auction_category = ellmer::type_string(
      "Auction category: 'A01' Annual, 'A02' Monthly, 'A03' Weekly, 'A04' Daily (default).",
      required = FALSE
    )
  )
)


# ============================================================
# Transmission Tools
# ============================================================

tool_cross_border_physical_flows <- ellmer::tool(
  name = "cross_border_physical_flows",
  fun = function(eic_in, eic_out, period_start, period_end) {
    entsoeapi::cross_border_physical_flows(
      eic_in       = eic_in,
      eic_out      = eic_out,
      period_start = parse_date(period_start),
      period_end   = parse_date(period_end),
      tidy_output  = TRUE
    ) |> safe_to_json()
  },
  description = "Get physical cross-border flows (12.1.G) between two bidding zones.
Max 1-year range. Use area_eic() to find EIC codes.",
  arguments = list(
    eic_in       = ellmer::type_string("EIC code of the importing bidding zone."),
    eic_out      = ellmer::type_string("EIC code of the exporting bidding zone."),
    period_start = ellmer::type_string("Start date in YYYY-MM-DD format (CET timezone)."),
    period_end   = ellmer::type_string("End date in YYYY-MM-DD format (CET timezone).")
  )
)

tool_total_commercial_sched <- ellmer::tool(
  name = "total_commercial_sched",
  fun = function(eic_in, eic_out, period_start, period_end) {
    entsoeapi::total_commercial_sched(
      eic_in       = eic_in,
      eic_out      = eic_out,
      period_start = parse_date(period_start),
      period_end   = parse_date(period_end),
      tidy_output  = TRUE
    ) |> safe_to_json()
  },
  description = "Get total commercial schedules (12.1.F) between two bidding zones.
Max 1-year range.",
  arguments = list(
    eic_in       = ellmer::type_string("EIC code of the importing bidding zone."),
    eic_out      = ellmer::type_string("EIC code of the exporting bidding zone."),
    period_start = ellmer::type_string("Start date in YYYY-MM-DD format (CET timezone)."),
    period_end   = ellmer::type_string("End date in YYYY-MM-DD format (CET timezone).")
  )
)

tool_net_positions <- ellmer::tool(
  name = "net_positions",
  fun = function(eic, period_start, period_end, contract_type = "A01") {
    entsoeapi::net_positions(
      eic           = eic,
      period_start  = parse_date(period_start),
      period_end    = parse_date(period_end),
      contract_type = contract_type,
      tidy_output   = TRUE
    ) |> safe_to_json()
  },
  description = "Get net positions (12.1.H) for a bidding zone.
contract_type: 'A01' = Day ahead (default), 'A07' = Intraday. Max 1-year range.",
  arguments = list(
    eic           = ellmer::type_string("EIC code of the bidding zone."),
    period_start  = ellmer::type_string("Start date in YYYY-MM-DD format (CET timezone)."),
    period_end    = ellmer::type_string("End date in YYYY-MM-DD format (CET timezone)."),
    contract_type = ellmer::type_string(
      "Contract type: 'A01' Day ahead (default), 'A07' Intraday.",
      required = FALSE
    )
  )
)

tool_forecasted_transfer_capacities <- ellmer::tool(
  name = "forecasted_transfer_capacities",
  fun = function(eic_in, eic_out, period_start, period_end,
                 market_agreement_type = "A01") {
    entsoeapi::forecasted_transfer_capacities(
      eic_in                = eic_in,
      eic_out               = eic_out,
      period_start          = parse_date(period_start),
      period_end            = parse_date(period_end),
      market_agreement_type = market_agreement_type,
      tidy_output           = TRUE
    ) |> safe_to_json()
  },
  description = "Get forecasted transfer capacities (11.1) between two bidding zones.
market_agreement_type: 'A01' = Day ahead (default), 'A02' = Total capacity.",
  arguments = list(
    eic_in                = ellmer::type_string("EIC code of the importing bidding zone."),
    eic_out               = ellmer::type_string("EIC code of the exporting bidding zone."),
    period_start          = ellmer::type_string("Start date in YYYY-MM-DD format (CET timezone)."),
    period_end            = ellmer::type_string("End date in YYYY-MM-DD format (CET timezone)."),
    market_agreement_type = ellmer::type_string(
      "Market agreement type: 'A01' Day ahead (default), 'A02' Total capacity.",
      required = FALSE
    )
  )
)


# ============================================================
# Outages Tools
# ============================================================

tool_outages_gen_units <- ellmer::tool(
  name = "outages_gen_units",
  fun = function(eic, period_start, period_end,
                 doc_status = NULL, event_nature = NULL) {
    entsoeapi::outages_gen_units(
      eic          = eic,
      period_start = parse_date(period_start),
      period_end   = parse_date(period_end),
      doc_status   = doc_status,
      event_nature = event_nature,
      tidy_output  = TRUE
    ) |> safe_to_json()
  },
  description = "Get unavailability of generation units (15.1.A&B) for a bidding zone.
Max 1-year range. doc_status: 'A05' active, 'A09' cancelled, 'A13' withdrawn (NULL = active+cancelled).
event_nature: 'A53' planned, 'A54' unplanned (NULL = both).",
  arguments = list(
    eic          = ellmer::type_string("EIC code of the bidding zone or control area."),
    period_start = ellmer::type_string("Start date in YYYY-MM-DD format (CET timezone)."),
    period_end   = ellmer::type_string("End date in YYYY-MM-DD format (CET timezone)."),
    doc_status   = ellmer::type_string(
      "Document status filter: 'A05' active, 'A09' cancelled, 'A13' withdrawn.",
      required = FALSE
    ),
    event_nature = ellmer::type_string(
      "Event nature: 'A53' planned maintenance, 'A54' unplanned outage.",
      required = FALSE
    )
  )
)

tool_outages_prod_units <- ellmer::tool(
  name = "outages_prod_units",
  fun = function(eic, period_start, period_end,
                 doc_status = NULL, event_nature = NULL) {
    entsoeapi::outages_prod_units(
      eic          = eic,
      period_start = parse_date(period_start),
      period_end   = parse_date(period_end),
      doc_status   = doc_status,
      event_nature = event_nature,
      tidy_output  = TRUE
    ) |> safe_to_json()
  },
  description = "Get unavailability of production units (15.1.C&D) for a bidding zone.
Max 1-year range. doc_status: 'A05' active, 'A09' cancelled, 'A13' withdrawn.
event_nature: 'A53' planned, 'A54' unplanned.",
  arguments = list(
    eic          = ellmer::type_string("EIC code of the bidding zone or control area."),
    period_start = ellmer::type_string("Start date in YYYY-MM-DD format (CET timezone)."),
    period_end   = ellmer::type_string("End date in YYYY-MM-DD format (CET timezone)."),
    doc_status   = ellmer::type_string(
      "Document status filter: 'A05' active, 'A09' cancelled, 'A13' withdrawn.",
      required = FALSE
    ),
    event_nature = ellmer::type_string(
      "Event nature: 'A53' planned maintenance, 'A54' unplanned outage.",
      required = FALSE
    )
  )
)

tool_outages_transmission_grid <- ellmer::tool(
  name = "outages_transmission_grid",
  fun = function(eic_in, eic_out, period_start, period_end,
                 doc_status = NULL, event_nature = NULL) {
    entsoeapi::outages_transmission_grid(
      eic_in       = eic_in,
      eic_out      = eic_out,
      period_start = parse_date(period_start),
      period_end   = parse_date(period_end),
      doc_status   = doc_status,
      event_nature = event_nature,
      tidy_output  = TRUE
    ) |> safe_to_json()
  },
  description = "Get unavailability in the transmission grid (15.1.A&B) on a border.
Max 1-year range. Requires both eic_in and eic_out for the affected border.
doc_status: 'A05' active, 'A09' cancelled, 'A13' withdrawn.
event_nature: 'A53' planned, 'A54' unplanned.",
  arguments = list(
    eic_in       = ellmer::type_string("EIC code of the importing side of the affected border."),
    eic_out      = ellmer::type_string("EIC code of the exporting side of the affected border."),
    period_start = ellmer::type_string("Start date in YYYY-MM-DD format (CET timezone)."),
    period_end   = ellmer::type_string("End date in YYYY-MM-DD format (CET timezone)."),
    doc_status   = ellmer::type_string(
      "Document status filter: 'A05' active, 'A09' cancelled, 'A13' withdrawn.",
      required = FALSE
    ),
    event_nature = ellmer::type_string(
      "Event nature: 'A53' planned maintenance, 'A54' unplanned outage.",
      required = FALSE
    )
  )
)


# ============================================================
# Balancing Tools
# ============================================================

tool_imbalance_prices <- ellmer::tool(
  name = "imbalance_prices",
  fun = function(eic, period_start, period_end) {
    entsoeapi::imbalance_prices(
      eic          = eic,
      period_start = parse_date(period_start),
      period_end   = parse_date(period_end),
      tidy_output  = TRUE
    ) |> safe_to_json()
  },
  description = "Get imbalance prices (17.1.D) for a scheduling area. Max 1-year range.
Use area_eic() to find the EIC code.",
  arguments = list(
    eic          = ellmer::type_string("EIC code of the scheduling/control area."),
    period_start = ellmer::type_string("Start date in YYYY-MM-DD format (CET timezone)."),
    period_end   = ellmer::type_string("End date in YYYY-MM-DD format (CET timezone).")
  )
)

tool_imbalance_volumes <- ellmer::tool(
  name = "imbalance_volumes",
  fun = function(eic, period_start, period_end) {
    entsoeapi::imbalance_volumes(
      eic          = eic,
      period_start = parse_date(period_start),
      period_end   = parse_date(period_end),
      tidy_output  = TRUE
    ) |> safe_to_json()
  },
  description = "Get imbalance volumes (17.1.C) for a scheduling area. Max 1-year range.",
  arguments = list(
    eic          = ellmer::type_string("EIC code of the scheduling/control area."),
    period_start = ellmer::type_string("Start date in YYYY-MM-DD format (CET timezone)."),
    period_end   = ellmer::type_string("End date in YYYY-MM-DD format (CET timezone).")
  )
)

tool_contracted_reserves <- ellmer::tool(
  name = "contracted_reserves",
  fun = function(eic, market_agreement_type, period_start, period_end,
                 process_type = NULL) {
    entsoeapi::contracted_reserves(
      eic                   = eic,
      market_agreement_type = market_agreement_type,
      process_type          = process_type,
      period_start          = parse_date(period_start),
      period_end            = parse_date(period_end),
      tidy_output           = TRUE
    ) |> safe_to_json()
  },
  description = "Get contracted balancing reserves (17.1.B) for a scheduling area.
market_agreement_type (required): 'A01' Daily, 'A02' Weekly, 'A03' Monthly,
'A04' Yearly, 'A06' Long-term, 'A13' Intraday.
process_type (optional): 'A46' FCR, 'A47' mFRR, 'A51' aFRR, 'A52' RR.",
  arguments = list(
    eic                   = ellmer::type_string("EIC code of the scheduling/control area."),
    market_agreement_type = ellmer::type_string(
      "Required. Agreement type: 'A01' Daily, 'A02' Weekly, 'A03' Monthly, 'A04' Yearly, 'A06' Long-term, 'A13' Intraday."
    ),
    period_start          = ellmer::type_string("Start date in YYYY-MM-DD format (CET timezone)."),
    period_end            = ellmer::type_string("End date in YYYY-MM-DD format (CET timezone)."),
    process_type          = ellmer::type_string(
      "Optional reserve type: 'A46' FCR, 'A47' mFRR, 'A51' aFRR, 'A52' RR.",
      required = FALSE
    )
  )
)
