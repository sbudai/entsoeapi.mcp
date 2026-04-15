# ============================================================
# EIC Lookup Tools (no date params)
# ============================================================

# Helper: keep only identification-relevant columns and filter rows by a query string.
# Drops rarely-needed metadata (postal_code, vat_code, parent, responsible_party, status)
# to reduce context window usage.
.eic_filter <- function(df, query) {
  key_cols <- c(
    "eic_code", "eic_display_name", "eic_long_name",
    "market_participant_iso_country_code", "eic_type_function_list"
  )
  needed_cols <- intersect(x = key_cols, y = names(df))
  df <- df[, needed_cols, drop = FALSE]
  if (!is.null(query) && nchar(trimws(query)) > 0L) {
    mask <- apply(
      X = df,
      MARGIN = 1L,
      FUN = \(row) grepl(pattern = query, x = row, ignore.case = TRUE) |> any()
    )
    df <- df[mask, , drop = FALSE]
  }
  df
}


tool_area_eic <- ellmer::tool(
  name = "area_eic",
  fun = \(query = NULL) {
    entsoeapi::area_eic() |>
      .eic_filter(query) |>
      safe_to_csv(max_rows = 200L)
  },
  description = "Return Area Y EIC codes (bidding zones, control areas, member states, etc.). Pass a query string to filter by country name, ISO code, or zone name (case-insensitive). IMPORTANT: Always display the full table of matches to the user and ask them to confirm which EIC code to use before querying data — many countries have multiple bidding zones.",
  arguments = list(
    query = ellmer::type_string(
      description = "Optional filter string, e.g. 'Germany', 'DE', 'France', 'Bidding Zone'. Case-insensitive match against display name, long name, and country code.",
      required = FALSE
    )
  )
)


tool_party_eic <- ellmer::tool(
  name = "party_eic",
  fun = \(query = NULL) {
    entsoeapi::party_eic() |>
      .eic_filter(query) |>
      safe_to_csv(max_rows = 200L)
  },
  description = "Return Party X EIC codes for market participants (TSOs, traders, etc.). Pass a query string to filter by name or country code.",
  arguments = list(
    query = ellmer::type_string(
      description = "Optional filter string, e.g. 'Germany', 'TSO', 'DE'. Case-insensitive.",
      required = FALSE
    )
  )
)


tool_all_approved_eic <- ellmer::tool(
  name = "all_approved_eic",
  fun = \(query = NULL) {
    entsoeapi::all_approved_eic() |>
      .eic_filter(query) |>
      safe_to_csv(max_rows = 200L)
  },
  description = "Return all approved EIC codes across all types (Area, Party, Tie-line, Resource Object, Substation, etc.) in one table. Slower than area_eic() alone. Pass a query string to filter results.",
  arguments = list(
    query = ellmer::type_string(
      description = "Optional filter string, e.g. 'Germany', 'DE', 'Tie-line'. Case-insensitive.",
      required = FALSE
    )
  )
)


tool_resource_object_eic <- ellmer::tool(
  name = "resource_object_eic",
  fun = \(query = NULL) {
    entsoeapi::resource_object_eic() |>
      .eic_filter(query) |>
      safe_to_csv(max_rows = 200L)
  },
  description = "Return Resource Object W EIC codes — generation units and other resources. Pass a query string to filter by plant name, country code, or fuel type.",
  arguments = list(
    query = ellmer::type_string(
      description = "Optional filter string, e.g. 'wind', 'DE', 'nuclear'. Case-insensitive.",
      required = FALSE
    )
  )
)

tool_get_news <- ellmer::tool(
  name = "get_news",
  fun = \(n = 10L) {
    entsoeapi::get_news(n = as.integer(n)) |>
      safe_to_csv()
  },
  description = "Fetch the latest news items from the ENTSO-E Transparency Platform RSS feed. Useful for checking maintenance windows or data availability issues.",
  arguments = list(
    n = ellmer::type_integer(
      description = "Number of news items to return. Defaults to 10.",
      required = FALSE
    )
  )
)


# ============================================================
# Load Tools
# ============================================================

tool_load <- ellmer::tool(
  name = "load",
  fun = function(eic, period_start, period_end, type = "actual") {
    ps <- parse_date(period_start)
    pe <- parse_date(period_end)
    df <- switch(type,
      actual      = entsoeapi::load_actual_total(eic, ps, pe, tidy_output = TRUE),
      day_ahead   = entsoeapi::load_day_ahead_total_forecast(eic, ps, pe, tidy_output = TRUE),
      week_ahead  = entsoeapi::load_week_ahead_total_forecast(eic, ps, pe, tidy_output = TRUE),
      month_ahead = entsoeapi::load_month_ahead_total_forecast(eic, ps, pe, tidy_output = TRUE),
      year_ahead  = entsoeapi::load_year_ahead_total_forecast(eic, ps, pe, tidy_output = TRUE),
      margin      = entsoeapi::load_year_ahead_forecast_margin(eic, ps, pe, tidy_output = TRUE),
      stop("Unknown type '", type, "'. Use: actual, day_ahead, week_ahead, month_ahead, year_ahead, margin")
    )
    safe_to_csv(df)
  },
  description = "Get load data for a bidding zone. Max 1-year range.
type: 'actual' (6.1.A, default), 'day_ahead' (6.1.B), 'week_ahead' (6.1.C),
'month_ahead' (6.1.D), 'year_ahead' (6.1.E), 'margin' (8.1 year-ahead margin).
Use area_eic() to find the EIC code.",
  arguments = list(
    eic          = ellmer::type_string("EIC code of the bidding zone, e.g. '10Y1001A1001A83F' for Germany."),
    period_start = ellmer::type_string("Start date YYYY-MM-DD (CET). Keep to 1-7 days — data is at 15-min to 1-hour resolution, capped at 100 rows."),
    period_end   = ellmer::type_string("End date YYYY-MM-DD (CET)."),
    type         = ellmer::type_string(
      "Load series: 'actual' (default), 'day_ahead', 'week_ahead', 'month_ahead', 'year_ahead', 'margin'.",
      required = FALSE
    )
  )
)


# ============================================================
# Generation Tools
# ============================================================

tool_gen_time_series <- ellmer::tool(
  name = "gen_time_series",
  fun = function(eic, period_start, period_end, type = "actual", gen_type = NULL) {
    ps <- parse_date(period_start)
    pe <- parse_date(period_end)
    df <- switch(type,
      actual     = entsoeapi::gen_per_prod_type(eic, ps, pe, gen_type = gen_type, tidy_output = TRUE),
      wind_solar = entsoeapi::gen_wind_solar_forecasts(eic, ps, pe, tidy_output = TRUE),
      day_ahead  = entsoeapi::gen_day_ahead_forecast(eic, ps, pe, tidy_output = TRUE),
      per_unit   = entsoeapi::gen_per_gen_unit(eic, ps, pe, gen_type = gen_type, tidy_output = TRUE),
      storage    = entsoeapi::gen_storage_mean_filling_rate(eic, ps, pe, tidy_output = TRUE),
      stop("Unknown type '", type, "'. Use: actual, wind_solar, day_ahead, per_unit, storage")
    )
    safe_to_csv(df)
  },
  description = "Get generation time-series for a bidding zone. Max 1-year range.
type: 'actual' (16.1.B&C per production type, default), 'wind_solar' (14.1.D forecast),
'day_ahead' (14.1.C aggregate forecast), 'per_unit' (16.1.A per unit, slow),
'storage' (16.1.D hydro reservoir filling rate).
gen_type: optional ENTSO-E production type code used with 'actual'/'per_unit',
e.g. 'B01' Biomass, 'B16' Solar. 'actual' and 'per_unit' are paginated daily.",
  arguments = list(
    eic          = ellmer::type_string("EIC code of the bidding zone or control area."),
    period_start = ellmer::type_string("Start date YYYY-MM-DD (CET). Keep to 1-7 days — data is at 15-min to 1-hour resolution, capped at 100 rows."),
    period_end   = ellmer::type_string("End date YYYY-MM-DD (CET)."),
    type         = ellmer::type_string(
      "Generation series: 'actual' (default), 'wind_solar', 'day_ahead', 'per_unit', 'storage'.",
      required = FALSE
    ),
    gen_type     = ellmer::type_string(
      "Optional ENTSO-E production type code, e.g. 'B01' Biomass, 'B16' Solar. Used with 'actual' and 'per_unit'.",
      required = FALSE
    )
  )
)


tool_gen_capacity <- ellmer::tool(
  name = "gen_capacity",
  fun = function(eic, year, per_unit = FALSE, psr_type = NULL) {
    fn <- if (isTRUE(per_unit)) {
      entsoeapi::gen_installed_capacity_per_pu
    } else {
      entsoeapi::gen_installed_capacity_per_pt
    }
    fn(eic = eic, year = as.integer(year), psr_type = psr_type, tidy_output = TRUE) |>
      safe_to_csv()
  },
  description = "Get installed generation capacity for a bidding zone and year.
per_unit=FALSE (default): aggregate by production type (14.1.A).
per_unit=TRUE: individual production unit level (14.1.C, more granular).",
  arguments = list(
    eic      = ellmer::type_string("EIC code of the bidding zone."),
    year     = ellmer::type_integer("Year as integer, e.g. 2024."),
    per_unit = ellmer::type_boolean(
      "FALSE (default) = per production type (14.1.A). TRUE = per production unit (14.1.C).",
      required = FALSE
    ),
    psr_type = ellmer::type_string(
      "Optional PSR type code, e.g. 'B16' for Solar PV.",
      required = FALSE
    )
  )
)


# ============================================================
# Market Tools
# ============================================================

tool_energy_prices <- ellmer::tool(
  name = "energy_prices",
  fun = \(eic, period_start, period_end, contract_type = "A01") {
    entsoeapi::energy_prices(
      eic = eic,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      contract_type = contract_type,
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = "Get day-ahead or intraday market clearing prices (12.1.D) for a bidding zone. Max 1-year range. contract_type: 'A01' = Day ahead (default), 'A07' = Intraday. Use area_eic() to find the EIC code.",
  arguments = list(
    eic = ellmer::type_string(description = "EIC code of the bidding zone."),
    period_start = ellmer::type_string(
      description = "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible — data is at 15-min to 1-hour resolution and results are capped at 100 rows."
    ),
    period_end = ellmer::type_string(
      description = "End date YYYY-MM-DD (CET)."
    ),
    contract_type = ellmer::type_string(
      description = "Contract type: 'A01' for Day ahead (default), 'A07' for Intraday.",
      required = FALSE
    )
  )
)


tool_intraday_prices <- ellmer::tool(
  name = "intraday_prices",
  fun = \(eic, period_start, period_end) {
    entsoeapi::intraday_prices(
      eic = eic,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = "Get intraday market prices for a bidding zone. Specialised wrapper around the intraday contract type. Max 1-year range.",
  arguments = list(
    eic = ellmer::type_string(description = "EIC code of the bidding zone."),
    period_start = ellmer::type_string(
      description = "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible — data is at 15-min to 1-hour resolution and results are capped at 100 rows."
    ),
    period_end = ellmer::type_string(
      description = "End date YYYY-MM-DD (CET)."
    )
  )
)


tool_net_transfer_capacities <- ellmer::tool(
  name = "net_transfer_capacities",
  fun = \(eic_in, eic_out, period_start, period_end, contract_type = "A02") {
    entsoeapi::net_transfer_capacities(
      eic_in = eic_in,
      eic_out = eic_out,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      contract_type = contract_type,
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = "Get Net Transfer Capacities (NTC) between two bidding zones (11.1). contract_type: 'A02' = Day ahead (default), 'A03' = Intraday, 'A04' = Week ahead. Use area_eic() to find EIC codes for both zones.",
  arguments = list(
    eic_in = ellmer::type_string(
      description = "EIC code of the importing bidding zone."
    ),
    eic_out = ellmer::type_string(
      description = "EIC code of the exporting bidding zone."
    ),
    period_start = ellmer::type_string(
      description = "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible — data is at 15-min to 1-hour resolution and results are capped at 100 rows."
    ),
    period_end = ellmer::type_string(
      description = "End date YYYY-MM-DD (CET)."
    ),
    contract_type = ellmer::type_string(
      description = "Contract type: 'A02' Day ahead (default), 'A03' Intraday, 'A04' Week ahead.",
      required = FALSE
    )
  )
)


tool_day_ahead_commercial_sched <- ellmer::tool(
  name = "day_ahead_commercial_sched",
  fun = \(eic_in, eic_out, period_start, period_end) {
    entsoeapi::day_ahead_commercial_sched(
      eic_in = eic_in,
      eic_out = eic_out,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = "Get day-ahead commercial schedules (12.1.F) between two bidding zones. Max 1-year range. Use area_eic() to find EIC codes.",
  arguments = list(
    eic_in = ellmer::type_string(
      description = "EIC code of the importing bidding zone."
    ),
    eic_out = ellmer::type_string(
      description = "EIC code of the exporting bidding zone."
    ),
    period_start = ellmer::type_string(
      description = "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible — data is at 15-min to 1-hour resolution and results are capped at 100 rows."
    ),
    period_end = ellmer::type_string(
      description = "End date YYYY-MM-DD (CET)."
    )
  )
)


tool_explicit_offered_transfer_capacities <- ellmer::tool(
  name = "explicit_offered_transfer_capacities",
  fun = \(eic_in, eic_out, period_start, period_end, contract_type = "A01") {
    entsoeapi::explicit_offered_transfer_capacities(
      eic_in = eic_in,
      eic_out = eic_out,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      contract_type = contract_type,
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = "Get explicit offered transfer capacities (12.1.A) between two bidding zones. contract_type: 'A01' = Day ahead (default).",
  arguments = list(
    eic_in = ellmer::type_string(
      description = "EIC code of the importing bidding zone."
    ),
    eic_out = ellmer::type_string(
      description = "EIC code of the exporting bidding zone."
    ),
    period_start = ellmer::type_string(
      description = "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible — data is at 15-min to 1-hour resolution and results are capped at 100 rows."
    ),
    period_end = ellmer::type_string(
      description = "End date YYYY-MM-DD (CET)."
    ),
    contract_type = ellmer::type_string(
      description = "Contract type, default 'A01' (Day ahead).",
      required = FALSE
    )
  )
)


tool_flow_based_allocations <- ellmer::tool(
  name = "flow_based_allocations",
  fun = \(eic, period_start, period_end, process_type = "A43") {
    entsoeapi::flow_based_allocations(
      eic = eic,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      process_type = process_type,
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = "Get flow-based allocations (12.1.B) for the CWE/Core region. process_type: 'A43' = Day ahead (default), 'A32' = Intraday.",
  arguments = list(
    eic = ellmer::type_string(
      description = "EIC code of the bidding zone (CWE/Core region)."
    ),
    period_start = ellmer::type_string(
      description = "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible — data is at 15-min to 1-hour resolution and results are capped at 100 rows."
    ),
    period_end = ellmer::type_string(
      description = "End date YYYY-MM-DD (CET)."
    ),
    process_type = ellmer::type_string(
      description = "Process type: 'A43' Day ahead (default), 'A32' Intraday.",
      required = FALSE
    )
  )
)


tool_congestion_income <- ellmer::tool(
  name = "congestion_income",
  fun = \(eic, period_start, period_end, contract_type = "A01") {
    entsoeapi::congestion_income(
      eic = eic,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      contract_type = contract_type,
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = "Get congestion income (12.1.G) for a bidding zone or border. contract_type: 'A01' = Day ahead (default).",
  arguments = list(
    eic = ellmer::type_string(description = "EIC code of the bidding zone."),
    period_start = ellmer::type_string(
      description = "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible — data is at 15-min to 1-hour resolution and results are capped at 100 rows."
    ),
    period_end = ellmer::type_string(
      description = "End date YYYY-MM-DD (CET)."
    ),
    contract_type = ellmer::type_string(
      description = "Contract type, default 'A01' (Day ahead).",
      required = FALSE
    )
  )
)


tool_allocated_transfer_capacities_3rd_countries <- ellmer::tool(
  name = "allocated_transfer_capacities_3rd_countries",
  fun = \(eic_in, eic_out, period_start, period_end, contract_type = "A01",
          auction_category = "A04") {
    entsoeapi::allocated_transfer_capacities_3rd_countries(
      eic_in = eic_in,
      eic_out = eic_out,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      contract_type = contract_type,
      auction_category = auction_category,
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = "Get allocated transfer capacities for third-country borders (12.1.C). auction_category: 'A01' Annual, 'A02' Monthly, 'A03' Weekly, 'A04' Daily (default).",
  arguments = list(
    eic_in = ellmer::type_string(
      description = "EIC code of the importing bidding zone."
    ),
    eic_out = ellmer::type_string(
      description = "EIC code of the exporting bidding zone."
    ),
    period_start = ellmer::type_string(
      description = "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible — data is at 15-min to 1-hour resolution and results are capped at 100 rows."
    ),
    period_end = ellmer::type_string(
      description = "End date YYYY-MM-DD (CET)."
    ),
    contract_type = ellmer::type_string(
      description = "Contract type, default 'A01' (Day ahead).",
      required = FALSE
    ),
    auction_category = ellmer::type_string(
      description = "Auction category: 'A01' Annual, 'A02' Monthly, 'A03' Weekly, 'A04' Daily (default).",
      required = FALSE
    )
  )
)


# ============================================================
# Transmission Tools
# ============================================================

tool_cross_border_physical_flows <- ellmer::tool(
  name = "cross_border_physical_flows",
  fun = \(eic_in, eic_out, period_start, period_end) {
    entsoeapi::cross_border_physical_flows(
      eic_in = eic_in,
      eic_out = eic_out,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = "Get physical cross-border flows (12.1.G) between two bidding zones. Max 1-year range. Use area_eic() to find EIC codes.",
  arguments = list(
    eic_in = ellmer::type_string(
      description = "EIC code of the importing bidding zone."
    ),
    eic_out = ellmer::type_string(
      description = "EIC code of the exporting bidding zone."
    ),
    period_start = ellmer::type_string(
      description = "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible — data is at 15-min to 1-hour resolution and results are capped at 100 rows."
    ),
    period_end = ellmer::type_string(
      description = "End date YYYY-MM-DD (CET)."
    )
  )
)


tool_total_commercial_sched <- ellmer::tool(
  name = "total_commercial_sched",
  fun = \(eic_in, eic_out, period_start, period_end) {
    entsoeapi::total_commercial_sched(
      eic_in = eic_in,
      eic_out = eic_out,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = "Get total commercial schedules (12.1.F) between two bidding zones. Max 1-year range.",
  arguments = list(
    eic_in = ellmer::type_string(
      description = "EIC code of the importing bidding zone."
    ),
    eic_out = ellmer::type_string(
      description = "EIC code of the exporting bidding zone."
    ),
    period_start = ellmer::type_string(
      description = "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible — data is at 15-min to 1-hour resolution and results are capped at 100 rows."
    ),
    period_end = ellmer::type_string(
      description = "End date YYYY-MM-DD (CET)."
    )
  )
)


tool_net_positions <- ellmer::tool(
  name = "net_positions",
  fun = \(eic, period_start, period_end, contract_type = "A01") {
    entsoeapi::net_positions(
      eic = eic,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      contract_type = contract_type,
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = "Get net positions (12.1.H) for a bidding zone. contract_type: 'A01' = Day ahead (default), 'A07' = Intraday. Max 1-year range.",
  arguments = list(
    eic = ellmer::type_string(description = "EIC code of the bidding zone."),
    period_start = ellmer::type_string(
      description = "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible — data is at 15-min to 1-hour resolution and results are capped at 100 rows."
    ),
    period_end = ellmer::type_string(
      description = "End date YYYY-MM-DD (CET)."
    ),
    contract_type = ellmer::type_string(
      description = "Contract type: 'A01' Day ahead (default), 'A07' Intraday.",
      required = FALSE
    )
  )
)


tool_forecasted_transfer_capacities <- ellmer::tool(
  name = "forecasted_transfer_capacities",
  fun = \(eic_in, eic_out, period_start, period_end,
          market_agreement_type = "A01") {
    entsoeapi::forecasted_transfer_capacities(
      eic_in = eic_in,
      eic_out = eic_out,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      market_agreement_type = market_agreement_type,
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = "Get forecasted transfer capacities (11.1) between two bidding zones. market_agreement_type: 'A01' = Day ahead (default), 'A02' = Total capacity.",
  arguments = list(
    eic_in = ellmer::type_string(
      description = "EIC code of the importing bidding zone."
    ),
    eic_out = ellmer::type_string(
      description = "EIC code of the exporting bidding zone."
    ),
    period_start = ellmer::type_string(
      description = "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible — data is at 15-min to 1-hour resolution and results are capped at 100 rows."
    ),
    period_end = ellmer::type_string(
      description = "End date YYYY-MM-DD (CET)."
    ),
    market_agreement_type = ellmer::type_string(
      description = "Market agreement type: 'A01' Day ahead (default), 'A02' Total capacity.",
      required = FALSE
    )
  )
)


# ============================================================
# Outages Tools
# ============================================================

tool_outages_gen_units <- ellmer::tool(
  name = "outages_gen_units",
  fun = \(eic, period_start, period_end, doc_status = NULL,
          event_nature = NULL) {
    entsoeapi::outages_gen_units(
      eic = eic,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      doc_status = doc_status,
      event_nature = event_nature,
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = "Get unavailability of generation units (15.1.A&B) for a bidding zone. Max 1-year range. doc_status: 'A05' active, 'A09' cancelled, 'A13' withdrawn (NULL = active+cancelled). event_nature: 'A53' planned, 'A54' unplanned (NULL = both).",
  arguments = list(
    eic = ellmer::type_string(
      description = "EIC code of the bidding zone or control area."
    ),
    period_start = ellmer::type_string(
      description = "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible — data is at 15-min to 1-hour resolution and results are capped at 100 rows."
    ),
    period_end = ellmer::type_string(
      description = "End date YYYY-MM-DD (CET)."
    ),
    doc_status = ellmer::type_string(
      description = "Document status filter: 'A05' active, 'A09' cancelled, 'A13' withdrawn.",
      required = FALSE
    ),
    event_nature = ellmer::type_string(
      description = "Event nature: 'A53' planned maintenance, 'A54' unplanned outage.",
      required = FALSE
    )
  )
)


tool_outages_prod_units <- ellmer::tool(
  name = "outages_prod_units",
  fun = \(eic, period_start, period_end, doc_status = NULL,
          event_nature = NULL) {
    entsoeapi::outages_prod_units(
      eic = eic,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      doc_status = doc_status,
      event_nature = event_nature,
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = "Get unavailability of production units (15.1.C&D) for a bidding zone. Max 1-year range. doc_status: 'A05' active, 'A09' cancelled, 'A13' withdrawn. event_nature: 'A53' planned, 'A54' unplanned.",
  arguments = list(
    eic = ellmer::type_string(
      description = "EIC code of the bidding zone or control area."
    ),
    period_start = ellmer::type_string(
      description = "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible — data is at 15-min to 1-hour resolution and results are capped at 100 rows."
    ),
    period_end = ellmer::type_string(
      description = "End date YYYY-MM-DD (CET)."
    ),
    doc_status = ellmer::type_string(
      description = "Document status filter: 'A05' active, 'A09' cancelled, 'A13' withdrawn.",
      required = FALSE
    ),
    event_nature = ellmer::type_string(
      description = "Event nature: 'A53' planned maintenance, 'A54' unplanned outage.",
      required = FALSE
    )
  )
)


tool_outages_transmission_grid <- ellmer::tool(
  name = "outages_transmission_grid",
  fun = \(eic_in, eic_out, period_start, period_end, doc_status = NULL,
          event_nature = NULL) {
    entsoeapi::outages_transmission_grid(
      eic_in = eic_in,
      eic_out = eic_out,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      doc_status = doc_status,
      event_nature = event_nature,
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = "Get unavailability in the transmission grid (15.1.A&B) on a border. Max 1-year range. Requires both eic_in and eic_out for the affected border. doc_status: 'A05' active, 'A09' cancelled, 'A13' withdrawn. event_nature: 'A53' planned, 'A54' unplanned.",
  arguments = list(
    eic_in = ellmer::type_string(
      description = "EIC code of the importing side of the affected border."
    ),
    eic_out = ellmer::type_string(
      description = "EIC code of the exporting side of the affected border."
    ),
    period_start = ellmer::type_string(
      description = "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible — data is at 15-min to 1-hour resolution and results are capped at 100 rows."
    ),
    period_end = ellmer::type_string(
      description = "End date YYYY-MM-DD (CET)."
    ),
    doc_status = ellmer::type_string(
      description = "Document status filter: 'A05' active, 'A09' cancelled, 'A13' withdrawn.",
      required = FALSE
    ),
    event_nature = ellmer::type_string(
      description = "Event nature: 'A53' planned maintenance, 'A54' unplanned outage.",
      required = FALSE
    )
  )
)


# ============================================================
# Balancing Tools
# ============================================================

tool_imbalance_prices <- ellmer::tool(
  name = "imbalance_prices",
  fun = \(eic, period_start, period_end) {
    entsoeapi::imbalance_prices(
      eic = eic,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = "Get imbalance prices (17.1.D) for a scheduling area. Max 1-year range. Use area_eic() to find the EIC code.",
  arguments = list(
    eic = ellmer::type_string(
      description = "EIC code of the scheduling/control area."
    ),
    period_start = ellmer::type_string(
      description = "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible — data is at 15-min to 1-hour resolution and results are capped at 100 rows."
    ),
    period_end = ellmer::type_string(
      description = "End date YYYY-MM-DD (CET)."
    )
  )
)


tool_imbalance_volumes <- ellmer::tool(
  name = "imbalance_volumes",
  fun = \(eic, period_start, period_end) {
    entsoeapi::imbalance_volumes(
      eic = eic,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = "Get imbalance volumes (17.1.C) for a scheduling area. Max 1-year range.",
  arguments = list(
    eic = ellmer::type_string(
      description = "EIC code of the scheduling/control area."
    ),
    period_start = ellmer::type_string(
      description = "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible — data is at 15-min to 1-hour resolution and results are capped at 100 rows."
    ),
    period_end = ellmer::type_string(
      description = "End date YYYY-MM-DD (CET)."
    )
  )
)


tool_contracted_reserves <- ellmer::tool(
  name = "contracted_reserves",
  fun = \(
    eic, market_agreement_type, period_start, period_end, process_type = NULL
  ) {
    entsoeapi::contracted_reserves(
      eic = eic,
      market_agreement_type = market_agreement_type,
      process_type = process_type,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = "Get contracted balancing reserves (17.1.B) for a scheduling area. market_agreement_type (required): 'A01' Daily, 'A02' Weekly, 'A03' Monthly, 'A04' Yearly, 'A06' Long-term, 'A13' Intraday. process_type (optional): 'A46' FCR, 'A47' mFRR, 'A51' aFRR, 'A52' RR.",
  arguments = list(
    eic = ellmer::type_string(
      description = "EIC code of the scheduling/control area."
    ),
    market_agreement_type = ellmer::type_string(
      description = "Required. Agreement type: 'A01' Daily, 'A02' Weekly, 'A03' Monthly, 'A04' Yearly, 'A06' Long-term, 'A13' Intraday."
    ),
    period_start = ellmer::type_string(
      description = "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where possible — data is at 15-min to 1-hour resolution and results are capped at 100 rows."
    ),
    period_end = ellmer::type_string(
      description = "End date YYYY-MM-DD (CET)."
    ),
    process_type = ellmer::type_string(
      description = "Optional reserve type: 'A46' FCR, 'A47' mFRR, 'A51' aFRR, 'A52' RR.",
      required = FALSE
    )
  )
)
