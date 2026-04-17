# ============================================================
# EIC Lookup Tools (no date params)
# ============================================================

# Helper: keep only identification-relevant columns and filter rows by
# a query string.
# Drops rarely-needed metadata (postal_code, vat_code, parent,
# responsible_party, status) to reduce context window usage.
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


#' @importFrom ellmer tool type_string type_integer type_boolean
#' @importFrom entsoeapi area_eic party_eic all_approved_eic resource_object_eic
#'   get_news load_actual_total load_day_ahead_total_forecast congestion_income
#'   load_week_ahead_total_forecast load_month_ahead_total_forecast
#'   load_year_ahead_total_forecast load_year_ahead_forecast_margin
#'   gen_per_prod_type gen_wind_solar_forecasts gen_day_ahead_forecast
#'   gen_per_gen_unit gen_storage_mean_filling_rate net_transfer_capacities
#'   gen_installed_capacity_per_pt gen_installed_capacity_per_pu energy_prices
#'   intraday_prices day_ahead_commercial_sched
#'   explicit_offered_transfer_capacities flow_based_allocations
#'   allocated_transfer_capacities_3rd_countries cross_border_physical_flows
#'   total_commercial_sched net_positions forecasted_transfer_capacities
#'   outages_gen_units outages_prod_units outages_transmission_grid
#'   imbalance_prices imbalance_volumes contracted_reserves
tool_area_eic <- tool(
  name = "area_eic",
  fun = \(query = NULL) {
    area_eic() |>
      .eic_filter(query) |>
      safe_to_csv(max_rows = 200L)
  },
  description = paste(
    "Return Area Y EIC codes (bidding zones, control areas, member states,",
    "etc.). Pass a query string to filter by country name, ISO code, or zone",
    "name (case-insensitive). IMPORTANT: Always display the full table of",
    "matches to the user and ask them to confirm which EIC code to use before",
    "querying data - many countries have multiple bidding zones."
  ),
  arguments = list(
    query = type_string(
      description = paste(
        "Optional filter string, e.g. 'Germany', 'DE', 'France',",
        "'Bidding Zone'. Case-insensitive match against display name, long",
        "name, and country code."
      ),
      required = FALSE
    )
  )
)


tool_party_eic <- tool(
  name = "party_eic",
  fun = \(query = NULL) {
    party_eic() |>
      .eic_filter(query) |>
      safe_to_csv(max_rows = 200L)
  },
  description = paste(
    "Return Party X EIC codes for market participants (TSOs, traders, etc.).",
    "Pass a query string to filter by name or country code."
  ),
  arguments = list(
    query = type_string(
      description = paste(
        "Optional filter string, e.g. 'Germany', 'TSO', 'DE'.",
        "Case-insensitive."
      ),
      required = FALSE
    )
  )
)


tool_all_approved_eic <- tool(
  name = "all_approved_eic",
  fun = \(query = NULL) {
    all_approved_eic() |>
      .eic_filter(query) |>
      safe_to_csv(max_rows = 200L)
  },
  description = paste(
    "Return all approved EIC codes across all types (Area, Party, Tie-line,",
    "Resource Object, Substation, etc.) in one table. Slower than area_eic()",
    "alone. Pass a query string to filter results."
  ),
  arguments = list(
    query = type_string(
      description = paste(
        "Optional filter string, e.g. 'Germany', 'DE', 'Tie-line'.",
        "Case-insensitive."
      ),
      required = FALSE
    )
  )
)


tool_resource_object_eic <- tool(
  name = "resource_object_eic",
  fun = \(query = NULL) {
    resource_object_eic() |>
      .eic_filter(query) |>
      safe_to_csv(max_rows = 200L)
  },
  description = paste(
    "Return Resource Object W EIC codes - generation units and other",
    "resources. Pass a query string to filter by plant name, country code,",
    "or fuel type."
  ),
  arguments = list(
    query = type_string(
      description = paste(
        "Optional filter string, e.g. 'wind', 'DE', 'nuclear'.",
        "Case-insensitive."
      ),
      required = FALSE
    )
  )
)


tool_get_news <- tool(
  name = "get_news",
  fun = \(n = 10L) {
    get_news(n = as.integer(n)) |>
      safe_to_csv()
  },
  description = paste(
    "Fetch the latest news items from the ENTSO-E Transparency Platform RSS",
    "feed. Useful for checking maintenance windows or data availability",
    "issues."
  ),
  arguments = list(
    n = type_integer(
      description = "Number of news items to return. Defaults to 10.",
      required = FALSE
    )
  )
)


# ============================================================
# Load Tools
# ============================================================

tool_load <- tool(
  name = "load",
  fun = \(eic, period_start, period_end, type = "actual") {
    ps <- parse_date(period_start)
    pe <- parse_date(period_end)
    switch(type,
      actual = load_actual_total(eic, ps, pe, tidy_output = TRUE),
      day_ahead = load_day_ahead_total_forecast(
        eic, ps, pe, tidy_output = TRUE
      ),
      week_ahead = load_week_ahead_total_forecast(
        eic, ps, pe, tidy_output = TRUE
      ),
      month_ahead = load_month_ahead_total_forecast(
        eic, ps, pe, tidy_output = TRUE
      ),
      year_ahead = load_year_ahead_total_forecast(
        eic, ps, pe, tidy_output = TRUE
      ),
      margin = load_year_ahead_forecast_margin(
        eic, ps, pe, tidy_output = TRUE
      ),
      stop(
        "Unknown type '", type, "'. Use: actual, day_ahead, week_ahead,",
        " month_ahead, year_ahead, margin"
      )
    ) |>
      safe_to_csv()
  },
  description = paste(
    "Get load data for a bidding zone. Prefer 1-3 day date ranges - 15-min",
    "data fills the 100-row cap in ~1 day; hourly data allows ~4 days.",
    "Max 1-year API limit. type: 'actual' (6.1.A, default), 'day_ahead'",
    "(6.1.B), 'week_ahead' (6.1.C), 'month_ahead' (6.1.D), 'year_ahead'",
    "(6.1.E), 'margin' (8.1 year-ahead margin). Use area_eic() to find the",
    "EIC code."
  ),
  arguments = list(
    eic  = type_string(
      description = paste(
        "EIC code of the bidding zone, e.g. '10Y1001A1001A83F' for Germany."
      )
    ),
    period_start = type_string(
      description = paste(
        "Start date YYYY-MM-DD (CET). Keep to 1-7 days - data is at 15-min to",
        "1-hour resolution, capped at 100 rows."
      )
    ),
    period_end = type_string("End date YYYY-MM-DD (CET)."),
    type = type_string(
      description = paste(
        "Load series: 'actual' (default), 'day_ahead', 'week_ahead',",
        "'month_ahead', 'year_ahead', 'margin'."
      ),
      required = FALSE
    )
  )
)


# ============================================================
# Generation Tools
# ============================================================

tool_gen_time_series <- tool(
  name = "gen_time_series",
  fun = \(eic, period_start, period_end, type = "actual", gen_type = NULL) {
    ps <- parse_date(period_start)
    pe <- parse_date(period_end)
    switch(
      type,
      actual = gen_per_prod_type(
        eic, ps, pe, gen_type = gen_type, tidy_output = TRUE
      ),
      wind_solar = gen_wind_solar_forecasts(eic, ps, pe, tidy_output = TRUE),
      day_ahead = gen_day_ahead_forecast(eic, ps, pe, tidy_output = TRUE),
      per_unit = gen_per_gen_unit(
        eic, ps, pe, gen_type = gen_type, tidy_output = TRUE
      ),
      storage = gen_storage_mean_filling_rate(eic, ps, pe, tidy_output = TRUE),
      stop(
        "Unknown type '", type,
        "'. Use: actual, wind_solar, day_ahead, per_unit, storage"
      )
    ) |>
      safe_to_csv()
  },
  description = paste(
    "Get generation time-series for a bidding zone. Prefer 1-3 day date ranges",
    "- 15-min data fills the 100-row cap in ~1 day; hourly data allows ~4",
    "days. Max 1-year API limit. type: 'actual' (16.1.B&C per production type,",
    "default), 'wind_solar' (14.1.D forecast), 'day_ahead' (14.1.C aggregate",
    "forecast), 'per_unit' (16.1.A per unit, slow), 'storage' (16.1.D hydro",
    "reservoir filling rate). gen_type: optional ENTSO-E production type code",
    "used with 'actual'/'per_unit', e.g. 'B01' Biomass, 'B16' Solar. 'actual'",
    "and 'per_unit' are paginated daily."
  ),
  arguments = list(
    eic  = type_string("EIC code of the bidding zone or control area."),
    period_start = type_string(
      description = paste(
        "Start date YYYY-MM-DD (CET). Keep to 1-7 days - data is at 15-min to",
        "1-hour resolution, capped at 100 rows."
      )
    ),
    period_end = type_string("End date YYYY-MM-DD (CET)."),
    type = type_string(
      description = paste(
        "Generation series: 'actual' (default), 'wind_solar', 'day_ahead',",
        "'per_unit', 'storage'."
      ),
      required = FALSE
    ),
    gen_type = type_string(
      description = paste(
        "Optional ENTSO-E production type code, e.g. 'B01' Biomass, 'B16'",
        "Solar. Used with 'actual' and 'per_unit'."
      ),
      required = FALSE
    )
  )
)


tool_gen_capacity <- tool(
  name = "gen_capacity",
  fun = \(eic, year, per_unit = FALSE, psr_type = NULL) {
    fn <- if (isTRUE(per_unit)) {
      gen_installed_capacity_per_pu
    } else {
      gen_installed_capacity_per_pt
    }
    fn(
      eic = eic,
      year = as.integer(year),
      psr_type = psr_type,
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = paste(
    "Get installed generation capacity for a bidding zone and year.",
    "per_unit=FALSE (default): aggregate by production type (14.1.A).",
    "per_unit=TRUE: individual production unit level (14.1.C, more granular)."
  ),
  arguments = list(
    eic = type_string("EIC code of the bidding zone."),
    year = type_integer("Year as integer, e.g. 2024."),
    per_unit = type_boolean(
      description = paste(
        "FALSE (default) = per production type (14.1.A). TRUE = per",
        "production unit (14.1.C)."
      ),
      required = FALSE
    ),
    psr_type = type_string(
      description = "Optional PSR type code, e.g. 'B16' for Solar PV.",
      required = FALSE
    )
  )
)


# ============================================================
# Market Tools
# ============================================================

tool_energy_prices <- tool(
  name = "energy_prices",
  fun = \(eic, period_start, period_end, contract_type = "A01") {
    energy_prices(
      eic = eic,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      contract_type = contract_type,
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = paste(
    "Get day-ahead or intraday market clearing prices (12.1.D) for a bidding",
    "zone. Prefer 1-3 day date ranges (results capped at 100 rows). Max 1-year",
    "API limit. contract_type: 'A01' = Day ahead (default), 'A07' = Intraday.",
    "Use area_eic() to find the EIC code."
  ),
  arguments = list(
    eic = type_string(description = "EIC code of the bidding zone."),
    period_start = type_string(
      description = paste(
        "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where",
        "possible - data is at 15-min to 1-hour resolution and results are",
        "capped at 100 rows."
      )
    ),
    period_end = type_string(
      description = "End date YYYY-MM-DD (CET)."
    ),
    contract_type = type_string(
      description = paste(
        "Contract type: 'A01' for Day ahead (default), 'A07' for Intraday."
      ),
      required = FALSE
    )
  )
)


tool_intraday_prices <- tool(
  name = "intraday_prices",
  fun = \(eic, period_start, period_end) {
    intraday_prices(
      eic = eic,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = paste(
    "Get intraday market prices for a bidding zone. Specialised wrapper around",
    "the intraday contract type. Prefer 1-3 day date ranges (results capped at",
    "100 rows). Max 1-year API limit."
  ),
  arguments = list(
    eic = type_string(description = "EIC code of the bidding zone."),
    period_start = type_string(
      description = paste(
        "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where",
        "possible - data is at 15-min to 1-hour resolution and results are",
        "capped at 100 rows."
      )
    ),
    period_end = type_string(
      description = "End date YYYY-MM-DD (CET)."
    )
  )
)


tool_net_transfer_capacities <- tool(
  name = "net_transfer_capacities",
  fun = \(eic_in, eic_out, period_start, period_end, contract_type = "A02") {
    net_transfer_capacities(
      eic_in = eic_in,
      eic_out = eic_out,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      contract_type = contract_type,
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = paste(
    "Get Net Transfer Capacities (NTC) between two bidding zones (11.1).",
    "Prefer 1-3 day date ranges (results capped at 100 rows). contract_type:",
    "'A02' = Day ahead (default), 'A03' = Intraday, 'A04' = Week ahead.",
    "Use area_eic() to find EIC codes for both zones."
  ),
  arguments = list(
    eic_in = type_string(
      description = "EIC code of the importing bidding zone."
    ),
    eic_out = type_string(
      description = "EIC code of the exporting bidding zone."
    ),
    period_start = type_string(
      description = paste(
        "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where",
        "possible - data is at 15-min to 1-hour resolution and results are",
        "capped at 100 rows."
      )
    ),
    period_end = type_string(
      description = "End date YYYY-MM-DD (CET)."
    ),
    contract_type = type_string(
      description = paste(
        "Contract type: 'A02' Day ahead (default), 'A03' Intraday, 'A04'",
        "Week ahead."
      ),
      required = FALSE
    )
  )
)


tool_day_ahead_commercial_sched <- tool( # nolint: object_length_linter
  name = "day_ahead_commercial_sched",
  fun = \(eic_in, eic_out, period_start, period_end) {
    day_ahead_commercial_sched(
      eic_in = eic_in,
      eic_out = eic_out,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = paste(
    "Get day-ahead commercial schedules (12.1.F) between two bidding zones.",
    "Prefer 1-3 day date ranges (results capped at 100 rows). Max 1-year API",
    "limit. Use area_eic() to find EIC codes."
  ),
  arguments = list(
    eic_in = type_string(
      description = "EIC code of the importing bidding zone."
    ),
    eic_out = type_string(
      description = "EIC code of the exporting bidding zone."
    ),
    period_start = type_string(
      description = paste(
        "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where",
        "possible - data is at 15-min to 1-hour resolution and results are",
        "capped at 100 rows."
      )
    ),
    period_end = type_string(
      description = "End date YYYY-MM-DD (CET)."
    )
  )
)


tool_explicit_offered_transfer_capacities <- tool( # nolint: object_length_linter
  name = "explicit_offered_transfer_capacities",
  fun = \(eic_in, eic_out, period_start, period_end, contract_type = "A01") {
    explicit_offered_transfer_capacities(
      eic_in = eic_in,
      eic_out = eic_out,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      contract_type = contract_type,
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = paste(
    "Get explicit offered transfer capacities (12.1.A) between two bidding",
    "zones. Prefer 1-3 day date ranges (results capped at 100 rows).",
    "contract_type: 'A01' = Day ahead (default)."
  ),
  arguments = list(
    eic_in = type_string(
      description = "EIC code of the importing bidding zone."
    ),
    eic_out = type_string(
      description = "EIC code of the exporting bidding zone."
    ),
    period_start = type_string(
      description = paste(
        "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where",
        "possible - data is at 15-min to 1-hour resolution and results are",
        "capped at 100 rows."
      )
    ),
    period_end = type_string(
      description = "End date YYYY-MM-DD (CET)."
    ),
    contract_type = type_string(
      description = "Contract type, default 'A01' (Day ahead).",
      required = FALSE
    )
  )
)


tool_flow_based_allocations <- tool(
  name = "flow_based_allocations",
  fun = \(eic, period_start, period_end, process_type = "A43") {
    flow_based_allocations(
      eic = eic,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      process_type = process_type,
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = paste(
    "Get flow-based allocations (12.1.B) for the CWE/Core region. Prefer 1-3",
    "day date ranges (results capped at 100 rows). process_type: 'A43' = Day",
    "ahead (default), 'A32' = Intraday."
  ),
  arguments = list(
    eic = type_string(
      description = "EIC code of the bidding zone (CWE/Core region)."
    ),
    period_start = type_string(
      description = paste(
        "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where",
        "possible - data is at 15-min to 1-hour resolution and results are",
        "capped at 100 rows."
      )
    ),
    period_end = type_string(
      description = "End date YYYY-MM-DD (CET)."
    ),
    process_type = type_string(
      description = "Process type: 'A43' Day ahead (default), 'A32' Intraday.",
      required = FALSE
    )
  )
)


tool_congestion_income <- tool(
  name = "congestion_income",
  fun = \(eic, period_start, period_end, contract_type = "A01") {
    congestion_income(
      eic = eic,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      contract_type = contract_type,
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = paste(
    "Get congestion income (12.1.G) for a bidding zone or border. Prefer 1-3",
    "day date ranges (results capped at 100 rows). contract_type: 'A01' =",
    "Day ahead (default)."
  ),
  arguments = list(
    eic = type_string(description = "EIC code of the bidding zone."),
    period_start = type_string(
      description = paste(
        "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where",
        "possible - data is at 15-min to 1-hour resolution and results are",
        "capped at 100 rows."
      )
    ),
    period_end = type_string(
      description = "End date YYYY-MM-DD (CET)."
    ),
    contract_type = type_string(
      description = "Contract type, default 'A01' (Day ahead).",
      required = FALSE
    )
  )
)


tool_allocated_transfer_capacities_3rd_countries <- tool( # nolint: object_length_linter
  name = "allocated_transfer_capacities_3rd_countries",
  fun = \(eic_in, eic_out, period_start, period_end, contract_type = "A01",
          auction_category = "A04") {
    allocated_transfer_capacities_3rd_countries(
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
  description = paste(
    "Get allocated transfer capacities for third-country borders (12.1.C).",
    "Prefer 1-3 day date ranges (results capped at 100 rows).",
    "auction_category: 'A01' Annual, 'A02' Monthly, 'A03' Weekly, 'A04'",
    "Daily (default)."
  ),
  arguments = list(
    eic_in = type_string(
      description = "EIC code of the importing bidding zone."
    ),
    eic_out = type_string(
      description = "EIC code of the exporting bidding zone."
    ),
    period_start = type_string(
      description = paste(
        "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where",
        "possible - data is at 15-min to 1-hour resolution and results are",
        "capped at 100 rows."
      )
    ),
    period_end = type_string(
      description = "End date YYYY-MM-DD (CET)."
    ),
    contract_type = type_string(
      description = "Contract type, default 'A01' (Day ahead).",
      required = FALSE
    ),
    auction_category = type_string(
      description = paste(
        "Auction category: 'A01' Annual, 'A02' Monthly, 'A03' Weekly, 'A04'",
        "Daily (default)."
      ),
      required = FALSE
    )
  )
)


# ============================================================
# Transmission Tools
# ============================================================

tool_cross_border_physical_flows <- tool( # nolint: object_length_linter
  name = "cross_border_physical_flows",
  fun = \(eic_in, eic_out, period_start, period_end) {
    cross_border_physical_flows(
      eic_in = eic_in,
      eic_out = eic_out,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = paste(
    "Get physical cross-border flows (12.1.G) between two bidding zones.",
    "Prefer 1-3 day date ranges (results capped at 100 rows). Max 1-year API",
    "limit. Use area_eic() to find EIC codes."
  ),
  arguments = list(
    eic_in = type_string(
      description = "EIC code of the importing bidding zone."
    ),
    eic_out = type_string(
      description = "EIC code of the exporting bidding zone."
    ),
    period_start = type_string(
      description = paste(
        "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where",
        "possible - data is at 15-min to 1-hour resolution and results are",
        "capped at 100 rows."
      )
    ),
    period_end = type_string(
      description = "End date YYYY-MM-DD (CET)."
    )
  )
)


tool_total_commercial_sched <- tool(
  name = "total_commercial_sched",
  fun = \(eic_in, eic_out, period_start, period_end) {
    total_commercial_sched(
      eic_in = eic_in,
      eic_out = eic_out,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = paste(
    "Get total commercial schedules (12.1.F) between two bidding zones.",
    "Prefer 1-3 day date ranges (results capped at 100 rows). Max 1-year",
    "API limit."
  ),
  arguments = list(
    eic_in = type_string(
      description = "EIC code of the importing bidding zone."
    ),
    eic_out = type_string(
      description = "EIC code of the exporting bidding zone."
    ),
    period_start = type_string(
      description = paste(
        "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where",
        "possible - data is at 15-min to 1-hour resolution and results are",
        "capped at 100 rows."
      )
    ),
    period_end = type_string(
      description = "End date YYYY-MM-DD (CET)."
    )
  )
)


tool_net_positions <- tool(
  name = "net_positions",
  fun = \(eic, period_start, period_end, contract_type = "A01") {
    net_positions(
      eic = eic,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      contract_type = contract_type,
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = paste(
    "Get net positions (12.1.H) for a bidding zone. Prefer 1-3 day date ranges",
    "(results capped at 100 rows). Max 1-year API limit. contract_type:",
    "'A01' = Day ahead (default), 'A07' = Intraday."
  ),
  arguments = list(
    eic = type_string(description = "EIC code of the bidding zone."),
    period_start = type_string(
      description = paste(
        "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where",
        "possible - data is at 15-min to 1-hour resolution and results are",
        "capped at 100 rows."
      )
    ),
    period_end = type_string(
      description = "End date YYYY-MM-DD (CET)."
    ),
    contract_type = type_string(
      description = "Contract type: 'A01' Day ahead (default), 'A07' Intraday.",
      required = FALSE
    )
  )
)


tool_forecasted_transfer_capacities <- tool( # nolint: object_length_linter
  name = "forecasted_transfer_capacities",
  fun = \(eic_in, eic_out, period_start, period_end,
          market_agreement_type = "A01") {
    forecasted_transfer_capacities(
      eic_in = eic_in,
      eic_out = eic_out,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      market_agreement_type = market_agreement_type,
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = paste(
    "Get forecasted transfer capacities (11.1) between two bidding zones.",
    "Prefer 1-3 day date ranges (results capped at 100 rows).",
    "market_agreement_type: 'A01' = Day ahead (default), 'A02' =",
    "Total capacity."
  ),
  arguments = list(
    eic_in = type_string(
      description = "EIC code of the importing bidding zone."
    ),
    eic_out = type_string(
      description = "EIC code of the exporting bidding zone."
    ),
    period_start = type_string(
      description = paste(
        "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where",
        "possible - data is at 15-min to 1-hour resolution and results are",
        "capped at 100 rows."
      )
    ),
    period_end = type_string(
      description = "End date YYYY-MM-DD (CET)."
    ),
    market_agreement_type = type_string(
      description = paste(
        "Market agreement type: 'A01' Day ahead (default), 'A02' Total",
        "capacity."
      ),
      required = FALSE
    )
  )
)


# ============================================================
# Outages Tools
# ============================================================

tool_outages_gen_units <- tool(
  name = "outages_gen_units",
  fun = \(eic, period_start, period_end, doc_status = NULL,
          event_nature = NULL) {
    outages_gen_units(
      eic = eic,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      doc_status = doc_status,
      event_nature = event_nature,
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = paste(
    "Get unavailability of generation units (15.1.A&B) for a bidding zone.",
    "Prefer 1-3 day date ranges (results capped at 100 rows). Max 1-year API",
    "limit. doc_status: 'A05' active, 'A09' cancelled, 'A13' withdrawn",
    "(NULL = active+cancelled). event_nature: 'A53' planned, 'A54' unplanned",
    "(NULL = both)."
  ),
  arguments = list(
    eic = type_string(
      description = "EIC code of the bidding zone or control area."
    ),
    period_start = type_string(
      description = paste(
        "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where",
        "possible - data is at 15-min to 1-hour resolution and results are",
        "capped at 100 rows."
      )
    ),
    period_end = type_string(
      description = "End date YYYY-MM-DD (CET)."
    ),
    doc_status = type_string(
      description = paste(
        "Document status filter: 'A05' active, 'A09' cancelled, 'A13'",
        "withdrawn."
      ),
      required = FALSE
    ),
    event_nature = type_string(
      description = paste(
        "Event nature: 'A53' planned maintenance, 'A54' unplanned outage."
      ),
      required = FALSE
    )
  )
)


tool_outages_prod_units <- tool(
  name = "outages_prod_units",
  fun = \(eic, period_start, period_end, doc_status = NULL,
          event_nature = NULL) {
    outages_prod_units(
      eic = eic,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      doc_status = doc_status,
      event_nature = event_nature,
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = paste(
    "Get unavailability of production units (15.1.C&D) for a bidding zone.",
    "Prefer 1-3 day date ranges (results capped at 100 rows). Max 1-year API",
    "limit. doc_status: 'A05' active, 'A09' cancelled, 'A13' withdrawn.",
    "event_nature: 'A53' planned, 'A54' unplanned."
  ),
  arguments = list(
    eic = type_string(
      description = "EIC code of the bidding zone or control area."
    ),
    period_start = type_string(
      description = paste(
        "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where",
        "possible - data is at 15-min to 1-hour resolution and results are",
        "capped at 100 rows."
      )
    ),
    period_end = type_string(
      description = "End date YYYY-MM-DD (CET)."
    ),
    doc_status = type_string(
      description = paste(
        "Document status filter: 'A05' active, 'A09' cancelled, 'A13'",
        "withdrawn."
      ),
      required = FALSE
    ),
    event_nature = type_string(
      description = paste(
        "Event nature: 'A53' planned maintenance, 'A54' unplanned outage."
      ),
      required = FALSE
    )
  )
)


tool_outages_transmission_grid <- tool(
  name = "outages_transmission_grid",
  fun = \(eic_in, eic_out, period_start, period_end, doc_status = NULL,
          event_nature = NULL) {
    outages_transmission_grid(
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
  description = paste(
    "Get unavailability in the transmission grid (15.1.A&B) on a border.",
    "Prefer 1-3 day date ranges (results capped at 100 rows). Max 1-year API",
    "limit. Requires both eic_in and eic_out for the affected border.",
    "doc_status: 'A05' active, 'A09' cancelled, 'A13' withdrawn.",
    "event_nature: 'A53' planned, 'A54' unplanned."
  ),
  arguments = list(
    eic_in = type_string(
      description = "EIC code of the importing side of the affected border."
    ),
    eic_out = type_string(
      description = "EIC code of the exporting side of the affected border."
    ),
    period_start = type_string(
      description = paste(
        "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where",
        "possible - data is at 15-min to 1-hour resolution and results are",
        "capped at 100 rows."
      )
    ),
    period_end = type_string(
      description = "End date YYYY-MM-DD (CET)."
    ),
    doc_status = type_string(
      description = paste(
        "Document status filter: 'A05' active, 'A09' cancelled, 'A13'",
        "withdrawn."
      ),
      required = FALSE
    ),
    event_nature = type_string(
      description = paste(
        "Event nature: 'A53' planned maintenance, 'A54' unplanned outage."
      ),
      required = FALSE
    )
  )
)


# ============================================================
# Balancing Tools
# ============================================================

tool_imbalance_prices <- tool(
  name = "imbalance_prices",
  fun = \(eic, period_start, period_end) {
    imbalance_prices(
      eic = eic,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = paste(
    "Get imbalance prices (17.1.D) for a scheduling area. Prefer 1-3 day date",
    "ranges (results capped at 100 rows). Max 1-year API limit. Use area_eic()",
    "to find the EIC code."
  ),
  arguments = list(
    eic = type_string(description = "EIC code of the scheduling/control area."),
    period_start = type_string(
      description = paste(
        "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where",
        "possible - data is at 15-min to 1-hour resolution and results are",
        "capped at 100 rows."
      )
    ),
    period_end = type_string(description = "End date YYYY-MM-DD (CET).")
  )
)


tool_imbalance_volumes <- tool(
  name = "imbalance_volumes",
  fun = \(eic, period_start, period_end) {
    imbalance_volumes(
      eic = eic,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = paste(
    "Get imbalance volumes (17.1.C) for a scheduling area. Prefer 1-3 day",
    "date ranges (results capped at 100 rows). Max 1-year API limit."
  ),
  arguments = list(
    eic = type_string(description = "EIC code of the scheduling/control area."),
    period_start = type_string(
      description = paste(
        "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where",
        "possible - data is at 15-min to 1-hour resolution and results are",
        "capped at 100 rows."
      )
    ),
    period_end = type_string(description = "End date YYYY-MM-DD (CET).")
  )
)


tool_contracted_reserves <- tool(
  name = "contracted_reserves",
  fun = \(
    eic, market_agreement_type, period_start, period_end, process_type = NULL
  ) {
    contracted_reserves(
      eic = eic,
      market_agreement_type = market_agreement_type,
      process_type = process_type,
      period_start = parse_date(ts = period_start),
      period_end = parse_date(ts = period_end),
      tidy_output = TRUE
    ) |>
      safe_to_csv()
  },
  description = paste(
    "Get contracted balancing reserves (17.1.B) for a scheduling area. Prefer",
    "1-3 day date ranges (results capped at 100 rows). market_agreement_type",
    "(required): 'A01' Daily, 'A02' Weekly, 'A03' Monthly, 'A04' Yearly, 'A06'",
    "Long-term, 'A13' Intraday. process_type (optional): 'A46' FCR, 'A47' ",
    "mFRR, 'A51' aFRR, 'A52' RR."
  ),
  arguments = list(
    eic = type_string(
      description = "EIC code of the scheduling/control area."
    ),
    market_agreement_type = type_string(
      description = paste(
        "Required. Agreement type: 'A01' Daily, 'A02' Weekly, 'A03' Monthly,",
        "'A04' Yearly, 'A06' Long-term, 'A13' Intraday."
      )
    ),
    period_start = type_string(
      description = paste(
        "Start date in YYYY-MM-DD format (CET). Keep ranges to 1-7 days where",
        "possible - data is at 15-min to 1-hour resolution and results are",
        "capped at 100 rows."
      )
    ),
    period_end = type_string(description = "End date YYYY-MM-DD (CET)."),
    process_type = type_string(
      description = paste(
        "Optional reserve type: 'A46' FCR, 'A47' mFRR, 'A51' aFRR, 'A52' RR."
      ),
      required = FALSE
    )
  )
)
