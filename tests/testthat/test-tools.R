# ── .eic_filter() ────────────────────────────────────────────────────────────

testthat::test_that(
  desc = ".eic_filter() keeps only the five key columns, drops everything else",
  code = {
    df <- data.frame(
      eic_code = "X001",
      eic_display_name = "Zone A",
      eic_long_name = "Zone Alpha",
      market_participant_iso_country_code = "DE",
      eic_type_function_list = "Y",
      postal_code = "12345",
      vat_code = "VAT123"
    )
    result <- entsoeapi.mcp:::.eic_filter(df = df, query = NULL)
    testthat::expect_equal(
      object = sort(names(result)),
      expected = c(
        "eic_code", "eic_display_name", "eic_long_name",
        "market_participant_iso_country_code", "eic_type_function_list"
      ) |>
        sort()
    )
  }
)


testthat::test_that(
  desc = ".eic_filter() returns all rows when query is NULL",
  code = {
    df <- data.frame(
      eic_code = c("X001", "X002", "X003"),
      eic_display_name = c("Alpha", "Beta", "Gamma")
    )
    result <- entsoeapi.mcp:::.eic_filter(df = df, query = NULL)
    testthat::expect_equal(object = nrow(result), expected = 3L)
  }
)


testthat::test_that(
  desc = ".eic_filter() returns all rows when query is empty or whitespace",
  code = {
    df <- data.frame(
      eic_code = c("X001", "X002"),
      eic_display_name = c("Alpha", "Beta")
    )
    testthat::expect_equal(
      object = entsoeapi.mcp:::.eic_filter(df = df, query = "") |> nrow(),
      expected = 2L
    )
    testthat::expect_equal(
      object = entsoeapi.mcp:::.eic_filter(df = df, query = "   ") |> nrow(),
      expected = 2L
    )
  }
)


testthat::test_that(
  desc = ".eic_filter() filters rows case-insensitively on any key column",
  code = {
    df <- data.frame(
      eic_code = c("X001", "X002", "X003"),
      eic_display_name = c("Germany", "France", "Austria"),
      market_participant_iso_country_code = c("DE", "FR", "AT")
    )
    # Lowercase match on display name
    lower <- entsoeapi.mcp:::.eic_filter(df = df, query = "germany")
    testthat::expect_equal(object = nrow(lower), expected = 1L)
    testthat::expect_equal(object = lower$eic_code, expected = "X001")

    # Uppercase match on country code
    upper <- entsoeapi.mcp:::.eic_filter(df = df, query = "fr")
    testthat::expect_equal(object = nrow(upper), expected = 1L)
    testthat::expect_equal(object = upper$eic_code, expected = "X002")
  }
)


testthat::test_that(
  desc = ".eic_filter() returns zero rows when query matches nothing",
  code = {
    df <- data.frame(
      eic_code = c("X001", "X002"),
      eic_display_name = c("Germany", "France")
    )
    result <- entsoeapi.mcp:::.eic_filter(df = df, query = "zzznomatch")
    testthat::expect_equal(object = nrow(result), expected = 0L)
  }
)


testthat::test_that(
  desc = ".eic_filter() handles df that contains only a subset of key columns",
  code = {
    df <- data.frame(
      eic_code = c("X001", "X002"),
      eic_long_name = c("Zone Alpha", "Zone Beta"),
      irrelevant = c("drop", "me")
    )
    result <- entsoeapi.mcp:::.eic_filter(df = df, query = NULL)
    testthat::expect_true(object = "eic_code" %in% names(result))
    testthat::expect_true(object = "eic_long_name" %in% names(result))
    testthat::expect_false(object = "irrelevant" %in% names(result))
    # Columns absent from the input must not appear in the output
    testthat::expect_false(object = "eic_display_name" %in% names(result))
  }
)


# ── tool argument names ──────────────────────────────────────────────────────
# arguments is an ellmer::TypeObject (S7); parameter names live in @properties.

testthat::test_that(
  desc = "tool_load exposes eic, period_start, period_end, and type arguments",
  code = {
    arg_names <- attr(
      x = entsoeapi.mcp:::tool_load,
      which = "arguments"
    )@properties |>
      names()
    testthat::expect_true(
      object = all(
        c("eic", "period_start", "period_end", "type") %in% arg_names
      )
    )
  }
)


testthat::test_that(
  desc = paste(
    "tool_gen_capacity exposes eic, year, per_unit, and psr_type arguments"
  ),
  code = {
    arg_names <- attr(
      x = entsoeapi.mcp:::tool_gen_capacity,
      which = "arguments"
    )@properties |>
      names()
    testthat::expect_true(
      object = all(c("eic", "year", "per_unit", "psr_type") %in% arg_names)
    )
  }
)


testthat::test_that(
  desc = "cross-border tools expose eic_in and eic_out, not bare eic",
  code = {
    border_tools <- list(
      entsoeapi.mcp:::tool_net_transfer_capacities,
      entsoeapi.mcp:::tool_day_ahead_commercial_sched,
      entsoeapi.mcp:::tool_cross_border_physical_flows,
      entsoeapi.mcp:::tool_outages_transmission_grid,
      entsoeapi.mcp:::tool_forecasted_transfer_capacities
    )
    for (t in border_tools) {
      nm        <- attr(x = t, which = "name")
      arg_names <- names(attr(x = t, which = "arguments")@properties)
      testthat::expect_true(
        object = all(c("eic_in", "eic_out") %in% arg_names),
        label = paste(nm, "should have eic_in and eic_out")
      )
      testthat::expect_false(
        object = "eic" %in% arg_names,
        label = paste(nm, "should not have bare 'eic'")
      )
    }
  }
)


# ── error paths (no network calls needed) ────────────────────────────────────
# parse_date() is network-free; the stop() fires inside switch() before any
# entsoeapi function is invoked.

testthat::test_that(
  desc = "tool_load errors with an informative message for an unknown type",
  code = {
    testthat::expect_error(
      object = entsoeapi.mcp:::tool_load(
        eic = "10Y1001A1001A83F",
        period_start = "2024-01-01",
        period_end = "2024-01-02",
        type = "not_a_valid_type"
      ),
      regexp = "Unknown type"
    )
  }
)


testthat::test_that(
  desc = paste(
    "tool_gen_time_series errors with an informative message for an",
    "unknown type"
  ),
  code = {
    testthat::expect_error(
      object = entsoeapi.mcp:::tool_gen_time_series(
        eic = "10Y1001A1001A83F",
        period_start = "2024-01-01",
        period_end = "2024-01-02",
        type = "not_a_valid_type"
      ),
      regexp = "Unknown type"
    )
  }
)


# ── dispatch logic ───────────────────────────────────────────────────────────
# entsoeapi functions live in the *imports environment* (parent of the
# namespace), not the namespace itself, so with_mocked_bindings(.package=)
# cannot reach them. The imports env is locked, but lockEnvironment() only
# prevents *adding* new bindings — individual bindings can still be temporarily
# unlocked with unlockBinding() / lockBinding().  All mocked functions must
# therefore exist in the imports env first (NAMESPACE importFrom declarations
# required). safe_to_csv IS in the namespace, so it stays with
# with_mocked_bindings.

.with_imp_bindings <- function(bindings, expr) {
  env <- getNamespace(name = "entsoeapi.mcp") |> parent.env()
  nms <- names(bindings)

  old_vals <- setNames(
    object = lapply(X = nms, FUN = \(nm) get(x = nm, envir = env)),
    nm = nms
  )
  for (nm in nms) {
    unlockBinding(sym = nm, env = env)
    assign(x = nm, value = bindings[[nm]], envir = env)
  }

  on.exit(
    expr = {
      for (nm in nms) {
        assign(x = nm, value = old_vals[[nm]], envir = env)
        lockBinding(sym = nm, env = env)
      }
    },
    add = TRUE
  )

  force(expr)
}


testthat::test_that(
  desc = paste(
    "tool_gen_capacity calls gen_installed_capacity_per_pt when",
    "per_unit = FALSE"
  ),
  code = {
    called <- NULL
    .with_imp_bindings(
      bindings = list(
        gen_installed_capacity_per_pt = \(...) {
          called <<- "per_pt"
          data.frame(x = 1L)
        },
        gen_installed_capacity_per_pu = \(...) {
          called <<- "per_pu"
          data.frame(x = 1L)
        }
      ),
      expr = testthat::with_mocked_bindings(
        safe_to_csv = \(df, ...) {
          force(df)
          "csv"
        },
        .package = "entsoeapi.mcp",
        entsoeapi.mcp:::tool_gen_capacity(
          eic = "X",
          year = 2024L,
          per_unit = FALSE
        )
      )
    )
    testthat::expect_equal(object = called, expected = "per_pt")
  }
)


testthat::test_that(
  desc = paste(
    "tool_gen_capacity calls gen_installed_capacity_per_pu when",
    "per_unit = TRUE"
  ),
  code = {
    called <- NULL
    .with_imp_bindings(
      bindings = list(
        gen_installed_capacity_per_pt = \(...) {
          called <<- "per_pt"
          data.frame(x = 1L)
        },
        gen_installed_capacity_per_pu = \(...) {
          called <<- "per_pu"
          data.frame(x = 1L)
        }
      ),
      expr = testthat::with_mocked_bindings(
        safe_to_csv = \(df, ...) {
          force(df)
          "csv"
        },
        .package = "entsoeapi.mcp",
        entsoeapi.mcp:::tool_gen_capacity(
          eic = "X",
          year = 2024L,
          per_unit = TRUE
        )
      )
    )
    testthat::expect_equal(object = called, expected = "per_pu")
  }
)


testthat::test_that(
  desc = "tool_load dispatches to load_actual_total for type = 'actual'",
  code = {
    called <- NULL
    .with_imp_bindings(
      bindings = list(
        load_actual_total = \(...) {
          called <<- "load_actual_total"
          data.frame(x = 1L)
        }
      ),
      expr = testthat::with_mocked_bindings(
        safe_to_csv = \(df, ...) {
          force(df)
          "csv"
        },
        .package = "entsoeapi.mcp",
        entsoeapi.mcp:::tool_load(
          eic = "10Y1001A1001A83F",
          period_start = "2024-01-01",
          period_end = "2024-01-02",
          type = "actual"
        )
      )
    )
    testthat::expect_equal(object = called, expected = "load_actual_total")
  }
)


testthat::test_that(
  desc = paste(
    "tool_load dispatches to load_day_ahead_total_forecast for",
    "type = 'day_ahead'"
  ),
  code = {
    called <- NULL
    .with_imp_bindings(
      bindings = list(
        load_day_ahead_total_forecast = \(...) {
          called <<- "load_day_ahead_total_forecast"
          data.frame(x = 1L)
        }
      ),
      expr = testthat::with_mocked_bindings(
        safe_to_csv = \(df, ...) {
          force(df)
          "csv"
        },
        .package = "entsoeapi.mcp",
        entsoeapi.mcp:::tool_load(
          eic = "10Y1001A1001A83F",
          period_start = "2024-01-01",
          period_end = "2024-01-02",
          type = "day_ahead"
        )
      )
    )
    testthat::expect_equal(
      object = called,
      expected = "load_day_ahead_total_forecast"
    )
  }
)


# ── full-coverage tool invocation helper ─────────────────────────────────────
# Mocks one (or more) entsoeapi function(s) in the *imports* env and
# safe_to_csv in the *namespace*, then invokes the tool with `call_args`.
# Verifies (a) the mocked fn(s) ran and (b) the closure returned the mocked
# CSV string. Each generated mock closure must `force(nm)` so its captured
# name is bound by value, not by lazy reference into the lapply frame.
.expect_tool_calls <- function(tool_obj, entsoeapi_fns, call_args) {
  called <- character()
  bindings <- lapply(
    X = entsoeapi_fns,
    FUN = \(nm) {
      force(nm)
      \(...) {
        called <<- c(called, nm)
        data.frame(x = 1L)
      }
    }
  )
  names(bindings) <- entsoeapi_fns
  result <- .with_imp_bindings(
    bindings = bindings,
    expr = testthat::with_mocked_bindings(
      safe_to_csv = \(df, ...) {
        force(df)
        "csv"
      },
      .package = "entsoeapi.mcp",
      do.call(what = tool_obj, args = call_args)
    )
  )
  testthat::expect_setequal(object = called, expected = entsoeapi_fns)
  testthat::expect_equal(object = result, expected = "csv")
}


# ── EIC lookup tools (no date arguments) ─────────────────────────────────────

testthat::test_that(
  desc = "tool_area_eic invokes entsoeapi::area_eic",
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_area_eic,
    entsoeapi_fns = "area_eic",
    call_args = list()
  )
)


testthat::test_that(
  desc = "tool_party_eic invokes entsoeapi::party_eic",
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_party_eic,
    entsoeapi_fns = "party_eic",
    call_args = list()
  )
)


testthat::test_that(
  desc = "tool_all_approved_eic invokes entsoeapi::all_approved_eic",
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_all_approved_eic,
    entsoeapi_fns = "all_approved_eic",
    call_args = list()
  )
)


testthat::test_that(
  desc = "tool_resource_object_eic invokes entsoeapi::resource_object_eic",
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_resource_object_eic,
    entsoeapi_fns = "resource_object_eic",
    call_args = list()
  )
)


testthat::test_that(
  desc = "tool_get_news invokes entsoeapi::get_news with the requested n",
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_get_news,
    entsoeapi_fns = "get_news",
    call_args = list(n = 5L)
  )
)


# ── tool_load remaining switch() branches ────────────────────────────────────

testthat::test_that(
  desc = "tool_load dispatches to load_week_ahead_total_forecast",
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_load,
    entsoeapi_fns = "load_week_ahead_total_forecast",
    call_args = list(
      eic = "10Y1001A1001A83F",
      period_start = "2024-01-01",
      period_end = "2024-01-08",
      type = "week_ahead"
    )
  )
)


testthat::test_that(
  desc = "tool_load dispatches to load_month_ahead_total_forecast",
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_load,
    entsoeapi_fns = "load_month_ahead_total_forecast",
    call_args = list(
      eic = "10Y1001A1001A83F",
      period_start = "2024-01-01",
      period_end = "2024-02-01",
      type = "month_ahead"
    )
  )
)


testthat::test_that(
  desc = "tool_load dispatches to load_year_ahead_total_forecast",
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_load,
    entsoeapi_fns = "load_year_ahead_total_forecast",
    call_args = list(
      eic = "10Y1001A1001A83F",
      period_start = "2024-01-01",
      period_end = "2025-01-01",
      type = "year_ahead"
    )
  )
)


testthat::test_that(
  desc = "tool_load dispatches to load_year_ahead_forecast_margin",
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_load,
    entsoeapi_fns = "load_year_ahead_forecast_margin",
    call_args = list(
      eic = "10Y1001A1001A83F",
      period_start = "2024-01-01",
      period_end = "2025-01-01",
      type = "margin"
    )
  )
)


# ── tool_gen_time_series switch() branches ───────────────────────────────────

testthat::test_that(
  desc = "tool_gen_time_series dispatches to gen_per_prod_type for 'actual'",
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_gen_time_series,
    entsoeapi_fns = "gen_per_prod_type",
    call_args = list(
      eic = "10Y1001A1001A83F",
      period_start = "2024-01-01",
      period_end = "2024-01-02",
      type = "actual"
    )
  )
)


testthat::test_that(
  desc = paste(
    "tool_gen_time_series dispatches to gen_wind_solar_forecasts for",
    "'wind_solar'"
  ),
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_gen_time_series,
    entsoeapi_fns = "gen_wind_solar_forecasts",
    call_args = list(
      eic = "10Y1001A1001A83F",
      period_start = "2024-01-01",
      period_end = "2024-01-02",
      type = "wind_solar"
    )
  )
)


testthat::test_that(
  desc = paste(
    "tool_gen_time_series dispatches to gen_day_ahead_forecast for",
    "'day_ahead'"
  ),
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_gen_time_series,
    entsoeapi_fns = "gen_day_ahead_forecast",
    call_args = list(
      eic = "10Y1001A1001A83F",
      period_start = "2024-01-01",
      period_end = "2024-01-02",
      type = "day_ahead"
    )
  )
)


testthat::test_that(
  desc = "tool_gen_time_series dispatches to gen_per_gen_unit for 'per_unit'",
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_gen_time_series,
    entsoeapi_fns = "gen_per_gen_unit",
    call_args = list(
      eic = "10Y1001A1001A83F",
      period_start = "2024-01-01",
      period_end = "2024-01-02",
      type = "per_unit"
    )
  )
)


testthat::test_that(
  desc = paste(
    "tool_gen_time_series dispatches to gen_storage_mean_filling_rate for",
    "'storage'"
  ),
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_gen_time_series,
    entsoeapi_fns = "gen_storage_mean_filling_rate",
    call_args = list(
      eic = "10Y1001A1001A83F",
      period_start = "2024-01-01",
      period_end = "2024-01-02",
      type = "storage"
    )
  )
)


# ── market tools ─────────────────────────────────────────────────────────────

testthat::test_that(
  desc = "tool_energy_prices invokes entsoeapi::energy_prices",
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_energy_prices,
    entsoeapi_fns = "energy_prices",
    call_args = list(
      eic = "10Y1001A1001A83F",
      period_start = "2024-01-01",
      period_end = "2024-01-02"
    )
  )
)


testthat::test_that(
  desc = "tool_intraday_prices invokes entsoeapi::intraday_prices",
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_intraday_prices,
    entsoeapi_fns = "intraday_prices",
    call_args = list(
      eic = "10Y1001A1001A83F",
      period_start = "2024-01-01",
      period_end = "2024-01-02"
    )
  )
)


testthat::test_that(
  desc = paste(
    "tool_net_transfer_capacities invokes",
    "entsoeapi::net_transfer_capacities"
  ),
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_net_transfer_capacities,
    entsoeapi_fns = "net_transfer_capacities",
    call_args = list(
      eic_in = "10Y1001A1001A83F",
      eic_out = "10YFR-RTE------C",
      period_start = "2024-01-01",
      period_end = "2024-01-02"
    )
  )
)


testthat::test_that(
  desc = paste(
    "tool_day_ahead_commercial_sched invokes",
    "entsoeapi::day_ahead_commercial_sched"
  ),
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_day_ahead_commercial_sched,
    entsoeapi_fns = "day_ahead_commercial_sched",
    call_args = list(
      eic_in = "10Y1001A1001A83F",
      eic_out = "10YFR-RTE------C",
      period_start = "2024-01-01",
      period_end = "2024-01-02"
    )
  )
)


testthat::test_that(
  desc = paste(
    "tool_explicit_offered_transfer_capacities invokes",
    "entsoeapi::explicit_offered_transfer_capacities"
  ),
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_explicit_offered_transfer_capacities,
    entsoeapi_fns = "explicit_offered_transfer_capacities",
    call_args = list(
      eic_in = "10Y1001A1001A83F",
      eic_out = "10YFR-RTE------C",
      period_start = "2024-01-01",
      period_end = "2024-01-02"
    )
  )
)


testthat::test_that(
  desc = paste(
    "tool_flow_based_allocations invokes entsoeapi::flow_based_allocations"
  ),
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_flow_based_allocations,
    entsoeapi_fns = "flow_based_allocations",
    call_args = list(
      eic = "10Y1001A1001A83F",
      period_start = "2024-01-01",
      period_end = "2024-01-02"
    )
  )
)


testthat::test_that(
  desc = "tool_congestion_income invokes entsoeapi::congestion_income",
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_congestion_income,
    entsoeapi_fns = "congestion_income",
    call_args = list(
      eic = "10Y1001A1001A83F",
      period_start = "2024-01-01",
      period_end = "2024-01-02"
    )
  )
)


testthat::test_that(
  desc = paste(
    "tool_allocated_transfer_capacities_3rd_countries invokes",
    "entsoeapi::allocated_transfer_capacities_3rd_countries"
  ),
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_allocated_transfer_capacities_3rd_countries,
    entsoeapi_fns = "allocated_transfer_capacities_3rd_countries",
    call_args = list(
      eic_in = "10Y1001A1001A83F",
      eic_out = "10YFR-RTE------C",
      period_start = "2024-01-01",
      period_end = "2024-01-02"
    )
  )
)


# ── transmission tools ───────────────────────────────────────────────────────

testthat::test_that(
  desc = paste(
    "tool_cross_border_physical_flows invokes",
    "entsoeapi::cross_border_physical_flows"
  ),
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_cross_border_physical_flows,
    entsoeapi_fns = "cross_border_physical_flows",
    call_args = list(
      eic_in = "10Y1001A1001A83F",
      eic_out = "10YFR-RTE------C",
      period_start = "2024-01-01",
      period_end = "2024-01-02"
    )
  )
)


testthat::test_that(
  desc = paste(
    "tool_total_commercial_sched invokes entsoeapi::total_commercial_sched"
  ),
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_total_commercial_sched,
    entsoeapi_fns = "total_commercial_sched",
    call_args = list(
      eic_in = "10Y1001A1001A83F",
      eic_out = "10YFR-RTE------C",
      period_start = "2024-01-01",
      period_end = "2024-01-02"
    )
  )
)


testthat::test_that(
  desc = "tool_net_positions invokes entsoeapi::net_positions",
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_net_positions,
    entsoeapi_fns = "net_positions",
    call_args = list(
      eic = "10Y1001A1001A83F",
      period_start = "2024-01-01",
      period_end = "2024-01-02"
    )
  )
)


testthat::test_that(
  desc = paste(
    "tool_forecasted_transfer_capacities invokes",
    "entsoeapi::forecasted_transfer_capacities"
  ),
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_forecasted_transfer_capacities,
    entsoeapi_fns = "forecasted_transfer_capacities",
    call_args = list(
      eic_in = "10Y1001A1001A83F",
      eic_out = "10YFR-RTE------C",
      period_start = "2024-01-01",
      period_end = "2024-01-02"
    )
  )
)


# ── outages tools ────────────────────────────────────────────────────────────

testthat::test_that(
  desc = "tool_outages_gen_units invokes entsoeapi::outages_gen_units",
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_outages_gen_units,
    entsoeapi_fns = "outages_gen_units",
    call_args = list(
      eic = "10Y1001A1001A83F",
      period_start = "2024-01-01",
      period_end = "2024-01-02"
    )
  )
)


testthat::test_that(
  desc = "tool_outages_prod_units invokes entsoeapi::outages_prod_units",
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_outages_prod_units,
    entsoeapi_fns = "outages_prod_units",
    call_args = list(
      eic = "10Y1001A1001A83F",
      period_start = "2024-01-01",
      period_end = "2024-01-02"
    )
  )
)


testthat::test_that(
  desc = paste(
    "tool_outages_transmission_grid invokes",
    "entsoeapi::outages_transmission_grid"
  ),
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_outages_transmission_grid,
    entsoeapi_fns = "outages_transmission_grid",
    call_args = list(
      eic_in = "10Y1001A1001A83F",
      eic_out = "10YFR-RTE------C",
      period_start = "2024-01-01",
      period_end = "2024-01-02"
    )
  )
)


# ── balancing tools ──────────────────────────────────────────────────────────

testthat::test_that(
  desc = "tool_imbalance_prices invokes entsoeapi::imbalance_prices",
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_imbalance_prices,
    entsoeapi_fns = "imbalance_prices",
    call_args = list(
      eic = "10Y1001A1001A83F",
      period_start = "2024-01-01",
      period_end = "2024-01-02"
    )
  )
)


testthat::test_that(
  desc = "tool_imbalance_volumes invokes entsoeapi::imbalance_volumes",
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_imbalance_volumes,
    entsoeapi_fns = "imbalance_volumes",
    call_args = list(
      eic = "10Y1001A1001A83F",
      period_start = "2024-01-01",
      period_end = "2024-01-02"
    )
  )
)


testthat::test_that(
  desc = "tool_contracted_reserves invokes entsoeapi::contracted_reserves",
  code = .expect_tool_calls(
    tool_obj = entsoeapi.mcp:::tool_contracted_reserves,
    entsoeapi_fns = "contracted_reserves",
    call_args = list(
      eic = "10Y1001A1001A83F",
      market_agreement_type = "A01",
      period_start = "2024-01-01",
      period_end = "2024-01-02"
    )
  )
)
