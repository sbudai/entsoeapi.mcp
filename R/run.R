#' Start the ENTSO-E MCP server
#'
#' Launches a Model Context Protocol server over stdin/stdout, exposing all
#' ENTSO-E Transparency Platform tools to the connected LLM client (e.g.
#' Claude Desktop). This function blocks until the client disconnects.
#'
#' The server reads `ENTSOE_PAT` from the environment automatically — the
#' underlying `entsoeapi` functions call `Sys.getenv("ENTSOE_PAT")` as their
#' default `security_token`.
#'
#' @param session_tools Logical. If `TRUE`, expose tools that can register
#'   additional tools at runtime. Defaults to `FALSE`.
#'
#' @return Does not return; runs until the MCP client closes the connection.
#'
#' @export
run <- function(session_tools = FALSE) {
  mcptools::mcp_server(
    tools = lapply(X = all_tools(), FUN = mute_stdout_tool),
    session_tools = session_tools
  ) |>
    invisible()
}


#' Wrap a ToolDef so that any stdout printed during the call is discarded.
#'
#' entsoeapi::api_req() calls httr2::req_verbose(header_resp = TRUE) on every
#' request. httr2's verbose callback uses cat() which writes HTTP response
#' headers to R's stdout connection — the same pipe used by the MCP stdio
#' transport. Claude Desktop then receives lines like "<- x-content-type: ..."
#' interleaved with JSON-RPC, which fails JSON parsing.
#'
#' Fix: build a new wrapper function that calls the original via fn(...) inside
#' capture.output, then copy the original formals and restore all S7 attributes.
#' Using fn(...) avoids bquote body-substitution scoping issues entirely — the
#' original function already has the correct formals bound, so ... just forwards
#' them. capture.output's on.exit always restores stdout even on error.
#'
#' @param tool_def description
#'
#' @return ...
#'
#' @noRd
mute_stdout_tool <- function(tool_def) {
  # capture the original ToolDef as a plain fn ref
  fn <- tool_def
  old_class <- class(tool_def)

  # S7 slots: class, S7_class, name, description, ...
  old_attrs <- attributes(tool_def)

  wrapper <- \(...) {
    .result <- NULL
    capture.output(.result <- fn(...), type = "output")
    .result
  }

  # NOTE: we intentionally do NOT replace formals(wrapper). mcptools builds the
  # JSON schema from the S7 `arguments` attribute (restored below), not from R
  # formals. fn(...) forwards all named arguments from Claude Desktop correctly.

  # body() replacement strips S7 class identity — restore all saved
  # attributes (skip srcref/srcfile/srcfilecopy which belong to the old body).
  src_attrs <- c("srcref", "srcfile", "srcfilecopy")
  for (nm in base::setdiff(x = names(old_attrs), y = src_attrs)) {
    attr(x = wrapper, which = nm) <- old_attrs[[nm]]
  }
  class(wrapper) <- old_class
  wrapper
}


#' Collect all tool objects into a list
#'
#' @return a list of accessible tools
#'
#' @noRd
all_tools <- function() {
  list(
    # EIC lookup
    tool_area_eic,
    tool_party_eic,
    tool_all_approved_eic,
    tool_resource_object_eic,
    tool_get_news,

    # Load
    tool_load_actual_total,
    tool_load_day_ahead_total_forecast,
    tool_load_week_ahead_total_forecast,
    tool_load_month_ahead_total_forecast,
    tool_load_year_ahead_total_forecast,
    tool_load_year_ahead_forecast_margin,

    # Generation
    tool_gen_per_prod_type,
    tool_gen_installed_capacity_per_pt,
    tool_gen_installed_capacity_per_pu,
    tool_gen_wind_solar_forecasts,
    tool_gen_day_ahead_forecast,
    tool_gen_per_gen_unit,
    tool_gen_storage_mean_filling_rate,

    # Market
    tool_energy_prices,
    tool_intraday_prices,
    tool_net_transfer_capacities,
    tool_day_ahead_commercial_sched,
    tool_explicit_offered_transfer_capacities,
    tool_flow_based_allocations,
    tool_congestion_income,
    tool_allocated_transfer_capacities_3rd_countries,

    # Transmission
    tool_cross_border_physical_flows,
    tool_total_commercial_sched,
    tool_net_positions,
    tool_forecasted_transfer_capacities,

    # Outages
    tool_outages_gen_units,
    tool_outages_prod_units,
    tool_outages_transmission_grid,

    # Balancing
    tool_imbalance_prices,
    tool_imbalance_volumes,
    tool_contracted_reserves
  )
}
