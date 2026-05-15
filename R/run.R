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
#' @importFrom mcptools mcp_server
#'
#' @return Does not return; runs until the MCP client closes the connection.
#'
#' @export
run <- function(session_tools = FALSE) {
  mcp_server(  # nolint: object_usage_linter
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
#' @param tool_def A `ToolDef` S7 object (as returned by `mcptools::tool()`).
#'
#' @return A `ToolDef` object identical to `tool_def` except that its callable
#'   body suppresses any stdout produced during execution (e.g. HTTP headers
#'   printed by `httr2::req_verbose()`).
#'
#' @importFrom utils capture.output
#'
#' @noRd
mute_stdout_tool <- function(tool_def) {
  # capture the original ToolDef as a plain fn ref
  fn <- tool_def
  old_class <- class(x = tool_def)

  # S7 slots: class, S7_class, name, description, ...
  old_attrs <- attributes(x = tool_def)

  wrapper <- \(...) {
    .result <- NULL
    .err <- tryCatch(
      expr = {
        capture.output(.result <- fn(...), type = "output")
        NULL
      },
      error = identity
    )
    if (!is.null(.err)) return(paste0("# error: ", conditionMessage(.err)))
    .result
  }

  # NOTE: we intentionally do NOT replace formals(wrapper). mcptools builds the
  # JSON schema from the S7 `arguments` attribute (restored below), not from R
  # formals. fn(...) forwards all named arguments from Claude Desktop correctly.

  # body() replacement strips S7 class identity — restore all saved
  # attributes (skip srcref/srcfile/srcfilecopy which belong to the old body).
  src_attrs <- c("srcref", "srcfile", "srcfilecopy")
  for (nm in setdiff(x = names(old_attrs), y = src_attrs)) {
    attr(x = wrapper, which = nm) <- old_attrs[[nm]]
  }
  class(x = wrapper) <- old_class
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
    tool_area_eic,  # nolint: object_usage_linter
    tool_party_eic,  # nolint: object_usage_linter
    tool_all_approved_eic,  # nolint: object_usage_linter
    tool_resource_object_eic,  # nolint: object_usage_linter
    tool_get_news,  # nolint: object_usage_linter

    # Load (merged: actual, day_ahead,  # nolint: commented_code_linter
    #               week_ahead, month_ahead,  # nolint: commented_code_linter
    #               year_ahead, margin)  # nolint: commented_code_linter
    tool_load,  # nolint: object_usage_linter

    # Generation (merged: time-series &  # nolint: commented_code_linter
    #                     capacity)  # nolint: commented_code_linter
    tool_gen_time_series,  # nolint: object_usage_linter
    tool_gen_capacity,  # nolint: object_usage_linter

    # Market  # nolint: commented_code_linter
    tool_energy_prices,  # nolint: object_usage_linter
    tool_intraday_prices,  # nolint: object_usage_linter
    tool_net_transfer_capacities,  # nolint: object_usage_linter
    tool_day_ahead_commercial_sched,  # nolint: object_usage_linter
    tool_explicit_offered_transfer_capacities,  # nolint: object_usage_linter
    tool_flow_based_allocations,  # nolint: object_usage_linter
    tool_congestion_income,  # nolint: object_usage_linter
    tool_allocated_transfer_capacities_3rd_countries,  # nolint: object_usage_linter

    # Transmission  # nolint: commented_code_linter
    tool_cross_border_physical_flows,  # nolint: object_usage_linter
    tool_total_commercial_sched,  # nolint: object_usage_linter
    tool_net_positions,  # nolint: object_usage_linter
    tool_forecasted_transfer_capacities,  # nolint: object_usage_linter

    # Outages  # nolint: commented_code_linter
    tool_outages_gen_units,  # nolint: object_usage_linter
    tool_outages_prod_units,  # nolint: object_usage_linter
    tool_outages_transmission_grid,  # nolint: object_usage_linter

    # Balancing  # nolint: commented_code_linter
    tool_imbalance_prices,  # nolint: object_usage_linter
    tool_imbalance_volumes,  # nolint: object_usage_linter
    tool_contracted_reserves,  # nolint: object_usage_linter

    # Session DuckDB cache  # nolint: commented_code_linter
    tool_sql_query,  # nolint: object_usage_linter
    tool_list_tables,  # nolint: object_usage_linter
    tool_describe_table  # nolint: object_usage_linter
  )
}
