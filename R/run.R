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
#' The downloaded EIC code tables are cached on disk across server sessions,
#' so the EIC lookup tools answer instantly after the first download of the
#' day. The cache lives in `ENTSOEAPI_CACHE_DIR` when that environment
#' variable is set, otherwise in `tools::R_user_dir("entsoeapi", "cache")`.
#' Set `ENTSOEAPI_CACHE_DIR` to an empty string to keep the cache in memory.
#' An `entsoeapi.cache_dir` option set before calling `run()` is respected.
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
  use_eic_disk_cache()
  use_server_instructions()
  mcp_server(  # nolint: object_usage_linter
    tools = lapply(X = all_tools(), FUN = mute_stdout_tool),
    session_tools = session_tools
  ) |>
    invisible()
}


#' Switch the entsoeapi EIC table cache from memory to disk.
#'
#' entsoeapi (>= 1.3.0) caches the downloaded EIC code tables in memory by
#' default, which means every new MCP server process (one per client session)
#' downloads them again. The disk cache is enabled via the
#' `entsoeapi.cache_dir` option. `cachem::cache_disk()` errors when it cannot
#' create its directory, which would break every EIC tool, so the directory is
#' only used when it is writable; otherwise the in-memory cache is kept.
#'
#' @param cache_dir Directory of the disk cache. An empty string disables it.
#'
#' @return The cache directory in use (invisibly), or `NULL` when the
#'   in-memory cache is kept.
#'
#' @noRd
use_eic_disk_cache <- function(
  cache_dir = Sys.getenv(
    x = "ENTSOEAPI_CACHE_DIR",
    unset = tools::R_user_dir(package = "entsoeapi", which = "cache")
  )
) {
  # an explicit user choice wins
  if (!is.null(getOption("entsoeapi.cache_dir"))) {
    return(invisible(getOption("entsoeapi.cache_dir")))
  }
  if (!nzchar(cache_dir)) return(invisible(NULL))

  dir.create(path = cache_dir, showWarnings = FALSE, recursive = TRUE)
  writable <- dir.exists(cache_dir) &&
    file.access(names = cache_dir, mode = 2L) == 0L
  if (!writable) return(invisible(NULL))
  options(entsoeapi.cache_dir = cache_dir)
  invisible(cache_dir)
}


#' Server-level guidance sent to the MCP client in the `initialize` reply.
#'
#' Clients show this to the model once per session, before any tool call, so
#' it carries the cross-tool workflow that no single tool description can:
#' resolve the EIC first, then fetch, then aggregate in the session cache.
#'
#' @noRd
server_instructions <- paste(
  "Real data from the ENTSO-E Transparency Platform for European electricity",
  "markets: day-ahead and intraday prices, load, generation, installed",
  "capacity, cross-border flows, transfer capacities, outages and balancing.",
  "Prefer these tools over web search or made-up numbers for any European",
  "power market question.",
  "Workflow:",
  "(1) Resolve the area first with area_eic. Many countries have several",
  "EICs; prices are per bidding zone (BZN), load per control area (CTA),",
  "flows and capacities per border. Show the matches and let the user",
  "confirm when more than one fits.",
  "(2) Call a data tool with dates as YYYY-MM-DD (CET); one request covers",
  "at most one year. Point timestamps are stored twice: ts_point_dt_start",
  "in UTC and ts_point_dt_start_cet in Central European wall-clock time",
  "(CET/CEST, DST handled); group by the _cet column for local days and",
  "hours instead of adding a fixed INTERVAL. Each data tool stores its full",
  "result in a session DuckDB table and returns the table name, schema and",
  "a few sample rows.",
  "The sample rows are not the result.",
  "(3) Answer with sql_query against that table (GROUP BY to stay under the",
  "row cap) instead of calling the data tool again. Repeating a call with",
  "the same arguments reuses the cached table; list_tables and",
  "describe_table show what is already cached.",
  "If a result reports several series (e.g. price sequences), filter",
  "explicitly or ask which one the user means.",
  "Errors come back as text starting with '# error:'; read them and retry",
  "with corrected arguments."
)


#' Replace mcptools' generic server instructions with `server_instructions`.
#'
#' mcptools (1.0.3) hardcodes "This provides information about a running R
#' session." in its internal `capabilities()`, which builds the `initialize`
#' reply, and `mcp_server()` offers no argument to change it. The function is
#' swapped in mcptools' namespace for a wrapper that calls the original and
#' only overrides `instructions` (when the negotiated protocol has it) and
#' `serverInfo`. If a future mcptools drops `capabilities()`, nothing is
#' patched and the server still starts.
#'
#' @param instructions Text to send as the server instructions.
#'
#' @return `TRUE` (invisibly) when the patch is in place, otherwise `FALSE`.
#'
#' @importFrom utils packageVersion
#'
#' @noRd
use_server_instructions <- function(instructions = server_instructions) {
  ns <- asNamespace("mcptools")
  if (!exists(x = "capabilities", envir = ns, inherits = FALSE)) {
    return(invisible(FALSE))
  }
  # keep the pristine function so repeated run() calls do not nest wrappers
  if (is.null(.pkg_env$mcptools_capabilities)) {  # nolint: object_usage_linter
    .pkg_env$mcptools_capabilities <- get(x = "capabilities", envir = ns)  # nolint: object_usage_linter
  }
  original <- .pkg_env$mcptools_capabilities  # nolint: object_usage_linter
  if (!is.function(original)) return(invisible(FALSE))

  server_info <- list(
    name = "entsoeapi.mcp",
    version = as.character(packageVersion(pkg = "entsoeapi.mcp"))
  )
  patched <- function(...) {
    res <- original(...)
    if (!is.list(res)) return(res)
    if (!is.null(res$instructions)) res$instructions <- instructions
    res$serverInfo <- server_info
    res
  }

  locked <- bindingIsLocked(sym = "capabilities", env = ns)
  if (locked) unlockBinding(sym = "capabilities", env = ns)
  assign(x = "capabilities", value = patched, envir = ns)
  if (locked) lockBinding(sym = "capabilities", env = ns)
  invisible(TRUE)
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
