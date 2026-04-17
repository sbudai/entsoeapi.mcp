---
editor_options: 
  markdown: 
    wrap: 80
---

# entsoeapi.mcp 0.1.0

## Initial release

-   Provides a Model Context Protocol (MCP) server that exposes the `entsoeapi`
    package as 26 structured tools for LLM-enabled applications such as Claude
    Desktop.

-   Tools cover all major ENTSO-E Transparency Platform domains:

    -   **EIC lookup** – area, party, resource-object and all-approved EIC
        codes, and ENTSO-E news.
    -   **Load** – actual, day-ahead, week-ahead, month-ahead, year-ahead, and
        margin load (unified `tool_load` with a `type` parameter).
    -   **Generation** – time-series (`tool_gen_time_series`) and installed
        capacity (`tool_gen_capacity`), each merging several underlying API
        document types.
    -   **Market** – day-ahead energy prices, intraday prices, net transfer
        capacities, day-ahead commercial schedules, explicit offered transfer
        capacities, flow-based allocations, congestion income, and allocated
        transfer capacities for third countries.
    -   **Transmission** – cross-border physical flows, total commercial
        schedules, net positions, and forecasted transfer capacities.
    -   **Outages** – generation units, production units, and transmission grid.
    -   **Balancing** – imbalance prices, imbalance volumes, and contracted
        reserves.

## Key implementation details

-   MCP server runs over stdin/stdout via `mcptools::mcp_server()`; the
    `ENTSOE_PAT` security token is read automatically from the environment.

-   All tool results are serialised as CSV (instead of JSON), reducing
    context-window token usage by 60–70 % for wide time-series data frames.
    Results are capped at 100 rows with a truncation notice appended when data
    is partial.

-   Uninformative constant columns (repeated domain names, currency codes, unit
    labels) are dropped from every result via an internal `slim_ts()` helper,
    further reducing payload size.

-   HTTP response headers printed by `httr2::req_verbose()` are suppressed
    before they can corrupt the JSON-RPC stream on the MCP stdio transport.

-   Tool argument descriptions steer the LLM toward short date ranges to avoid
    unnecessarily large API responses.
