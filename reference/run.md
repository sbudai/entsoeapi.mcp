# Start the ENTSO-E MCP server

Launches a Model Context Protocol server over stdin/stdout, exposing all
ENTSO-E Transparency Platform tools to the connected LLM client (e.g.
Claude Desktop). This function blocks until the client disconnects.

## Usage

``` r
run(session_tools = FALSE)
```

## Arguments

- session_tools:

  Logical. If `TRUE`, expose tools that can register additional tools at
  runtime. Defaults to `FALSE`.

## Value

Does not return; runs until the MCP client closes the connection.

## Details

The server reads `ENTSOE_PAT` from the environment automatically — the
underlying `entsoeapi` functions call `Sys.getenv("ENTSOE_PAT")` as
their default `security_token`.

The downloaded EIC code tables are cached on disk across server
sessions, so the EIC lookup tools answer instantly after the first
download of the day. The cache lives in `ENTSOEAPI_CACHE_DIR` when that
environment variable is set, otherwise in
`tools::R_user_dir("entsoeapi", "cache")`. Set `ENTSOEAPI_CACHE_DIR` to
an empty string to keep the cache in memory. An `entsoeapi.cache_dir`
option set before calling `run()` is respected.
