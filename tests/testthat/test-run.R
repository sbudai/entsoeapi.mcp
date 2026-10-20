testthat::test_that(
  desc = "all_tools() returns a list of 30 tool definitions",
  code = {
    tools <- entsoeapi.mcp:::all_tools()
    testthat::expect_type(object = tools, type = "list")
    testthat::expect_length(object = tools, n = 30L)
  }
)

testthat::test_that(
  desc = "all_tools() includes the three session DuckDB tools",
  code = {
    tools <- entsoeapi.mcp:::all_tools()
    names_vec <- vapply(
      X         = tools,
      FUN       = \(t) attr(x = t, which = "name") %||% "",
      FUN.VALUE = character(1L)
    )
    testthat::expect_true(object = "sql_query"      %in% names_vec)
    testthat::expect_true(object = "list_tables"    %in% names_vec)
    testthat::expect_true(object = "describe_table" %in% names_vec)
  }
)

# test mute_stdout_tool()
# Helper: build a minimal ToolDef-like object matching ellmer's real class.
make_mock_tool <- function(fn = \(...) "result", extra_attrs = list()) {
  class(fn) <- c("ellmer::ToolDef", "function", "S7_object")
  attr(x = fn, which = "name") <- "mock_tool"
  attr(x = fn, which = "description") <- "A mock tool for testing"
  for (nm in names(extra_attrs)) attr(x = fn, which = nm) <- extra_attrs[[nm]]
  fn
}

testthat::test_that(
  desc = "mute_stdout_tool() preserves class and regular attributes",
  code = {
    tool  <- make_mock_tool()
    muted <- entsoeapi.mcp:::mute_stdout_tool(tool_def = tool)
    testthat::expect_equal(
      object = class(muted),
      expected = c("ellmer::ToolDef", "function", "S7_object")
    )
    testthat::expect_equal(
      object = attr(x = muted, which = "name"),
      expected = "mock_tool"
    )
    testthat::expect_equal(
      object = attr(x = muted, which = "description"),
      expected = "A mock tool for testing"
    )
  }
)

testthat::test_that(
  desc = paste(
    "mute_stdout_tool() does not copy srcref / srcfile / srcfilecopy",
    "from the original"
  ),
  code = {
    # These src attrs are set on the original tool, not on the wrapper.
    # R may attach its own srcref to the wrapper's body, but the original's
    # fake string values must not appear on the muted wrapper.
    tool <- make_mock_tool(
      extra_attrs = list(
        srcref      = "fake_srcref",
        srcfile     = "fake_srcfile",
        srcfilecopy = "fake_srcfilecopy"
      )
    )
    muted <- entsoeapi.mcp:::mute_stdout_tool(tool_def = tool)
    testthat::expect_false(
      object = identical(x = attr(muted, "srcref"), y = "fake_srcref")
    )
    testthat::expect_false(
      object = identical(x = attr(muted, "srcfile"), y = "fake_srcfile")
    )
    testthat::expect_false(
      object = identical(x = attr(muted, "srcfilecopy"), y = "fake_srcfilecopy")
    )
  }
)

testthat::test_that(
  desc = "mute_stdout_tool() wrapper returns the original function's value",
  code = {
    tool <- make_mock_tool(fn = \(...) 42L)
    muted <- entsoeapi.mcp:::mute_stdout_tool(tool_def = tool)
    testthat::expect_equal(object = muted(), expected = 42L)
  }
)

testthat::test_that(
  desc = paste(
    "run() calls mcp_server with all tools and session_tools = FALSE",
    "by default"
  ),
  code = {
    captured <- NULL
    testthat::with_mocked_bindings(
      use_eic_disk_cache = \(...) invisible(NULL),
      use_server_instructions = \(...) invisible(TRUE),
      mcp_server = \(tools, session_tools) {
        captured <<- list(tools = tools, session_tools = session_tools)
        invisible(NULL)
      },
      .package = "entsoeapi.mcp",
      {
        run()
      }
    )
    testthat::expect_false(object = captured$session_tools)
    testthat::expect_length(object = captured$tools, n = 30L)
  }
)

testthat::test_that(
  desc = "run() forwards session_tools = TRUE to mcp_server",
  code = {
    captured <- NULL
    testthat::with_mocked_bindings(
      use_eic_disk_cache = \(...) invisible(NULL),
      use_server_instructions = \(...) invisible(TRUE),
      mcp_server = \(tools, session_tools) {
        captured <<- list(tools = tools, session_tools = session_tools)
        invisible(NULL)
      },
      .package = "entsoeapi.mcp",
      {
        run(session_tools = TRUE)
      }
    )
    testthat::expect_true(object = captured$session_tools)
  }
)

testthat::test_that(
  desc = "run() wraps every tool with mute_stdout_tool()",
  code = {
    captured_tools <- NULL
    testthat::with_mocked_bindings(
      use_eic_disk_cache = \(...) invisible(NULL),
      use_server_instructions = \(...) invisible(TRUE),
      mcp_server = function(tools, session_tools) {
        captured_tools <<- tools
        invisible(NULL)
      },
      .package = "entsoeapi.mcp",
      {
        run()
      }
    )
    # Every element should be an ellmer ToolDef
    # (class preserved by mute_stdout_tool).
    classes <- vapply(
      X   = captured_tools,
      FUN = \(t) inherits(x = t, what = "ellmer::ToolDef"),
      FUN.VALUE = logical(1L)
    )
    testthat::expect_true(object = all(classes))
  }
)

# test use_eic_disk_cache()
testthat::test_that(
  desc = "use_eic_disk_cache() sets entsoeapi.cache_dir to a writable dir",
  code = {
    old <- options(entsoeapi.cache_dir = NULL)
    on.exit(options(old), add = TRUE)
    dir <- file.path(tempdir(), "eic-cache-ok")
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    res <- entsoeapi.mcp:::use_eic_disk_cache(cache_dir = dir)
    testthat::expect_identical(object = res, expected = dir)
    testthat::expect_identical(
      object = getOption("entsoeapi.cache_dir"),
      expected = dir
    )
    testthat::expect_true(object = dir.exists(dir))
  }
)

testthat::test_that(
  desc = "use_eic_disk_cache() keeps the memory cache for an empty dir",
  code = {
    old <- options(entsoeapi.cache_dir = NULL)
    on.exit(options(old), add = TRUE)
    res <- entsoeapi.mcp:::use_eic_disk_cache(cache_dir = "")
    testthat::expect_null(object = res)
    testthat::expect_null(object = getOption("entsoeapi.cache_dir"))
  }
)

testthat::test_that(
  desc = "use_eic_disk_cache() keeps the memory cache for an unusable dir",
  code = {
    old <- options(entsoeapi.cache_dir = NULL)
    on.exit(options(old), add = TRUE)
    # a regular file cannot serve as a cache directory
    blocker <- tempfile()
    file.create(blocker)
    on.exit(unlink(blocker), add = TRUE)
    res <- entsoeapi.mcp:::use_eic_disk_cache(
      cache_dir = file.path(blocker, "sub")
    )
    testthat::expect_null(object = res)
    testthat::expect_null(object = getOption("entsoeapi.cache_dir"))
  }
)

testthat::test_that(
  desc = "use_eic_disk_cache() respects a pre-set entsoeapi.cache_dir",
  code = {
    old <- options(entsoeapi.cache_dir = "/user/choice")
    on.exit(options(old), add = TRUE)
    res <- entsoeapi.mcp:::use_eic_disk_cache(cache_dir = tempdir())
    testthat::expect_identical(object = res, expected = "/user/choice")
    testthat::expect_identical(
      object = getOption("entsoeapi.cache_dir"),
      expected = "/user/choice"
    )
  }
)

# test use_server_instructions()
# Restores mcptools' original capabilities() after a test patched it.
restore_capabilities <- function() {
  original <- entsoeapi.mcp:::.pkg_env$mcptools_capabilities
  if (is.null(original)) return(invisible(NULL))
  ns <- asNamespace("mcptools")
  locked <- bindingIsLocked(sym = "capabilities", env = ns)
  if (locked) unlockBinding(sym = "capabilities", env = ns)
  assign(x = "capabilities", value = original, envir = ns)
  if (locked) lockBinding(sym = "capabilities", env = ns)
  invisible(NULL)
}

testthat::test_that(
  desc = "use_server_instructions() replaces the initialize instructions",
  code = {
    on.exit(restore_capabilities(), add = TRUE)
    testthat::expect_true(
      object = entsoeapi.mcp:::use_server_instructions(instructions = "hello")
    )
    res <- mcptools:::capabilities()
    testthat::expect_identical(object = res$instructions, expected = "hello")
    testthat::expect_identical(
      object = res$serverInfo$name,
      expected = "entsoeapi.mcp"
    )
    # the capability flags still come from mcptools
    testthat::expect_true(object = "tools" %in% names(res$capabilities))
  }
)

testthat::test_that(
  desc = "use_server_instructions() does not nest wrappers on repeated calls",
  code = {
    on.exit(restore_capabilities(), add = TRUE)
    entsoeapi.mcp:::use_server_instructions(instructions = "first")
    entsoeapi.mcp:::use_server_instructions(instructions = "second")
    testthat::expect_identical(
      object = mcptools:::capabilities()$instructions,
      expected = "second"
    )
  }
)

testthat::test_that(
  desc = "use_server_instructions() adds no instructions to old protocols",
  code = {
    on.exit(restore_capabilities(), add = TRUE)
    entsoeapi.mcp:::use_server_instructions(instructions = "hello")
    res <- mcptools:::capabilities(protocol_version = "2024-11-05")
    testthat::expect_null(object = res$instructions)
  }
)

testthat::test_that(
  desc = "server_instructions mentions the core workflow tools",
  code = {
    text <- entsoeapi.mcp:::server_instructions
    for (tool_name in c("area_eic", "sql_query", "list_tables")) {
      testthat::expect_match(object = text, regexp = tool_name, fixed = TRUE)
    }
  }
)
