testthat::test_that(
  desc = "all_tools() returns a list of 26 tool definitions",
  code = {
    tools <- entsoeapi.mcp:::all_tools()
    testthat::expect_type(object = tools, type = "list")
    testthat::expect_length(object = tools, n = 26L)
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
    muted <- mute_stdout_tool(tool_def = tool)
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
    muted <- mute_stdout_tool(tool_def = tool)
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
    muted <- mute_stdout_tool(tool_def = tool)
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
    testthat::expect_length(object = captured$tools, n = 26L)
  }
)

testthat::test_that(
  desc = "run() forwards session_tools = TRUE to mcp_server",
  code = {
    captured <- NULL
    testthat::with_mocked_bindings(
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
