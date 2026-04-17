# Release Checklist — entsoeapi.mcp

## 1. Prepare

-   [ ] The package name is valid and available `pak::pkg_name_check("entsoeapi.mcp")`
-   [ ] All planned changes merged into `develop`
-   [ ] `NEWS.md` updated — new version header, all changes documented
-   [ ] Version bumped in `DESCRIPTION` (follow semver: major.minor.patch)
-   [ ] Version tag planned for Docker image matches `DESCRIPTION` (`vX.Y.Z`)
-   [ ] `RoxygenNote` in `DESCRIPTION` matches installed roxygen2 version
-   [ ] Run `lintr::lint_package()` — lint the scripts following general formatting rules

## 2. Documentation

-   [ ] Run `devtools::spell_check()` — no spelling errors in docs
-   [ ] `urlchecker::url_check()` — no broken URLs in docs
-   [ ] Run `devtools::document()` — regenerate all `.Rd` files and `NAMESPACE`
-   [ ] Run `devtools::build_readme()` — locate `README.Rmd` and build it into `README.md`
-   [ ] Verify README badges (tests, coverage, tools, R, license) render correctly on GitHub
-   [ ] README quick-start still works end-to-end (copy-paste test on a fresh machine)

## 3. Tests

-   [ ] Run `devtools::test()` — all tests pass, no unexpected skips
-   [ ] Run `covr::package_coverage()` — no significant test coverage regression (target: 100%)
-   [ ] Run `semgrep ci` in CLI — to check security vulnerabilities

## 4. R CMD CHECK

-   [ ] `devtools::check(cran = FALSE)` — **0 errors, 0 warnings, 0 notes**
-   [ ] Confirm `entsoeapi` and `mcptools` (GitHub-only Remotes) install cleanly from a clean library

## 5. Docker Image

-   [ ] `docker build -t sbudai/entsoeapi.mcp:vX.Y.Z -t sbudai/entsoeapi.mcp:vX.Y.Z .` — builds without errors
-   [ ] Smoke-test the image responds to an MCP `initialize` request using CLI:\
    `echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test","version":"0"}}}' | docker run --rm -i -e ENTSOE_PAT=test sbudai/entsoeapi.mcp:vX.Y.Z`
-   [ ] Exercise at least one live tool call end-to-end from Claude Desktop against the new image with a real `ENTSOE_PAT`
-   [ ] Image size is reasonable (check with `docker images sbudai/entsoeapi.mcp`)
-   [ ] Image runs as non-root `mcp` user (`docker run --rm sbudai/entsoeapi.mcp:vX.Y.Z id`)

## 6. Merge & Tag

-   [ ] Push + merge / merge + push: `develop` → `main` (PR or direct)
-   [ ] Create a git tag: `git tag vX.Y.Z && git push origin vX.Y.Z`
-   [ ] Create a GitHub Release with the `NEWS.md` entry as release notes: `gh release create vX.Y.Z --title "vX.Y.Z" --notes "See NEWS.md for changes" --repo sbudai/entsoeapi.mcp`

## 7. Publish Docker Image

-   [ ] `docker login` to Docker Hub with the `sbudai` account
-   [ ] `docker push sbudai/entsoeapi.mcp:vX.Y.Z`
-   [ ] `docker push sbudai/entsoeapi.mcp:latest`
-   [ ] Verify both tags appear at <https://hub.docker.com/r/sbudai/entsoeapi.mcp/tags>
-   [ ] `docker pull sbudai/entsoeapi.mcp:latest` from a clean host and re-run the smoke test

## 8. Announce

-   [ ] Close / reference any related GitHub issues in the release notes
