# Release Checklist — entsoeapi.mcp

## 1. Prepare

- [ ] The package name is valid and available `pak::pkg_name_check("entsoeapi.mcp")`
- [ ] All planned changes merged into `develop`
- [ ] `NEWS.md` updated — new version header, all changes documented
- [ ] Version bumped in `DESCRIPTION` (follow semver: major.minor.patch)
- [ ] Version tag planned for Docker image matches `DESCRIPTION` (`vX.Y.Z`)
- [ ] `RoxygenNote` in `DESCRIPTION` matches installed roxygen2 version
- [ ] Run `lintr::lint_package()` — lint the scripts following general formatting rules

## 2. Documentation

- [ ] Run `devtools::spell_check()` — no spelling errors in docs
- [ ] `urlchecker::url_check()` — no broken URLs in docs
- [ ] Run `devtools::document()` — regenerate all `.Rd` files and `NAMESPACE`
- [ ] Run `devtools::build_readme()` — locate `README.Rmd` and build it into `README.md`
- [ ] Verify README badges (tests, coverage, tools, R, license) render correctly on GitHub
- [ ] README quick-start still works end-to-end (copy-paste test on a fresh machine)

## 3. Tests

- [ ] Run `devtools::test()` — all tests pass, no unexpected skips
- [ ] Run `covr::package_coverage()` — no significant test coverage regression (target: 100%)
- [ ] Run `semgrep ci` in CLI — to check security vulnerabilities

## 4. R CMD CHECK

- [ ] `devtools::check(cran = FALSE)` — **0 errors, 0 warnings, 0 notes**
- [ ] Confirm `entsoeapi` and `mcptools` (GitHub-only Remotes) install cleanly from a clean library

## 5. Docker Image

- [ ] `docker build -t sbudai/entsoeapi.mcp:latest -t sbudai/entsoeapi.mcp:vX.Y.Z .` — builds without errors
- [ ] Smoke-test the image responds to an MCP `initialize` request using CLI:\
  `(echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test","version":"0"}}}'; sleep 10) | docker run --rm -i -e ENTSOE_PAT=test sbudai/entsoeapi.mcp:latest`\
  — expect one JSON line with `"result"`; the `sleep` keeps stdin open, since the server exits on EOF before answering
- [ ] Smoke-test the image lists every tool in reply to an MCP `tools/list` request, i.e. each tool serializes to MCP JSON:\
  `(printf '%s\n' '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test","version":"0"}}}' '{"jsonrpc":"2.0","method":"notifications/initialized"}' '{"jsonrpc":"2.0","id":2,"method":"tools/list"}'; sleep 10) | docker run --rm -i -e ENTSOE_PAT=test sbudai/entsoeapi.mcp:latest | jq -c 'select(.id == 2) | .result.tools | map(.name) | {listed: length, unique: (unique | length)}'`\
  — expect `listed` and `unique` both equal to the number of tools in the package:\
  `docker run --rm sbudai/entsoeapi.mcp:latest Rscript -e 'cat(length(entsoeapi.mcp:::all_tools()), "\n")'`
- [ ] Exercise at least one live tool call end-to-end from Claude Desktop against the new image with a real `ENTSOE_PAT`
- [ ] Image size is reasonable (check with `docker images sbudai/entsoeapi.mcp`)
- [ ] Image runs as non-root `mcp` user (`docker run --rm sbudai/entsoeapi.mcp:latest id`)

## 6. Merge & Tag

- [ ] Push + merge / merge + push: `develop` → `main` (PR or direct)
- [ ] Create a git tag: `git tag vX.Y.Z && git push origin vX.Y.Z`
- [ ] Create a GitHub Release with the `NEWS.md` entry as release notes: `gh release create vX.Y.Z --title "vX.Y.Z" --notes "See NEWS.md for changes" --repo sbudai/entsoeapi.mcp`

## 7. Publish Docker Image

Pushing the `vX.Y.Z` tag in section 6 triggers `.github/workflows/docker.yaml`,
which builds the image, re-runs the section 5 smoke tests (MCP `initialize` and
`tools/list`, non-root `mcp` user) and pushes `sbudai/entsoeapi.mcp:vX.Y.Z` and `:latest`
to Docker Hub (a pre-release tag such as `v1.0.0-rc.1` does not move `latest`).
The tag must match the `DESCRIPTION` version, or the workflow fails.

- [ ] One-time setup: repository secrets `DOCKERHUB_USERNAME` (`sbudai`) and `DOCKERHUB_TOKEN` (a Docker Hub personal access token with Read & Write scope) exist under *Settings → Secrets and variables → Actions*
- [ ] The `docker.yaml` run for the tag succeeded: `gh run list --workflow docker.yaml --repo sbudai/entsoeapi.mcp`
- [ ] Verify both tags appear at <https://hub.docker.com/r/sbudai/entsoeapi.mcp/tags>
- [ ] `docker pull sbudai/entsoeapi.mcp:latest` from a clean host and re-run the smoke tests

To (re)publish an existing tag, e.g. one released before the workflow existed,
run the workflow by hand; set `latest=false` when the tag is not the newest
release:\
`gh workflow run docker.yaml --repo sbudai/entsoeapi.mcp -f tag=vX.Y.Z -f latest=true`

Fallback, if CI is unavailable — publish from the image built in section 5:

- [ ] `docker login` to Docker Hub with the `sbudai` account
- [ ] `docker push sbudai/entsoeapi.mcp:vX.Y.Z`
- [ ] `docker push sbudai/entsoeapi.mcp:latest`

## 8. Announce

- [ ] Close / reference any related GitHub issues in the release notes
