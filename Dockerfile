# syntax=docker/dockerfile:1

# Two stages keep the image small:
#   builder - rocker/r-ver (compilers, headers, pak) installs the R packages
#             as pre-built binaries and trims the R installation down;
#   runtime - plain Ubuntu with the R installation copied over and only the
#             shared libraries that the R binaries actually link against.
# Both stages must use the same Ubuntu release (noble), because the Posit
# Public Package Manager binaries are built for it.

ARG R_VERSION=4.6.1

# ------------------------------------------------------------------------------
FROM rocker/r-ver:${R_VERSION} AS builder

# The rocker base image pins to a dated CRAN snapshot; mcptools and its deps
# postdate that freeze, so always resolve the current CRAN state instead.
ENV CRAN_REPO="https://packagemanager.posit.co/cran/__linux__/noble/latest"
# The binaries need only runtime libraries, which the runtime stage installs
# itself. pak's system requirement handling would add compilers, -dev headers,
# a JDK, LLVM, etc. (~1 GB) to this stage for nothing.
ENV PKG_SYSREQS=false

# pak lives outside the R library, so it never reaches the runtime stage
RUN mkdir /opt/pak && Rscript -e "install.packages('pak', lib = '/opt/pak', \
      repos = 'https://r-lib.github.io/p/pak/stable/')"

# Leave only base R in the site library (rocker ships littler and docopt)
RUN rm -rf /usr/local/lib/R/site-library/*

# Install the dependencies declared in DESCRIPTION (Imports + Remotes) first,
# so this slow layer is reused as long as DESCRIPTION is unchanged
COPY DESCRIPTION /pkg/DESCRIPTION
RUN Rscript -e " \
      .libPaths(c(.libPaths(), '/opt/pak')); \
      options(repos = c(CRAN = Sys.getenv('CRAN_REPO'))); \
      pak::local_install_deps('/pkg', dependencies = NA)"

# Install the package itself; it is test-loaded in the runtime stage, where
# the shared libraries are present
COPY . /pkg
RUN R CMD INSTALL --no-docs --no-html --no-multiarch --no-test-load /pkg

# Trim the R installation:
#   - drop the recommended packages (Matrix, mgcv, survival, ...) that are not
#     dependencies of entsoeapi.mcp;
#   - drop documentation, translations, headers and tests of every package;
#   - strip the debug symbols from the shared objects.
RUN <<'EOF'
set -eu
Rscript -e '
  ip <- installed.packages()
  deps <- tools::package_dependencies(
    packages = "entsoeapi.mcp", db = ip,
    which = c("Depends", "Imports"), recursive = TRUE
  )[[1L]]
  keep <- c("entsoeapi.mcp", deps, rownames(ip)[ip[, "Priority"] %in% "base"])
  drop <- setdiff(rownames(ip), keep)
  message("Removing: ", paste(drop, collapse = ", "))
  unlink(file.path(ip[drop, "LibPath"], drop), recursive = TRUE)
'
cd /usr/local/lib/R
# A headless server never plots: tcltk links Tcl/Tk, and the lazily loaded
# X11, data editor and cairo modules link X11/cairo/pango/fontconfig
rm -rf doc include library/translations library/tcltk share/texmf \
       modules/R_X11.so modules/R_de.so library/grDevices/libs/cairo.so \
       site-library/_cache
for lib in library site-library; do
  for pkg in "$lib"/*/; do
    rm -rf "$pkg"doc "$pkg"html "$pkg"help "$pkg"po "$pkg"tests \
           "$pkg"include "$pkg"demo "$pkg"NEWS* "$pkg"README*
  done
done
find . -name '*.so' -type f -exec strip --strip-unneeded {} +
EOF

# Work out the Ubuntu packages providing the shared libraries that R and the
# package binaries link against. The reference BLAS/LAPACK replace OpenBLAS:
# nothing here does heavy linear algebra, and OpenBLAS alone is ~100 MB.
RUN <<'EOF'
set -eu
# libR.so is found via R_HOME/lib, which R itself adds to LD_LIBRARY_PATH
export LD_LIBRARY_PATH=/usr/local/lib/R/lib
find /usr/local/lib/R -type f \( -name '*.so' -o -path '*/bin/exec/R' \) \
  -exec ldd {} + 2>/dev/null > /tmp/ldd.txt
if grep 'not found' /tmp/ldd.txt; then
  echo "Unresolved shared libraries, see above" >&2
  exit 1
fi
awk '/=> \// { print $3 }' /tmp/ldd.txt | sort -u \
  | while read -r lib; do
      case "$lib" in /usr/local/lib/R/*) continue ;; esac
      real=$(readlink -f "$lib")
      dpkg -S "$lib" 2>/dev/null || dpkg -S "$real" 2>/dev/null \
        || dpkg -S "/usr$real" 2>/dev/null || dpkg -S "${real#/usr}"
    done \
  | sed -E 's/^diversion by ([^ ]+) .*/\1/' | cut -d: -f1 | sort -u \
  | sed -E 's/^libopenblas.*/libblas3\nliblapack3/' | sort -u \
  > /runtime-deps.txt
echo "Runtime system dependencies:" && cat /runtime-deps.txt
EOF

# ------------------------------------------------------------------------------
FROM ubuntu:noble

# R_UNZIPCMD: R's built-in unzip, spares installing /usr/bin/unzip.
# ENTSOEAPI_CACHE_DIR: persistent EIC table cache; mount a volume on
# /home/mcp/.cache to keep it across containers.
ENV LANG=C.UTF-8 \
    TZ=Etc/UTC \
    R_UNZIPCMD=internal \
    ENTSOEAPI_CACHE_DIR=/home/mcp/.cache/entsoeapi

COPY --from=builder /runtime-deps.txt /tmp/runtime-deps.txt
RUN apt-get update \
 && DEBIAN_FRONTEND=noninteractive xargs -a /tmp/runtime-deps.txt \
      apt-get install -y --no-install-recommends ca-certificates tzdata \
 && rm -rf /var/lib/apt/lists/* /var/cache/apt/* /var/log/* /tmp/*

COPY --from=builder /usr/local/lib/R /usr/local/lib/R
RUN ln -s /usr/local/lib/R/bin/R /usr/local/bin/R \
 && ln -s /usr/local/lib/R/bin/Rscript /usr/local/bin/Rscript

# Smoke test: every namespace (incl. compiled code) loads, and every tool
# defined in the package is served, under a unique name, as a ToolDef that
# survives the stdout-muting wrapper run() applies
RUN Rscript -e ' \
      library(entsoeapi.mcp); \
      con <- DBI::dbConnect(duckdb::duckdb()); DBI::dbDisconnect(con); \
      ns <- asNamespace("entsoeapi.mcp"); \
      is_tool <- function(x) S7::S7_inherits(x, ellmer::ToolDef); \
      defined <- Filter(is_tool, mget(ls(ns), envir = ns)); \
      served <- lapply(ns$all_tools(), ns$mute_stdout_tool); \
      served_names <- vapply(served, function(x) x@name, ""); \
      stopifnot( \
        length(served) > 0L, \
        all(vapply(served, is_tool, NA)), \
        !anyDuplicated(served_names), \
        setequal(served_names, vapply(defined, function(x) x@name, "")) \
      )'

# Drop root: a dedicated non-privileged user, with a home for the EIC cache
RUN useradd --system --create-home --shell /usr/sbin/nologin mcp \
 && mkdir -p "$ENTSOEAPI_CACHE_DIR" \
 && chown -R mcp:mcp /home/mcp/.cache
USER mcp

# MCP uses stdin/stdout — no port needed
# Pass ENTSOE_PAT via:  docker run -e ENTSOE_PAT=<your-token> ...
CMD ["Rscript", "-e", "entsoeapi.mcp::run()"]
