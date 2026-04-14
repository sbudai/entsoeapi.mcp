FROM rocker/r-ver:4.4.3

# System libraries required by R packages (httr2, xml2, curl, openssl)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Use the latest Posit Public Package Manager snapshot for pre-built Linux binaries.
# The rocker base image pins to a dated CRAN snapshot; mcptools and its deps postdate
# that freeze, so we override repos here to always resolve the current CRAN state.
ENV CRAN_REPO="https://packagemanager.posit.co/cran/__linux__/noble/latest"

# Install pak for fast, parallel dependency resolution
RUN Rscript -e "install.packages('pak', repos = 'https://r-lib.github.io/p/pak/stable/')"

# Install all R runtime dependencies
RUN Rscript -e " \
  options(repos = c(CRAN = Sys.getenv('CRAN_REPO'))); \
  pak::pkg_install(c( \
    'krose/entsoeapi', \
    'posit-dev/mcptools', \
    'ellmer', \
    'lubridate', \
    'jsonlite' \
  ))"

# Copy the package source and install it
COPY . /pkg
RUN R CMD INSTALL /pkg

# Drop root: create a dedicated non-privileged user for the runtime
RUN useradd --system --no-create-home --shell /bin/false mcp
USER mcp

# MCP uses stdin/stdout — no port needed
# Pass ENTSOE_PAT via:  docker run -e ENTSOE_PAT=<your-token> ...
CMD ["Rscript", "-e", "entsoeapi.mcp::run()"]
