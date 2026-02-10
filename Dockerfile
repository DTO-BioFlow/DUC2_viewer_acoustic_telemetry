# Base: R in a Debian Linux environment
FROM rocker/shiny:4.5

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y \
    make \
    pandoc \
    cmake \
    gdal-bin \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    libsqlite3-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libv8-dev \
    libpng-dev \
    libjpeg-dev \
    libabsl-dev \
    zlib1g-dev \
    librdf0-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /usr/src/app

# Copy renv.lock and renv folder first for caching
COPY renv.lock renv.lock
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json
COPY .Rprofile .Rprofile
# Install renv and restore packages

RUN R -e 'install.packages("renv", repos="https://cloud.r-project.org")' \
    && R -e 'renv::restore()'

# Copy all R files
COPY www/ ./www/
COPY data/ ./data/
COPY R/ ./R/
COPY *.R .
# Expose Shiny port
EXPOSE 3838

# Default command: run the Shiny app

CMD ["R", "-e", "shiny::runApp('/usr/src/app', host='0.0.0.0', port=3838)"]


