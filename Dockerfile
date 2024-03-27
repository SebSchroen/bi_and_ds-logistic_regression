# Use shiny image
FROM rocker/shiny:4.3.0

# Update system libraries
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libcurl4-openssl-dev \
    libssl-dev

# Set shiny working directory
RUN rm -rf /srv/shiny-server/*
WORKDIR /srv/shiny-server/

# Copy Shiny files
COPY /app.R ./app.R
COPY /R ./R
COPY /rawdata ./rawdata

# Copy renv.lock file
COPY /renv.lock ./renv.lock

# Download renv and restore library
ENV RENV_VERSION 1.0.5
RUN Rscript -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN Rscript -e "install.packages('tidyverse', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN Rscript -e "install.packages('tidymodels', repos = c(CRAN = 'https://cloud.r-project.org'))"
#RUN Rscript -e "install.packages('plotly', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN Rscript -e "remotes::install_github('rstudio/renv@v${RENV_VERSION}')"
ENV RENV_PATHS_LIBRARY renv/library
RUN Rscript -e 'renv::restore()'

# Expose port
EXPOSE 3838