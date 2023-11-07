# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

LABEL maintainer="Alan Haynes <alan.haynes@unibe.ch>"

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* && \
    rm -rf /tmp/*

# copy necessary files
# start in at level above package
## app folder
COPY --chown -R shiny:shiny presize/inst/shinyApp ./app
COPY --chown -R shiny:shiny presize ./presize

# install deps
RUN Rscript -e 'install.packages("remotes");remotes::install_deps("presize", dependencies = TRUE);remotes::install_local("presize")'

USER shiny

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
