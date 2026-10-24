FROM rocker/r-ver:4.6.1

ARG QUARTO_VERSION="1.5.57"
ENV OPENBLAS_NUM_THREADS=1

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
       curl \
       gdebi-core \
       libfontconfig1-dev \
       libharfbuzz-dev \
       libfribidi-dev \
       libfreetype6-dev \
       libpng-dev \
       libtiff5-dev \
       libjpeg-dev \
       python3 \
    && curl -LO https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.deb \
    && gdebi --non-interactive quarto-${QUARTO_VERSION}-linux-amd64.deb \
    && rm quarto-${QUARTO_VERSION}-linux-amd64.deb \
    && rm -rf /var/lib/apt/lists/*

RUN quarto install tinytex --no-prompt

WORKDIR /app
COPY . /app

RUN R -e "install.packages('remotes')" \
    && R -e "remotes::install_local('.', upgrade = 'never', dependencies = TRUE)"

RUN chmod +x /app/inst/scripts/clinprog.R \
    && ln -s /app/inst/scripts/clinprog.R /usr/local/bin/clinprog

CMD ["clinprog"]
