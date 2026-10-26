FROM rocker/r-ver:4.6.1

ARG QUARTO_VERSION="1.5.57"

ENV HOME=/tmp
ENV OPENBLAS_NUM_THREADS=1
ENV QUARTO_DATA_PATH=/tmp/.local/share/quarto
ENV TINYTEX_DIR=/opt/TinyTeX
ENV XDG_CACHE_HOME=/tmp/.cache

ENV PATH="${TINYTEX_DIR}/bin/x86_64-linux:${PATH}"

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
       libssl-dev \
       libxml2-dev \
       libcurl4-openssl-dev \
       python3 \
       perl \
       xz-utils \
    && curl -LO https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.deb \
    && gdebi --non-interactive quarto-${QUARTO_VERSION}-linux-amd64.deb \
    && rm quarto-${QUARTO_VERSION}-linux-amd64.deb \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages('tinytex')" \
    && R -e "tinytex::install_tinytex(dir = '${TINYTEX_DIR}')" \
    && ${TINYTEX_DIR}/bin/x86_64-linux/tlmgr path add

RUN tlmgr update --self \
    && tlmgr install \
       xltabular \
       threeparttable \
       threeparttablex \
       environ \
       trimspaces \
       koma-script \
       caption \
       multirow \
       wrapfig \
       colortbl \
       pdflscape \
       ltablex \
       ulem \
       makecell

WORKDIR /app
COPY . /app

RUN R -e "install.packages('remotes')" \
    && R -e "remotes::install_local('.', upgrade = 'never', dependencies = TRUE)"

RUN chmod +x /app/inst/scripts/clinprog.R \
    && ln -s /app/inst/scripts/clinprog.R /usr/local/bin/clinprog

CMD ["clinprog"]
