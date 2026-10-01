
FROM ghcr.io/r-hub/r-minimal/r-minimal:latest

RUN installr -d -t "linux-headers curl-dev" -a git  \
      r-hub/biocatgh gitcreds &&                    \
    rm -rf /usr/local/lib/R/library/*/help &&       \
    rm -rf /usr/local/lib/R/library/*/doc
