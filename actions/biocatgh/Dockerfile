
FROM ghcr.io/r-hub/r-minimal/r-minimal:latest

# To work around a certificate issue with the file, I exported their cert
# from Firefox and add it manually to the container. The R command tests
# the cert.
COPY bioconductor-org.pem /usr/local/share/ca-certificates/bioconductor-org.pem
RUN update-ca-certificates
RUN R -q -e 'download.file("https://code.bioconductor.org", tempfile())'

RUN installr -d -t "linux-headers curl-dev" -a git  \
      r-hub/biocatgh gitcreds &&                    \
    rm -rf /usr/local/lib/R/library/*/help &&       \
    rm -rf /usr/local/lib/R/library/*/doc
