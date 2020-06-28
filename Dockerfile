FROM iseedevelopers/isee:latest

MAINTAINER kevinrue67@gmail.com
LABEL authors="kevinrue67@gmail.com" \
    description="Docker image containing the iSEEu package."

WORKDIR /home/rstudio/iseeu

COPY --chown=rstudio:rstudio . /home/rstudio/iseeu

ENV R_REMOTES_NO_ERRORS_FROM_WARNINGS=true

RUN Rscript -e "devtools::install('.', dependencies=TRUE, repos = BiocManager::repositories(), build_vignettes = TRUE)"
