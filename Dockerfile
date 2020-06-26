FROM docker.pkg.github.com/isee/isee/isee:latest

MAINTAINER kevinrue67@gmail.com
LABEL authors="kevinrue67@gmail.com" \
    description="Docker image containing the iSEEu package."

# Set the working directory to /app
WORKDIR /app

# Copy the current directory contents into the container at /app
ADD . /app

# Install iSEE and dependencies
RUN Rscript -e "BiocManager::install('iSEEu', version = 'devel')"

# Install the latest iSEE from GitHub branch master.
WORKDIR /iseeu
RUN git clone https://github.com/iSEE/iSEEu.git
RUN R CMD INSTALL iSEEu

CMD R
