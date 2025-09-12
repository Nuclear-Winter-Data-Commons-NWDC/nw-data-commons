#!/bin/bash
set -e

# Update package list
sudo apt update

# Install common geospatial and data science system dependencies for R packages
sudo apt install -y \
  gdal-bin \
  libgdal-dev \
  libproj-dev \
  libgeos-dev \
  libudunits2-dev \
  libssl-dev \
  libcurl4-openssl-dev \
  libxml2-dev \
  libfontconfig1-dev \
  libharfbuzz-dev \
  libfribidi-dev \
  libtiff-dev \
  libpng-dev \
  libsqlite3-dev \
  libhdf5-dev \
  libnetcdf-dev

echo "System dependencies installed."