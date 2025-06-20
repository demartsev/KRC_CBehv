# List of packages to install
packages = c(
  "tidyverse", "lme4", "bioacoustics", "warbleR", "tuneR", "seewave", "soundgen",
  "TTR", "behavr", "ggetho", "zeitgebr", "sleepr", "scales", "chron", "sp", 
  "raster", "move", "ggmap", "mapproj", "lattice", "RColorBrewer", "adehabitatHR", "ks",
  "Rraven", "cowplot", "XML"
)

# Function to install packages if not already installed
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Install all packages
sapply(packages, install_if_missing)

#load the packages
#method 1: each individually
library(tidyverse) ; library(TTR) ; library(scales) ; library(behavr) ; library(ggetho) ; 
library(zeitgebr) ; library(lubridate) ; library(hms) ; library(sleepr) ; #...etc

#method 2: all at once
lapply(packages, require, character.only = T)

#if packages are not installed by students here's some code which will download the packages and dependencies
#then save the package zip files to a USB and share with students to install
#' Get package dependencies
#'
#' @param packs A string vector of package names
#'
#' @return A string vector with packs plus the names of any dependencies
getDependencies <- function(packs){
  dependencyNames <- unlist(
    tools::package_dependencies(packages = packs, db = available.packages(), 
                                which = c("Depends", "Imports"),
                                recursive = TRUE))
  packageNames <- union(packs, dependencyNames)
  packageNames
}
# Calculate dependencies
packages.dep <- getDependencies(packages)
packages.download = c(packages,packages.dep)

# Download the packages to the working directory.
# Package names and filenames are returned in a matrix.
setwd("C:/Users/kylet/OneDrive/Rwork/offline_packages/")
pkgInfo <- download.packages(pkgs = packages.download, destdir = getwd(), type = "win.binary")
# Save just the package file names (basename() strips off the full paths leaving just the filename)
write.csv(file = "pkgFilenames.csv", basename(pkgInfo[, 2]), row.names = FALSE)

# Set working directory to the location of the package files
setwd("D:/my_usb/packages/")

# Read the package filenames and install
pkgFilenames <- read.csv("pkgFilenames.csv", stringsAsFactors = FALSE)[, 1]
install.packages(pkgFilenames, repos = NULL, type = "win.binary")
