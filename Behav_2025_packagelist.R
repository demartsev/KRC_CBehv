# List of packages to install
packages <- c(
  "tidyverse", "lme4", "bioacoustics", "warbleR", "tuneR", "seewave", "soundgen",
  "TTR", "behavr", "ggetho", "zeitgebr", "sleepr", "scales", "chron", "sp", 
  "raster", "move", "ggmap", "mapproj", "lattice", "RColorBrewer", "adehabitatHR", "ks"
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
library(tidyverse)
library(TTR)
library(scales)
library(behavr)
library(ggetho)
library(zeitgebr)
library(lubridate)
library(hms)
library(sleepr)
library(ks)
library(lme4)
library(bioacoustics)
library(warbleR)
library(tuneR)
library(seewave)
library(soundgen)
library(chron)
library(sp)
library(raster)
library(move) 
library(ggmap)
library(mapproj)
library(lattice)
library(RColorBrewer)
library(adehabitatHR)

#loading each individually for the first time helps to determine if any of them failed to install properly
#method 2: all at once
library(tidyverse) ; library(TTR) ; library(scales) ; library(behavr) ; library(ggetho) ; 
library(zeitgebr) ; library(lubridate) ; library(hms) ; library(sleepr) ; library(ks) ;
library(lme4) ; library(bioacoustics) ; library(warbleR) ; library(tuneR) ; library(seewave) ;
library(soundgen) ; library(chron) ; library(sp) ; library(raster) ; library(move) ; 
library(ggmap) ; library(mapproj) ; library(lattice) ; library(RColorBrewer) ; library(adehabitatHR) ;

#this also will load them all at once
#then will provide an output saying True or False if they loaded or did not load
lapply(packages, require, character.only = T)

#!STOP!#

#only run the next part if the packages were not installed to your computer before the course
#install packages from a USB drive
# Set working directory to the location of the package files
setwd("D:/my_usb/offline_packages/") #adjust as necessary

# Read the package filenames and install
pkgFilenames <- read.csv("pkgFilenames.csv", stringsAsFactors = FALSE)[, 1]
install.packages(pkgFilenames, repos = NULL, type = "win.binary")
