#here is code for the lecturers to download the packages and dependencies to an offline source
#then share with the students if they were not able to get the packages installed before the course begins
#save the package zip files to the working directory
#run this before the course and transfer the folder to a USB and share with students to install

#a list of the packages needed
packages = c(
  "tidyverse", "lme4", "bioacoustics", "warbler", "tuner", "seewave", "Soundgen",
  "TTR", "behavr", "ggetho", "zeitgebr", "sleepr", "scales", "chron", "sp", 
  "raster", "move", "ggmap", "mapproj", "lattice", "RColorBrewer", "adehabitatHR", "ks"
)

#Get package dependencies
#
#@param packs A string vector of package names
#
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

# Download the packages to the working directory.
# Package names and filenames are returned in a matrix.
setwd("C:.../offline_packages/") #adjust as necessary
pkgInfo <- download.packages(pkgs = packages, destdir = getwd(), type = "win.binary")
# Save just the package file names (basename() strips off the full paths leaving just the filename)
write.csv(file = "pkgFilenames.csv", basename(pkgInfo[, 2]), row.names = FALSE)

#then have the students run this on their computers
# Set working directory to the location of the package files
setwd("D:/my_usb/offline_packages/")

# Read the package filenames and install
pkgFilenames <- read.csv("pkgFilenames.csv", stringsAsFactors = FALSE)[, 1]
install.packages(pkgFilenames, repos = NULL, type = "win.binary")
