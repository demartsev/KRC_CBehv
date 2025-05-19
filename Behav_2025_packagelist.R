# List of packages to install
packages <- c(
  "tidyverse", "lme4", "bioacoustics", "warbler", "tuner", "seewave", "Soundgen",
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

lapply(packages, require, character.only = T)
