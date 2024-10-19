
library(devtools)
devtools::install_github('nationalparkservice/NSNSDAcoustics')
remotes::install_github("nationalparkservice/NSNSDAcoustics")

# Must set environment BEFORE calling in the reticulate package
Sys.setenv(RETICULATE_PYTHON = "C:/Users/vdemartsev/anaconda3/envs/pybirdanalyze/python.exe")
library(reticulate)

# Set your conda environment
use_condaenv(condaenv = "pybirdanalyze", required = TRUE)

# Create an audio directory for this example
dir.create('example-audio-directory')

# Create a results directory for this example
dir.create('example-results-directory')


library(NSNSDAcoustics)
# Run all audio data in a directory through BirdNET
birdnet_analyzer(audio.directory = 'C:/Users/vdemartsev/Documents/example-audio-directory',
                 results.directory = 'C:/Users/vdemartsev/Documents/example-results-directory',
                 birdnet.directory = 'C:/Users/vdemartsev/ownCloud - vdemartsev@ab.mpg.de@owncloud.gwdg.de/rogram/Rscripts/PAM/BirdNET-Analyzer-main',
                 use.week = TRUE,
                 lat = +21.83197,
                 lon = -26.97928)