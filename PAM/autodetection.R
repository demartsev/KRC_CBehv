setwd("C:/Users/vdemartsev/ownCloud - vdemartsev@ab.mpg.de@owncloud.gwdg.de/SA_field_course/Program/Rscripts/PAM/P04_00024969/P04_20230722_test_2")

library(ohun)
library(Rraven)
library(warbleR)

# run auto detection on audio recordings


#list audio files save in the working directory
audio <- list.files(pattern = ".wav", recursive = T, full.names = T)

#loop through the files and atempt to detect caling events

for (file in audio) {

  #this line was added as an example and the detection will only run on file #10
  #comment it out for atempting detection in all files
  file <- audio[10]

  #this is the function that performs the call detection
  #see documentation for additional settings
  detection <-
  energy_detector(
    files = file,
    bp = c(1, 8), #range of frequencies for detection
    thinning = 0.75, #reduction of number of samples
    threshold = 30,  #this is the amplitude threshold in percentage
    min.duration = 70, #minimum duration of a call
    smooth = 150,
    path = getwd()
  )


#saving a raven selection table
exp_raven(detection,
            path = NULL, file.name = file, khz.to.hz = TRUE, 
         sound.file.path = NULL, single.file = TRUE, parallel = 1, pb = TRUE)

}

#read the whole audio file into R
audio<- read_wave(file)

###  # plot spectrograme and detected calls
 label_spectro(wave =audio, f = 44100, 
               envelope = TRUE, 
               detection = detection, 
               fastdisp=TRUE, threshold = 50)