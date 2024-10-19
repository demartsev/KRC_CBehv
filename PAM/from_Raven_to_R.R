##### this is an example script written for the Animal behaviour and communication field course by Vlad Demartsev ####
##### it is not meant to be an ultimate and complete process for dealing with acoustic data. This is a demonstration 
##### of the process and explanation of the steps. The quality of the results and the outputs is very much dependent on 
##### the quality of the audio material and the consistency of the call annotation process.
##### for using the script you would need few audio files 




#load packages
library(Rraven)
library(warbleR)
library(tidyverse)

#open Rraven help file to check the available functions
vignette("Rraven")

setwd("C:/Users/gabri/ownCloud - gabriella.gall@ab.mpg.de@owncloud.gwdg.de/SA_field_course/Program/Rscripts/PAM/selection_tables")

#look at the selection files in the folder
# list.files(pattern = "\\.txt$")

SelectionFile<-"C:/Users/gabri/Desktop/Pukeko/AdultCalls_SelectionTable.txt"


#read all the selection files and combine into one big table
#make sure all your selection tables include the same columns
#you need to have the name of the audio file, the path to the audio-file, 
#the start and end time of each selection
#check the documentation of this function to see which arguments you might need
#if the selection tables are not in the same folder as the audio files you will have
#to specify the directory 
rvn.dat <- imp_raven(sound.file.col = "Begin File", 
                     warbler.format =  TRUE, all.data = T)


# convert the table to class selection.table
#a format that few bio-acoustic R packages like to work with
#it links the txt files to the WAV audio files
rvn.dat.st <- selection_table(rvn.dat, path = getwd())


# create a color palette fr plotting
#this is just for making thing pretty
trc <- function(n) terrain.colors(n = n, alpha = 0.3)

#create a selection of species - one call per specie
#this is just for our example. Dependent on the analysis you
#might want to select one call per type, one cal per file etc`
#group by() function will determine what are you selecting
#sample_n() function will determine how many samples per group you will get
species_selection <- rvn.dat.st %>% group_by(Annotation) %>% sample_n(1)


# plot catalog of calls
#this will save an image file with spectrograms according to your selections
#flim argument will determine the lowest and highest frequency that will be plotted
#adjust it acordig to the calls of your species
#the nrow (number rows) ncol (number column) will determine how many panels 
#you will have in the plot. You can adjust it to fit all the examples 
catalog(X = species_selection, flim = c(0.5, 10), nrow = 2, ncol = 2, 
        same.time.scale = F,  spec.mar = 1, box = FALSE,
        #here you specify the spectrogram settings. WIndow size (wl), window type (wn), window overlap (ovlp)
        ovlp = 90, parallel = 1, mar = 0.01, wl = 512, wn = "hanning", 
        pal = reverse.heat.colors, width = 20,
        labels = c("Annotation", "selec"), legend = 1, 
        tag.pal = list(trc))


##now we have a visual representation of our data
##next step is to get some acoustic measurements of the different calls


#here we select only one row (call) from out selection table
#but for processing all the calls in our data set this cn be wrapped into a loop

#for now we are looking at row #12 only
r <- 11

#reading the sound segment into R by specifying the path to the audio file (X), start (from) and end time (to) of the call
# we also add some buffer time (0.1s) before and after the call to avoid having very short audion segments 
# the duration of the buffer time can be adjusted acording to your data
wv1 <- read_wave(X = rvn.dat.st$`Begin Path`[r] , from = rvn.dat.st$start[r]-0.1, to = rvn.dat.st$end[r]+0.1)

#play the audio - this command might be different for mac computers
#or might not work at all if the default WAV player on your computer 
#was changed. If it is not working or throwing errors just comment it out
#it is not effecting any of the subsequent steps
play(wv1)

#plot a spectrogram of the current sound segment
#adjust spectrogram parameters as needed
spectro(wv1, wl = 256, grid = FALSE, scale = FALSE, ovlp = 90)


#select all the calls of only one species
#here we select the calls of Spotted owlet
owlet_calls <- rvn.dat.st[which(rvn.dat.st$Annotation == "Owlet") ,]


#now we will go through some quality control steps to make sure that our data is 
#rasonably consistent and of a good enough quality

#define the parameters of paneled plot
par(mfrow = c(3, 2), mar = rep(0, 4))

#a loop that will read each of the calls one by one, draw a spectrogram and add it as a new panel
for(i in 1:nrow(owlet_calls)){
  #read the sound segment into R
  wv <- read_wave(X = owlet_calls[i], from = owlet_calls$start[i]-0.1 , to = owlet_calls$end[i]+0.1)
  
  #plot a spectrogram
  spectro(wv, wl = 256, grid = FALSE, scale = FALSE, axisX = T,
          axisY = T, ovlp = 90)
}


#first thing we do is check the signal to noise ratio (SNR). Are our calls actually louder than the 
#background noise. If the signal we are interested in is the same level of loudness as the background we cant do any 
#reliable measurements as everything will be strongly masked by the environmental noise

#we are using the sig2noise() function from the warbleR package. It has few options for calculating SNR
#specified in the type argument. type=1 is the simplest one. Dividing the mean amplitude of the signal
# by the mean amplitude of the environmental noise. The environmental noise is samples from equal lengths time segments 
# before and after the signal (eq.dur = T). See documentation for further settings.
# the higher SNR value the stronger our signal in comparison to the background. Any calls with SNR < 1.3-1.5 should be 
# checked and potentially removed from further acoustic analysis.They can still be used for determining syntactic structures

SNR <- sig2noise(owlet_calls,  parallel = 1, path = NULL, pb = TRUE, type = 1, eq.dur = T,
          in.dB = TRUE, before = FALSE, lim.dB = TRUE, bp = NULL, wl = 10)

#visualize the distribution of SNR values
ggplot(data = SNR, aes(x = Annotation, y = SNR)) + geom_boxplot()

#check cross correlation between the different calls (similarity)
#we will expect a reasonably high correlation score between calls of the same type 
#and a lower correlation score between different type calls. If this is not the case and within call-type
#correlation is very low or more or less the same as between-call type correlation
#something might be wrong with our data. 

cross_corr <- cross_correlation(owlet_calls, wl = 512, bp = "pairwise.freq.range", ovlp = 70,
                  dens = NULL, wn = 'hanning', cor.method = "pearson", parallel = 1,
                  path = NULL, pb = TRUE, na.rm = FALSE, cor.mat = NULL, output = "cor.mat",
                  templates = NULL, surveys = NULL, compare.matrix = NULL, type = "fourier",
                  nbands = 40, method = 1)


#check how well the dominant frequency can be traced. This function will try and trace the strongest (loudest)
#frequency  of your call. The lenght.out argument determines how many measurements will be taken from the beginning
#to the end. In our cse we will make 10 measurements. 
dom_trace <- freq_ts(owlet_calls, type = "dominant", wl = 512, length.out = 10, wn = "hanning",
        ovlp = 70, bp = c(0.5, 10), threshold = 15, img = TRUE, parallel = 1, path = NULL,
        img.suffix = "frequency.ts", pb = TRUE, clip.edges = T, leglab = "frequency.ts",
        track.harm = FALSE, raw.contour = FALSE, adjust.wl = TRUE,
        ff.method = "seewave", entropy.range = c(2, 20))

#now we will plot the average contour of the dominant frequency
#it should resemble the overall shape of our call
#IMPORTANT: if the signal is not tonal (whistle like) this step might not make a lot
#of sense. Noisy, growly calls show highly variable dominant frequency

#for easier plotting we will convert the data from wide to long format
#the column names ffreq_1 - ffreq-10 will be used as values in a new column called “ffreq” 
#the values from these original columns will be placed into one new column called “KHz.”
dom_trace_long <- dom_trace %>% 
  pivot_longer(cols=c(3:12),
               names_to='ffreq',
               values_to='KHz')

#we will specify the correct order of the frequency bins and make them a factor
#so the X axis of our plot will correspond to the actual progression of the call 
dom_trace_long$ffreq <- factor(dom_trace_long$ffreq, 
                                  levels = c("ffreq-1", "ffreq-2", "ffreq-3",
                                             "ffreq-4", "ffreq-5", "ffreq-6",
                                             "ffreq-7", "ffreq-8", "ffreq-9",
                                             "ffreq-10"))
#now we make a simple scatter plot to visualize the tracing of the dominant frequency across all of our data
#we want to see similar shape to our call
ggplot(dom_trace_long, aes(x = ffreq, y = KHz, color = as.character(selec))) + geom_point(cex = 4)

#################################################################################################################
#after we have checked our data and made sure no garbage is going into the analysis we can do some measurements 
#what exactly we are measuring is very much dependent on the question, species and structure of the call
#for a general description of species  vocal repertoire  we might want to include measurements on the duration, Fundamental 
# min and max frequencies. Perhaps a measurement of Entropy (tonality) of the signal. You also might want to include the 
# relative occurrence of different types of calls (check this paper for a nice example - https://doi.org/10.1007/s10764-022-00287-x)

#run full spectral analysis for owlet calls
#note that not all measurements will work for every call. sometimes the function will fail in particular measurments
#due to the properties of the signal or the quality of the audio. 
spec_analys <- spectro_analysis(owlet_calls, bp = "frange", wl = 512, wl.freq = 512, threshold = 15,
                 parallel = 1, fast = TRUE, path = NULL, pb = TRUE, ovlp = 70, step = 6,
                 wn = "hanning", fsmooth = 0.1, harmonicity = T, nharmonics = 3)

# another option for acoustic analysis using soundgen package. It is not as user friendly 
# but has a few additional useful function like automatic piloting and measurement tracing 
# on the spectrogram. see more info on the package webpage - http://cogsci.se/soundgen.html

library(soundgen)

# loop that will read each of the calls one by one, and run the analyze() function on each one of the loaded calls
# you will get a spectrogram with the trace of the frequency and an object (a1) with 170!!! measurements. 

#create n empty table that will be populated with your measurements
all_measurements <- data.frame()

for(i in 1:nrow(owlet_calls)){
  #read the sound segment into R
  wv <- read_wave(X = owlet_calls[i], from = owlet_calls$start[i]-0.1 , to = owlet_calls$end[i]+0.1)
  #do the measurments and save the output as a1 object
  a1 = analyze(wv, samplingRate = wv@samp.rate, plot = TRUE, ylim = c(0, 10))
  #add the output to one big table
  all_measurements <- rbind(all_measurements, a1$summary)
}

#### FINAL NOTES: Expect adventures!!! Read the documentations for the packages that you are using, especially for SOUNDGEN.
#### the analyze() function is a monster and will require extensive tweaking to work properly with your data!!!!!
#### Do not skip on the Quality Control steps. For acoustic measurements; more is not necessarily better. Poor quality, nosy recordings
#### will produce almost random numbers when attempting to measure anything. When possible, collect more data and choose only 
#### the best exemplars. 

#### Deciding on the program to use: Raven will produce good results with the built in functions. Definitely use it for
#### labeling and annotating calls. Keep an eye on the updates as it is constantly improving. BUT...it is not nearly as
#### flexible as working in R. For working with a lot of data (hundreds and thousands of calls) make an effort and
#### set up your QC and analysis scripts. You only have to do it once and than the pipeline is almost fully automatic 
#### for processing new data. 